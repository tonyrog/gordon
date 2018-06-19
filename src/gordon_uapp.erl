%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Read uapp file
%%%    An uapp file may contain multiple products of
%%%    multiple versions with possible selected products
%%%    also support multiple banks or multiple segments (select?)
%%% @end
%%% Created : 15 Jun 2018 by Tony Rogvall <tony@rogvall.se>

-module(gordon_uapp).
-export([is_uapp/1, load/1]).
-export([decode_app_vsn/1]).

-define(UAPP_MAGIC,   <<"UAPP">>).
-define(UAPP_PRODUCT, <<"UPRD">>).
-define(UAPP_VERSION, <<"UVSN">>).
-define(UAPP_SERIAL,  <<"USNR">>).
-define(UAPP_BIN,     <<"UBIN">>).
-define(UAPP_ADDR,    <<"UADR">>).

-define(PRODUCT_ANY,          16#0000).
-define(PRODUCT_PDS_860,      16#0001). 
-define(PRODUCT_POWERBOX,     16#0001).   %% alias
-define(PRODUCT_PDS,          16#0001).   %% alias
-define(PRODUCT_PDS_1A,       16#0101).   %% output-1 has 1A mod
-define(PRODUCT_PDS_DMX,      16#0201).   %% RS485 serial output
-define(PRODUCT_PDS_DMX_1A,   16#0301).   %% 1A + RS485 serial
-define(PRODUCT_PDC_80,       16#8002).   %% 8 on/off,  - encoder
-define(PRODUCT_PDC_08,       16#0802).   %% - on/off,  8 encoder
-define(PRODUCT_PDC_44,       16#4402).   %% 4 on/off,  4 encoder
-define(PRODUCT_PDC_71,       16#7102).   %% 7 on/off,  1 encoder
-define(PRODUCT_PDC_8F,       16#8F02).   %% 8 sealed on/off
-define(PRODUCT_PDC,          16#0002).   %% alias for all kinds of panels
-define(PRODUCT_PANEL,        16#0002).   %% alias for all kinds of panels
-define(PRODUCT_PDD_25,       16#2503).
-define(PRODUCT_PDD,          16#0003).   %% alias for all pdd units
-define(PRODUCT_PDI_24,       16#0004).   %% 24 inputs
-define(PRODUCT_PDI,          16#0004).   %% alias
-define(PRODUCT_PDI_12,       16#0104).   %% 12 inputs, 8 outputs, 4 analog in
-define(PRODUCT_INPUT,        16#0004).
-define(PRODUCT_PDS_80,       16#0005).
-define(PRODUCT_FUSE,         16#0005).   %% alias
-define(PRODUCT_ADC,          16#0006).   %% a/d 
-define(PRODUCT_REMOTE,       16#0007).   %% remote control
-define(PRODUCT_RFID,         16#0008).   %% RFID/CAN product 
-define(PRODUCT_PDB,          16#0009).   %% bridgeZone
-define(PRODUCT_PDB_DMX,      16#0209).   %% bridgeZone+DMX
-define(PRODUCT_PDB_MARINCO,  16#0409).   %% bridgeZone+Marinco
-define(PRODUCT_PDB_KELLY,    16#0809).   %% bridgeZone+Kelly
-define(PRODUCT_VPULSE,       16#000A).   %% vario-pulse
-define(PRODUCT_DEV,          16#00FF).   %% devlopment board
-define(PRODUCT_DEV_MCB2100,  16#21FF).


%% return true|false|{error,Reason}
is_uapp(File) ->
    case file:open(File,[read,raw,binary]) of
	{ok,Fd} ->
	    try file:read(Fd, 4) of
		{ok, ?UAPP_MAGIC} -> true;
		{ok, _} -> false;
		_ -> false
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

load(File) ->
    case file:open(File,[read,raw,binary]) of
	{ok,Fd} ->
	    case file:read(Fd, 4) of
		{ok, ?UAPP_MAGIC} ->
		    try read_file_(Fd, []) of
			UApp ->
			    {ok, fat_decode(UApp)}
		    catch
			error:_ -> 
			    {error,corrupt_file}
		    after
			file:close(Fd)
		    end;
		{ok, _}  ->
		    file:close(Fd),
		    {error,bad_magic};
		Error ->
		    file:close(Fd),
		    Error
	    end;
	Error ->
	    Error
    end.

%% read the tagged file data
read_file_(Fd, Es) ->
    case file:read(Fd, 4) of
	eof -> Es;
	{ok,Tag} ->
	    {ok,<<Size:32>>} = file:read(Fd, 4),
	    {ok,Data} = file:read(Fd, Size),
	    read_file_(Fd, [{Tag,Data}|Es])
    end.

fat_decode(UApp) ->
    fat(decode(UApp,[]), []).

fat([{product,P}|Es],Ps) ->
    product(Es,P,[],Ps);
fat([], Acc) ->
    Acc.

product([{version,V},{serial,S}|Es],P,Bs,Ps) ->
    {Ds,Es1} = data(Es,[]),
    product(Es1,P,[{banks,[{version,V},{serial,S}],Ds}|Bs],Ps);
product([{version,V}|Es],P,Bs,Ps) ->
    {Ds,Es1} = data(Es,[]),
    product(Es1,P,[{banks,[{version,V}],Ds}|Bs],Ps);
product([{product,P1}|Es],P,Bs,Ps) ->
    product(Es,P1,[],[{product,P,Bs}|Ps]);
product([],P,Bs,Ps) ->
    [{uapp,P,Bs}|Ps].
    
data([{addr,A},{data,D}|Es],Ds) ->
    data(Es,[{A,D}|Ds]);
data(Es,Ds) ->
    {Ds,Es}.

decode([{?UAPP_PRODUCT,Data}|Es], Acc) ->
    <<Variant:8,Product:8,Major:8,Minor:8>> = Data,
    P = case Product of
	    ?PRODUCT_PDS -> powerZone;
	    ?PRODUCT_PDC -> controlZone;
	    ?PRODUCT_PDD -> displayZone;
	    ?PRODUCT_PDI -> ioZone;
	    ?PRODUCT_PDB -> bridgeZone;
	    _ -> unknown
	end,
    V = case (Variant bsl 8) bor Product of 
	    ?PRODUCT_PDS_1A     -> [scale1a];
	    ?PRODUCT_PDS_DMX    -> [dmx];
	    ?PRODUCT_PDS_DMX_1A -> [dmx,scale1a];
	    ?PRODUCT_PDC_80     -> [{onoff,8}];
	    ?PRODUCT_PDC_08     -> [{encoder,8}];
	    ?PRODUCT_PDC_44     -> [{onoff,4},{encoder,4}];
	    ?PRODUCT_PDC_71     -> [{onoff,7},{encoder,1}];
	    ?PRODUCT_PDC_8F     -> [{onoff,8},sealed];
	    ?PRODUCT_PDD_25     -> [{inch,2.5}];
	    ?PRODUCT_PDI_12     -> [{din,12},{dout,8},{ain,4},{pout,1}];
	    ?PRODUCT_PDI_24     -> [{din,24}];
	    ?PRODUCT_PDB_DMX    -> [{din,4},{dout,4},{ain,4},{pout,4},dmx];
	    ?PRODUCT_PDB_MARINCO -> [marinco];
	    ?PRODUCT_PDB_KELLY  -> [kelly];
	    _ -> []
	end,
    decode(Es, [{product,{P,V,Major,Minor}}|Acc]);
decode([{?UAPP_VERSION, <<Vsn:32>>}|Es], Acc) ->
    DateTime = decode_app_vsn(Vsn),
    decode(Es, [{version,DateTime}|Acc]);
decode([{?UAPP_SERIAL, <<Serial:32>>}|Es], Acc) ->
    decode(Es, [{serial,Serial}|Acc]);
decode([{?UAPP_ADDR, <<Serial:32>>}|Es], Acc) ->
    decode(Es, [{addr,Serial}|Acc]);
decode([{?UAPP_BIN, Data}|Es], Acc) ->
    decode(Es, [{data,Data}|Acc]);
decode([], Acc) ->
    Acc.

decode_app_vsn(undefined) -> undefined;
decode_app_vsn(Vsn) ->
    calendar:gregorian_seconds_to_datetime(Vsn+62167219200).
