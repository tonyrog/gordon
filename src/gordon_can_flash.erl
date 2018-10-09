%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%     upload data to CAN nodes
%%% @end
%%% Created : 15 Jun 2018 by Tony Rogvall <tony@rogvall.se>

-module(gordon_can_flash).

-include_lib("can/include/can.hrl").
-include_lib("canopen/include/canopen.hrl").
-include_lib("canopen/include/sdo.hrl").
-include_lib("canopen/include/co_app.hrl").

-export([upload/4, upload/5]).
-compile(export_all).

-define(ABORT_LPC_IAP_BASE, 16#0BAD0000). 
-define(IAP_CMD_SUCCESS,         0).

-define(is_result_iap(R),
	(((R) band 16#FFFFFF00) == ?ABORT_LPC_IAP_BASE)).

-define(is_result_iap_success(R),
	(?is_result_iap(R) andalso (((R) band 16#FF) == ?IAP_CMD_SUCCESS))).


%% FIXME: add flash multiple non disjoint segments?
upload(Nid,[{Addr,Segment}],Bsize,Progress) ->
    upload(Nid,Addr,Segment,Bsize,Progress).

upload(Nid,Addr,Data,Bsize,Progress) ->
    co_sdo_cli:attach(Nid),
    Len = byte_size(Data),
    Nblocks = (Len+Bsize-1) div Bsize,
    Kb = (Nblocks*Bsize+1023) div 1024,     %% kb to transfer
    Len1 = Nblocks*Bsize,                   %% transfer size
    case co_sdo_cli:get(Nid,?INDEX_BOOT_VSN,0,1000) of
	{error, abort_no_such_object} ->
	    {error, not_in_boot_mode};
	{error, Error} ->
	    {error, Error};
	{ok, _BootVsn} ->
	    case co_sdo_cli:set(Nid,?INDEX_UBOOT_ADDR,1,Addr,1000) of
		ok ->
		    case co_sdo_cli:set(Nid,?INDEX_UBOOT_ERASE,1,1024*Kb,10000) of
			ok ->
			    blocks_upload(Nid,Addr,Data,Bsize,{0,0,Len1},
					  Progress);
			{error,Code} when ?is_result_iap_success(Code) ->
			    blocks_upload(Nid,Addr,Data,Bsize,{0,0,Len1},
					  Progress);
			Error ->
			    Error
		    end;
		Error ->
		    Error
	    end
    end.

%% FIXME add retry
blocks_upload(Nid,_Addr,<<>>,_Bsize,{L,_L0,Len},Progress) ->
    Progress(L/Len),
    co_sdo_cli:set(Nid,?INDEX_BOOT_APP_VSN,0,16#2F5EBD7A,1000);
blocks_upload(Nid,Addr,Data,Bsize,LLen,Progress) ->
    {Block,Data1} = get_block(Data,Bsize),
    case block_upload(Nid,Addr,Block,LLen,Progress) of
	{ok,Addr1,LLen1} ->
	    blocks_upload(Nid,Addr1,Data1,Bsize,LLen1,Progress);
	Error ->
	    Error
    end.

block_upload(Nid,Addr,Block,LLen,Progress) ->
    %%io:format("block_upload: size=~w, Block=~p\n", [byte_size(Block), Block]),
    case co_sdo_cli:set(Nid,?INDEX_UBOOT_ADDR,1,Addr,1000) of
	ok ->
	    N = byte_size(Block),
	    case block_upload_(Nid,0,Block,N,LLen,Progress) of
		{ok,LLen1} ->
		    Crc = crc32r(Block),
		    case co_sdo_cli:set(Nid,?INDEX_UBOOT_FLASH,1,Crc,2000) of
			ok ->
			    {ok,Addr+N,LLen1};
			{error,Code} when ?is_result_iap_success(Code) ->
			    {ok,Addr+N,LLen1};
			{error,Code} when is_integer(Code) ->
			    {error,decode_iap_error(Code)};
			Result -> Result
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.

%% Upload one block
block_upload_(_Nid,_I,_Data,0,{L,_L0,Len},Progress) ->
    Progress(L/Len),
    {ok,{L,L,Len}};
block_upload_(Nid,I,Data,N,_LLen={L,L0,Len},Progress) when N > 0 ->
    Buf = get_buf32(I, Data),
    case co_sdo_cli:set(Nid,?INDEX_UBOOT_WRITE,1,Buf,1000) of
	ok ->
	    K = byte_size(Buf),
	    L1 = L + K,
	    LLen1 = if trunc(100*L1/Len) > trunc(100*L0/Len) ->
			    Progress(L1/Len), 
			    {L1,L1,Len};
		       true ->
			    {L1,L0,Len}
		    end,
	    block_upload_(Nid,I+K,Data,N-K,LLen1,Progress);
	{error,Reason} -> %% retry?
	    {error,Reason}
    end.

get_buf32(I, Data) ->
    case Data of
	<<_:I/binary, Buf:4/binary, _/binary>> -> Buf;
	<<_:I/binary, Buf/binary>> -> Buf
    end.

get_block(Data, Bsize) ->
    case Data of
	<<Block:Bsize/binary, Data1/binary>> -> {Block,Data1};
	Block ->
	    Pad = Bsize - byte_size(Block),
	    {<<Block/binary, 0:Pad/integer-unit:8>>, <<>>}
    end.

-define(CRC_POLY,  16#04C11DB7).
-define(CRC_INIT,  16#FFFFFFFF).
-define(CRC_FINAL, 16#FFFFFFFF).
-define(CRC_MASK,  16#FFFFFFFF).
-define(CRC_WIDTH, 32).

crc_table() ->
    { 16#00000000,16#04C11DB7,16#09823B6E,16#0D4326D9,16#130476DC,
      16#17C56B6B,16#1A864DB2,16#1E475005,16#2608EDB8,16#22C9F00F,
      16#2F8AD6D6,16#2B4BCB61,16#350C9B64,16#31CD86D3,16#3C8EA00A,
      16#384FBDBD,16#4C11DB70,16#48D0C6C7,16#4593E01E,16#4152FDA9,
      16#5F15ADAC,16#5BD4B01B,16#569796C2,16#52568B75,16#6A1936C8,
      16#6ED82B7F,16#639B0DA6,16#675A1011,16#791D4014,16#7DDC5DA3,
      16#709F7B7A,16#745E66CD,16#9823B6E0,16#9CE2AB57,16#91A18D8E,
      16#95609039,16#8B27C03C,16#8FE6DD8B,16#82A5FB52,16#8664E6E5,
      16#BE2B5B58,16#BAEA46EF,16#B7A96036,16#B3687D81,16#AD2F2D84,
      16#A9EE3033,16#A4AD16EA,16#A06C0B5D,16#D4326D90,16#D0F37027,
      16#DDB056FE,16#D9714B49,16#C7361B4C,16#C3F706FB,16#CEB42022,
      16#CA753D95,16#F23A8028,16#F6FB9D9F,16#FBB8BB46,16#FF79A6F1,
      16#E13EF6F4,16#E5FFEB43,16#E8BCCD9A,16#EC7DD02D,16#34867077, 
      16#30476DC0,16#3D044B19,16#39C556AE,16#278206AB,16#23431B1C,
      16#2E003DC5,16#2AC12072,16#128E9DCF,16#164F8078,16#1B0CA6A1,
      16#1FCDBB16,16#018AEB13,16#054BF6A4,16#0808D07D,16#0CC9CDCA,
      16#7897AB07,16#7C56B6B0,16#71159069,16#75D48DDE,16#6B93DDDB,
      16#6F52C06C,16#6211E6B5,16#66D0FB02,16#5E9F46BF,16#5A5E5B08,
      16#571D7DD1,16#53DC6066,16#4D9B3063,16#495A2DD4,16#44190B0D,
      16#40D816BA,16#ACA5C697,16#A864DB20,16#A527FDF9,16#A1E6E04E,
      16#BFA1B04B,16#BB60ADFC,16#B6238B25,16#B2E29692,16#8AAD2B2F,
      16#8E6C3698,16#832F1041,16#87EE0DF6,16#99A95DF3,16#9D684044,
      16#902B669D,16#94EA7B2A,16#E0B41DE7,16#E4750050,16#E9362689,
      16#EDF73B3E,16#F3B06B3B,16#F771768C,16#FA325055,16#FEF34DE2,
      16#C6BCF05F,16#C27DEDE8,16#CF3ECB31,16#CBFFD686,16#D5B88683,
      16#D1799B34,16#DC3ABDED,16#D8FBA05A,16#690CE0EE,16#6DCDFD59,
      16#608EDB80,16#644FC637,16#7A089632,16#7EC98B85,16#738AAD5C,
      16#774BB0EB,16#4F040D56,16#4BC510E1,16#46863638,16#42472B8F,
      16#5C007B8A,16#58C1663D,16#558240E4,16#51435D53,16#251D3B9E,
      16#21DC2629,16#2C9F00F0,16#285E1D47,16#36194D42,16#32D850F5,
      16#3F9B762C,16#3B5A6B9B,16#0315D626,16#07D4CB91,16#0A97ED48,
      16#0E56F0FF,16#1011A0FA,16#14D0BD4D,16#19939B94,16#1D528623,
      16#F12F560E,16#F5EE4BB9,16#F8AD6D60,16#FC6C70D7,16#E22B20D2,
      16#E6EA3D65,16#EBA91BBC,16#EF68060B,16#D727BBB6,16#D3E6A601,
      16#DEA580D8,16#DA649D6F,16#C423CD6A,16#C0E2D0DD,16#CDA1F604,
      16#C960EBB3,16#BD3E8D7E,16#B9FF90C9,16#B4BCB610,16#B07DABA7,
      16#AE3AFBA2,16#AAFBE615,16#A7B8C0CC,16#A379DD7B,16#9B3660C6,
      16#9FF77D71,16#92B45BA8,16#9675461F,16#8832161A,16#8CF30BAD,
      16#81B02D74,16#857130C3,16#5D8A9099,16#594B8D2E,16#5408ABF7,
      16#50C9B640,16#4E8EE645,16#4A4FFBF2,16#470CDD2B,16#43CDC09C,
      16#7B827D21,16#7F436096,16#7200464F,16#76C15BF8,16#68860BFD,
      16#6C47164A,16#61043093,16#65C52D24,16#119B4BE9,16#155A565E,
      16#18197087,16#1CD86D30,16#029F3D35,16#065E2082,16#0B1D065B,
      16#0FDC1BEC,16#3793A651,16#3352BBE6,16#3E119D3F,16#3AD08088,
      16#2497D08D,16#2056CD3A,16#2D15EBE3,16#29D4F654,16#C5A92679,
      16#C1683BCE,16#CC2B1D17,16#C8EA00A0,16#D6AD50A5,16#D26C4D12,
      16#DF2F6BCB,16#DBEE767C,16#E3A1CBC1,16#E760D676,16#EA23F0AF,
      16#EEE2ED18,16#F0A5BD1D,16#F464A0AA,16#F9278673,16#FDE69BC4,
      16#89B8FD09,16#8D79E0BE,16#803AC667,16#84FBDBD0,16#9ABC8BD5,
      16#9E7D9662,16#933EB0BB,16#97FFAD0C,16#AFB010B1,16#AB710D06,
      16#A6322BDF,16#A2F33668,16#BCB4666D,16#B8757BDA,16#B5365D03,
      16#B1F740B4}.

%%
%% Initiate CRC value from CRC tabe
%%
crc32r_init() -> 
    ?CRC_INIT.

crc32r_final(Crc) -> 
    Crc bxor ?CRC_FINAL.

crc32r_update(Crc, <<>>) ->
    Crc;
crc32r_update(Crc, <<Byte:8,Bin/binary>>) ->
    crc32r_update(crc32r_update_byte(Crc,Byte), Bin).

crc32r_update_byte(Crc,Byte) ->
    Table = crc_table(),
    Index = (((Crc bsr (?CRC_WIDTH-8)) bxor Byte) band 16#ff) + 1,
    ((Crc bsl 8) bxor element(Index,Table)) band ?CRC_MASK.

crc32r(Data) ->
    crc32r_final(crc32r_update(crc32r_init(), Data)).

%% in application error 
%% Seazone abort codes

-define(IAP_INVALID_COMMAND,     1).
-define(IAP_SRC_ADDR_ERROR,      2).
-define(IAP_DST_ADDR_ERROR,      3).
-define(IAP_SRC_ADDR_NOT_MAPPED, 4).
-define(IAP_DST_ADDR_NOT_MAPPED, 5).
-define(IAP_COUNT_ERROR,         6).
-define(IAP_INVALID_SECTOR,      7).
-define(IAP_SECTOR_NOT_BLANK,    8).
-define(IAP_SECTOR_NOT_PREPARED, 9).  %% FOR_WRITE_OPERATION
-define(IAP_COMPARE_ERROR,       10).
-define(IAP_BUSY,                11).
%% Basically ISP codes
-define(IAP_PARAM_ERROR,         12).
-define(IAP_ADDR_ERROR,          13).
-define(IAP_ADDR_NOT_MAPPED,     14).
-define(IAP_CMD_LOCKED,          15).
-define(IAP_INVALID_CODE,        16).
-define(IAP_INVALID_BAUDRATE,    17).
-define(IAP_INVALID_STOP_BIT,    18).
-define(IAP_CODE_PROTECTION,     19).

decode_iap_error(Code) ->
    case ?is_result_iap(Code) of
	true ->
	    decode_iap(Code);
	false ->
	    Code
    end.


decode_iap(Code) ->
    case Code band 16#ff of
	?IAP_CMD_SUCCESS -> success;
	?IAP_INVALID_COMMAND -> 'invalid command';
	?IAP_SRC_ADDR_ERROR -> 'source address error';
	?IAP_DST_ADDR_ERROR -> 'destination address error';
	?IAP_SRC_ADDR_NOT_MAPPED -> 'source address not mapped';
	?IAP_DST_ADDR_NOT_MAPPED -> 'destination address not mapped';
	?IAP_COUNT_ERROR -> 'count error';
	?IAP_INVALID_SECTOR -> 'invalid sector';
	?IAP_SECTOR_NOT_BLANK -> 'sector not blank';
	?IAP_SECTOR_NOT_PREPARED -> 'sector not prepared';
	?IAP_COMPARE_ERROR -> 'compare error';
	?IAP_BUSY -> 'busy';
	?IAP_PARAM_ERROR -> 'parameter error';
	?IAP_ADDR_ERROR -> 'address error';
	?IAP_ADDR_NOT_MAPPED -> 'address not mapped';
	?IAP_CMD_LOCKED -> 'command locked';
	?IAP_INVALID_CODE -> 'invalid code';
	?IAP_INVALID_BAUDRATE -> 'invalid baudrate';
	?IAP_INVALID_STOP_BIT -> 'invalid baudrate';
	?IAP_CODE_PROTECTION -> 'code prtection';
	IAP -> IAP
    end.

