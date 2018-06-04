%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Gordon flash programmer / node viewer
%%% @end
%%% Created :  4 Jun 2018 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(gordon_view).

-behaviour(gen_server).

-include_lib("can/include/can.hrl").
-include_lib("canopen/include/canopen.hrl").
-include_lib("canopen/include/sdo.hrl").
-include_lib("canopen/include/co_app.hrl").

%% API
-export([start_link/0, start/0]).

%%-export([serial_flash_bootloader/0]).
%%-export([can_flash_bootloader/1]).
%%-export([can_flash_application/1, can_flash_application/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
	{
	  uart,      %% serial flash uart device
	  timer,     %% CANbus ping timer
	  nodes = [] %% list of node maps
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    can_router:start(),
    can_udp:start(),
    hex_epx_server:start_link([{width,640}, {height,480}]),
    %% application:start(hex_epx),
    gen_server:start({local, ?SERVER}, ?MODULE, [{width,640}, {height,480}],
		     []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Options) ->
    Width  = proplists:get_value(width, Options, 640),
    Height = proplists:get_value(height, Options, 480),
    node_table(Width, Height),
    can_router:attach(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = {error,bad_call},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    io:format("got cast ~p\n", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Frame, State) when is_record(Frame,can_frame) ->
    CanID = Frame#can_frame.id,
    CobID = ?CANID_TO_COBID(CanID),
    {Func,_ID} = if ?is_can_id_eff(CanID) ->
			{?XFUNCTION_CODE(CobID),?XNODE_ID(CobID)};
		   true ->
			{?FUNCTION_CODE(CobID),?NODE_ID(CobID)}
		end,
    case Func of
	?PDO1_TX  -> pdo1_tx(CobID,Frame#can_frame.data,State);
	?SDO_TX   -> sdo_tx(CobID, Frame#can_frame.data,State);
	?SDO_RX   -> sdo_rx(CobID, Frame#can_frame.data,State);
	_ ->
	    io:format("Frame = ~p\n", [Frame]),
	    {noreply, State}
    end;
handle_info(_Info, State) ->
    io:format("got info ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Node table
%% +------+---+----------+---+--------+
%% |Serial| ID|Product   |Vsn| Status |
%% +------+---+----------+---+--------+
%% |801001|100|bridgeZone|2.3|  Boot  |
%% +------+---+----------+---+--------+

node_table(W,_H) ->
    header(0,W),
    [row(I,W) || I <- lists:seq(1,16)].

header(I,_W) ->
    Y = I*15+3,
    H = 15,
    Opts = [{font_color,white}, {color,black}, {fill,solid}],
    X0 = 3,

    text_cell("serial_header", X0, Y, 60, H,
	      [{text,"Serial"},{halign,right}|Opts]),
    X1 = X0 + 61,
    text_cell("id_header", X1, Y, 30, H, 
	      [{text,"ID"},{halign,right}|Opts]),

    X2 = X1 + 31,
    text_cell("product_header", X2, Y, 100, H,
	      [{text,"Product"},{halign,center}|Opts]),

    X3 = X2 + 101,
    text_cell("vsn_header", X3, Y, 30, H,
	      [{text,"Vsn"},{halign,center}|Opts]),

    X4 = X3 + 31,
    text_cell("status_header", X4, Y, 50, H,
	      [{text,"Status"},{halign,center}|Opts]).


row(I,_W) ->
    II = integer_to_list(I),
    Y = I*15+3,
    H = 15,

    X0 = 3,
    text_cell("serial_"++II, X0, Y, 60, H,
	      [{text,""},{halign,right}]),

    X1 = X0 + 61,
    text_cell("id_"++II, X1, Y, 30, H,
	      [{text,""},{halign,right}]),

    X2 = X1 + 31,
    text_cell("product_"++II, X2, Y, 100, H,
	      [{text,""},{halign,center}]),

    X3 = X2 + 101,
    text_cell("vsn_"++II, X3, Y, 30, H,
	      [{text,""},{halign,center}]),
    
    X4 = X3 + 31,
    text_cell("status_"++II, X4, Y, 50, H,
	      [{text,""},{halign,center}]).

text_cell(ID, X, Y, W, H, Opts) ->
    hex_epx:init_event(out,
		       [{id,ID},{type,text},
			{font,[{name,"Arial"},{slant,roman},{size,10}]},
			{x,X},{y,Y},
			{width,W},{height,H},{valign,center}|Opts]),
    hex_epx:init_event(out,
		       [{id,ID++".border"},{type,rectangle},
			{color,black},{x,X},{y,Y},{width,W+1},{height,H+1}]).


pdo1_tx(CobID,Data,State) ->
    case Data of
	<<16#80,?MSG_UBOOT_ON:16/little,_Si:8,Value:32/little>> ->
	    Serial = Value bsr 8,
	    node_booted(CobID, Serial, State);
	<<16#80,?MSG_POWER_ON:16/little,_Si:8,Value:32/little>> ->
	    Serial = Value bsr 8,
	    node_started(CobID, Serial, State);
	<<16#80,Index:16/little,Si:8,Value:32/little>> ->
	    node_message(CobID, Index, Si, Value, State);
	_ ->
	    io:format("Bad PDO1_TX CobID=~w data=~w\n", [CobID,Data]),
	    {noreply, State}
    end.

%% request to node
sdo_rx(CobID,Bin,State) ->
    case Bin of
	?ma_ccs_initiate_download_request(N,E,S,Index,SubInd,Data) when 
	      E =:= 1->
	    Value = sdo_value(S,N,Data),
	    io:format("sdo_rx: CobID=~s, SET index=~w, si=~w, value=~w\n", 
		      [integer_to_list(CobID,16), Index,SubInd,Value]);

	?ma_ccs_initiate_upload_request(Index,SubInd) ->
	    io:format("sdo_rx: CobID=~s, GET index=~w, si=~w\n", 
		      [integer_to_list(CobID,16), Index,SubInd]);

	_ ->
	    io:format("sdo_rx: CobID=~s, only  expedited mode supported\n",
		      [integer_to_list(CobID,16)])
    end,
    {noreply,State}.

%% generate a request to read node values
send_sdo_rx(CobId, Index, SubInd) ->
    Bin = ?sdo_ccs_initiate_upload_request(0,Index,SubInd,0),
    CobId1 = case ?is_cobid_extended(CobId) of
		 true ->
		     NodeId = ?XNODE_ID(CobId),
		     ?XCOB_ID(?SDO_RX,NodeId);
		 false ->
		     NodeId = ?NODE_ID(CobId),
		     ?COB_ID(?SDO_RX,NodeId)
	     end,
    io:format("XCobId1 = ~8.16.0B\n", [CobId1]),
    CanId = ?COBID_TO_CANID(CobId1),
    Frame = #can_frame { id=CanId,len=8,data=Bin},
    can:send(Frame).

%% reply from node
sdo_tx(CobID,Bin,State) ->  
    case Bin of
	?ma_scs_initiate_download_response(Index,SubInd) ->
	    io:format("sdo_tx: CobID=~s, SET RESP index=~w, si=~w\n",
		      [integer_to_list(CobID,16),Index,SubInd]),
	    {noreply,State};

	?ma_scs_initiate_upload_response(N,E,S,Index,SubInd,Data) when
	      E =:= 1 ->
	    Value = sdo_value(S,N,Data),
	    io:format("sdo_tx: CobID=~s, GET RESP index=~w, si=~w, value=~w\n", 
		      [integer_to_list(CobID,16),Index,SubInd,Value]),
	    State1 = set_value_by_cobid(CobID,Index,SubInd,Value,State),
	    {noreply,State1};
	_ ->
	    io:format("sdo_tx: CobID=~s, only  expedited mode supported\n",
		      [integer_to_list(CobID,16)]),
	    {noreply,State}
    end.

sdo_value(0,_N,_Bin) -> <<>>;
sdo_value(1,0,<<Value:32/little>>) -> Value;
sdo_value(1,1,<<Value:24/little,_:8>>) -> Value;
sdo_value(1,2,<<Value:16/little,_:16>>) -> Value;
sdo_value(1,3,<<Value:8/little,_:24>>) -> Value.

node_booted(_CobID, Serial, State) ->
    io:format("Node ~s booted\n", [integer_to_list(Serial,16)]),
    Nodes = set_status_by_serial(Serial, boot, State#state.nodes),
    {noreply, State#state { nodes=Nodes }}.

node_started(CobId, Serial, State) ->
    io:format("Node ~6.16.0B started\n", [Serial]),
    Nodes = set_status_by_serial(Serial, up, State#state.nodes),
    spawn(
      fun() ->
	      XCobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
	      io:format("XCobId = ~8.16.0B\n", [XCobId]),
	      send_sdo_rx(XCobId, ?INDEX_ID, 0),
	      send_sdo_rx(XCobId, ?IX_IDENTITY_OBJECT, ?SI_IDENTITY_PRODUCT)
	      %% ...
      end),
    {noreply, State#state { nodes=Nodes }}.

node_message(_CobID, Index, Si, Value, State) ->
    io:format("Value index=~w:~w value=~w\n", [Index,Si,Value]),
    {noreply, State}.

set_status_by_serial(Serial, Status, Ns) ->
    case take_node_by_serial(Serial, Ns) of
	{value,N=#{ pos := Pos },Ns1} ->
	    set_text(status,Pos,Status),
	    [ N#{ status => Status } | Ns1];
	false ->
	    Pos = length(Ns)+1,
	    set_text(status,Pos,Status),
	    set_text(serial,Pos,serial_to_text(Serial)),
	    Node = #{ pos => Pos, serial => Serial, status => Status },
	    [ Node | Ns]
    end.

set_value_by_cobid(CobId,Index,SubInd,Value,State) ->
    case Index of
	?INDEX_ID ->
	    set_by_cobid(CobId,id,integer_to_list(Value),State);
	?IX_IDENTITY_OBJECT when SubInd =:= ?SI_IDENTITY_PRODUCT ->
	    Product = (Value bsr 16) band 16#ff,
	    _Variant = (Value bsr 24) band 16#ff,
	    Vsn = integer_to_list((Value bsr 8) band 16#ff) ++ "." ++
		integer_to_list(Value band 16#ff),
	    State1 = set_by_cobid(CobId,vsn,Vsn,State),
	    case Product of
		1 ->
		    set_by_cobid(CobId,product,powerZone,State1);
		2 -> 
		    set_by_cobid(CobId,product,controlZone,State1);
		4 ->
		    set_by_cobid(CobId,product,ioZone,State1);
		9 ->
		    set_by_cobid(CobId,product,bridgeZone,State1);
		_ ->
		    State1
	    end;
	_ ->
	    State
    end.

set_by_cobid(CobID,Key,Value,State) ->
    case ?is_cobid_extended(CobID) of
	true ->
	    Serial = ?XNODE_ID(CobID),
	    Ns = State#state.nodes,
	    case take_node_by_serial(Serial, Ns) of
		false ->
		    Pos = length(Ns)+1,
		    set_text(serial,Pos,serial_to_text(Serial)),
		    set_text(Key,Pos,Value),
		    N = #{ pos=>Pos, serial=>Serial, Key=>Value },
		    State#state { nodes=[N|Ns]};
		{value,N=#{ pos := Pos},Ns1} ->
		    set_text(Key,Pos,Value),
		    N1 = N#{ Key => Value },
		    State#state { nodes=[N1|Ns1]}
	    end;
	false ->
	    ID = ?NODE_ID(CobID),
	    Ns = State#state.nodes,
	    case take_node_by_id(ID, Ns) of
		false ->
		    Pos = length(Ns)+1,
		    set_text(id,Pos,id_to_text(ID)),
		    set_text(Key,Pos,Value),
		    N = #{ pos=>Pos, id=>ID, Key=>Value },
		    State#state { nodes=[N|Ns]};
		{value,N=#{ pos := Pos},Ns1} ->
		    set_text(Key,Pos,Value),
		    N1 = N#{ Key => Value },
		    State#state { nodes=[N1|Ns1]}
	    end
    end.
	    
set_text(Key,Pos,Value) ->
    ID = atom_to_list(Key) ++ "_" ++ integer_to_list(Pos),
    io:format("set_text: id=~s, value=~s\n", [ID,Value]),
    hex_epx:output([{id,ID}],[{text,Value}]).

serial_to_text(Serial) ->
    tl(integer_to_list(16#1000000 + Serial, 16)).

id_to_text(ID) ->
    integer_to_list(ID).

%% find node with Serial
take_node_by_serial(Serial, Ns) ->
    take_node_by_serial(Serial, Ns, []).

take_node_by_serial(Serial, [Node=#{ serial := Serial}|Ns], Ms) ->
    {value,Node,Ns++Ms};
take_node_by_serial(Serial, [Node|Ns], Ms) ->
    take_node_by_serial(Serial, Ns, [Node|Ms]);
take_node_by_serial(_Serial, [], _Ms) ->
    false.


%% find node with ID
take_node_by_id(Id, Ns) ->
    take_node_by_id(Id, Ns, []).

take_node_by_id(Id, [Node=#{ id := Id}|Ns], Ms) ->
    {value,Node,Ns++Ms};
take_node_by_id(Id, [Node|Ns], Ms) ->
    take_node_by_id(Id, Ns, [Node|Ms]);
take_node_by_id(_Id, [], _Ms) ->
    false.
