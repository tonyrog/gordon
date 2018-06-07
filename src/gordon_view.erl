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
	  selected,        %% Selected node
	  selected_eff=0,  %% Selected node
	  selected_sff=0,  %% Selected node
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
    SELF = self(),
    hex_epx:output([{id,"screen"}],[{static,false}]), %% allow close
    hex_epx:add_event([{id,"screen"}], screen,
		      fun(Signal,Env) ->
			      SELF ! {event,Signal,Env}
		      end),

    Width  = proplists:get_value(width, Options, 640),
    Height = proplists:get_value(height, Options, 480),
    node_table(Width, Height),

    control_demo(Width div 2, 10, Width, Height),

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
handle_info({event, screen, [{closed,true}]}, State) ->
    %% fixme: try to terminate gracefully
    {stop, normal, State};

handle_info({row_select,ID,[{press,1},{row,R}]},State) ->
    case find_node_by_pos(R, State#state.nodes) of
	false ->
	    io:format("deselect old\n"),
	    {noreply, deselect_row(State#state.selected,State)};
	{value,Node} ->
	    EFF = case maps:get(serial,Node,0) of
		      0 -> 0;
		      N -> ?XCOB_ID(?PDO1_TX,N)
		  end,
	    SFF = case maps:get(id,Node,0) of
		      0 -> 0;
		      M -> ?COB_ID(?PDO1_TX,M)
		  end,
	    State1 = deselect_row(State#state.selected,State),
	    State2 = State#state { selected = Node, 
				   selected_eff = EFF,
				   selected_sff = SFF },
	    {noreply, State2}
    end;
handle_info({row_select,ID,[{press,0},{row,R}]},State) ->
    %% ignore mouse release
    {noreply, State};


handle_info({switch,Label,[{value,Value}]},State) ->
    case Value of
	0 -> hex_epx:output([{id,Label}],[{color,lightgray},{text,"OFF"}]);
	1 -> hex_epx:output([{id,Label}],[{color,green},{text,"ON"}])
    end,
    {noreply, State};    
handle_info(_Info, State) ->
    io:format("gordon_view: got info ~p\n", [_Info]),
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

deselect_row(undefined, State) ->
    State;
deselect_row(Row, State) ->
    State#state { selected=undefined }.

%% Node table
%% +------+---+----------+---+--------+
%% |Serial| ID|Product   |Vsn| Status |
%% +------+---+----------+---+--------+
%% |801001|100|bridgeZone|2.3|  Boot  |
%% +------+---+----------+---+--------+

node_table(W,_H) ->
    text("dummy", 0, 0, 10, 10, [{text,""}]),
    {ok,[{font,Font}]} = hex_epx:get_event("dummy", [font]),
    epx_gc:set_font(Font),
    {TxW0,TxH} = epx_font:dimension(epx_gc:current(), "Wy"),
    TxW = TxW0 div 2,
    XOffs = 8,
    YOffs = 8,
    NRows = 16,
    TabX = header(0,XOffs,YOffs,TxW,TxH,W),
    TabWidth = TabX - XOffs,
    [row(I,XOffs,YOffs,TxW,TxH,W) || I <- lists:seq(1,NRows)],
    %% create a invisible overlay for row selection
    selection_layer(XOffs, YOffs, NRows, TxH, TabWidth).

selection_layer(XOffs,YOffs,NRows,RowHeight,TableWidth) ->
    SELF = self(),
    ID = "row_select",
    hex_epx:init_event(out,
		       [{id,ID},{type,rectangle},
			%% {fill,none},{color,red},
			{x,XOffs},{y,YOffs+RowHeight},
			{width,TableWidth},{height,NRows*RowHeight}]),
    hex_epx:add_event([{id,ID}],select,
		      fun(_Signal,[{press,1},{x,_},{y,Y}|_]) ->
			      Row = (Y div RowHeight)+1,
			      SELF ! {row_select,ID,[{press,1},{row,Row}]};
			 (_Signal,[{press,0},{x,_},{y,Y}|_]) ->
			      Row = (Y div RowHeight)+1,
			      SELF ! {row_select,ID,[{press,1},{row,Row}]};
			 (_Signal, _Env) ->
			      io:format("bad select signal=~w, env=~w\n",
					[_Signal,_Env])
		      end).

%% install table header return next X location
header(I,XOffs,YOffs,TxW,TxH,_W) ->
    Y = I*TxH+YOffs,
    H = TxH,
    Opts = [{font_color,white}, {color,black}, {fill,solid}],
    X0 = XOffs,
    W0 = 6*TxW,
    text_cell("serial_header", X0, Y, W0, H,
	      [{text,"Serial"},{halign,right}|Opts]),
    X1 = X0 + W0 + 1,
    W1 = 3*TxW,
    text_cell("id_header", X1, Y, W1, H, 
	      [{text,"ID"},{halign,right}|Opts]),
    X2 = X1 + W1 + 1,
    W2 = 10*TxW,
    text_cell("product_header", X2, Y, W2, H,
	      [{text,"Product"},{halign,center}|Opts]),
    X3 = X2 + W2 + 1,
    W3 = 3*TxW,
    text_cell("vsn_header", X3, Y, W3, H,
	      [{text,"Vsn"},{halign,center}|Opts]),
    X4 = X3 + W3 + 1,
    W4 = 5*TxW,
    text_cell("status_header", X4, Y, W4, H,
	      [{text,"Status"},{halign,center}|Opts]),
    X4 + W4 + 1.

%% install table row return next X location
row(I,XOffs,YOffs,TxW,TxH,_W) ->
    II = integer_to_list(I),
    Y = I*TxH+YOffs,
    H = TxH,

    X0 = XOffs,
    W0 = 6*TxW,
    text_cell("serial_"++II, X0, Y, W0, H,
	      [{text,""},{halign,right}]),
    X1 = X0 + W0 + 1,
    W1 = 3*TxW,
    text_cell("id_"++II, X1, Y, W1, H,
	      [{text,""},{halign,right}]),
    X2 = X1 + W1 + 1,
    W2 = 10*TxW,
    text_cell("product_"++II, X2, Y, W2, H,
	      [{text,""},{halign,center}]),
    X3 = X2 + W2 + 1,
    W3 = 3*TxW,
    text_cell("vsn_"++II, X3, Y, W3, H,
	      [{text,""},{halign,center}]),
    X4 = X3 + W3 + 1,
    W4 = 5*TxW,
    text_cell("status_"++II, X4, Y, W4, H,
	      [{text,""},{halign,center}]),
    X4 + W4 + 1.


text_cell(ID, X, Y, W, H, Opts) ->
    text(ID, X, Y, W, H, Opts),
    border(ID, X, Y, W, H, Opts).

text(ID,X,Y,W,H,Opts) ->
    hex_epx:init_event(out,
		       [{id,ID},{type,text},
			{font,[{name,"Arial"},{slant,roman},{size,12}]},
			{x,X},{y,Y},
			{width,W},{height,H},{valign,center}|Opts]).

border(ID,X,Y,W,H,_Opts) ->
    hex_epx:init_event(out,
		       [{id,ID++".border"},{type,rectangle},
			{color,black},{x,X},{y,Y},{width,W+1},{height,H+1}]).

%% draw various "widgets"
control_demo(X, Y, W, H) ->
    bridgeZone(X,Y,W,H).

%% bridgeZone layout
bridgeZone(X,Y,W,H) ->
    Y1 = Y+10,
    X1 = X+10,
    X2 = X+10+64,
    
    %% Aout x 2 (row=Y1,column X1)
    XAout = X1,
    YAout = Y1,
    WAout = 32,
    HAout = 12,
    aout("aout_1", XAout, YAout+0,  WAout, HAout),
    aout("aout_2", XAout, YAout+16, WAout, HAout),

    Y2 = YAout+32,
    %% Ain x 4 (row=Y2,column=X1)
    XAin  = X1,
    YAin  = Y2,
    WAin = 24,
    HAin = 12,
    ain("ain_1",  XAin, YAin+0,  WAin, HAin),
    ain("ain_2",  XAin, YAin+16, WAin, HAin),
    ain("ain_3",  XAin, YAin+32, WAin, HAin),
    ain("ain_4",  XAin, YAin+48, WAin, HAin),

    %% Pout x 4 (row=Y2,column=X2)
    XPout  = X2,
    YPout  = Y2,
    WPout  = 32,
    HPout  = 12,
    pout("pout_1",  XPout, YPout+0, WPout, HPout),
    pout("pout_2",  XPout, YPout+16, WPout, HPout),
    pout("pout_3",  XPout, YPout+32, WPout, HPout),
    pout("pout_4",  XPout, YPout+48, WPout, HPout),
    
    Y3 = YAin+72,
    %% Din x 4 (row Y3,column=X1)
    XDin = X1,
    YDin = Y3,
    WDin = 24,
    HDin = 12,
    din("din_1", XDin, YDin+0, WDin, HDin),
    din("din_2", XDin, YDin+16, WDin, HDin),
    din("din_3", XDin, YDin+32, WDin, HDin),
    din("din_4", XDin, YDin+48, WDin, HDin),

    %% Dout x 4 (row Y3,column=X2)
    XDout = X2,
    YDout = Y3,
    WDout = 24,
    HDout = 12,
    dout("dout_1", XDout, YDout+0, WDout, HDout),
    dout("dout_2", XDout, YDout+16, WDout, HDout),
    dout("dout_3", XDout, YDout+32, WDout, HDout),
    dout("dout_4", XDout, YDout+48, WDout, HDout),

    ok.

    

dout(ID, X, Y, W, H) ->
    SELF = self(),
    hex_epx:init_event(in,
		       [{id,ID},{type,switch},
			{halign,center},
			{x,X},{y,Y},{width,W},{height,H},
			{shadow_x,3},{shadow_y,3},{children_first,false},
			{font,[{name,"Arial"},{weight,bold},{size,10}]},
			{fill,solid},{color,lightgray},
			{text,"OFF"}
		       ]),
    hex_epx:add_event([{id,ID}],switch,
		      fun(Signal,Env) ->
			      SELF ! {Signal,ID,Env}
		      end),
    ok.

din(ID, X, Y, W, H) ->
    hex_epx:init_event(out,
		       [{id,ID},{type,value},
			{halign,center},{valign,center},
			{x,X},{y,Y},{width,W},{height,H},
			{children_first,false},
			{font,[{name,"Arial"},{weight,bold},{size,10}]},
			{fill,solid},{color,white},
			{format,"~w"}
		       ]),
    hex_epx:init_event(out,
		       [{id,ID++".border"},
			{type,rectangle},
			{color,black},
			{relative,true},
			{x,-1},{y,-1},
			{width,W+2},{height,H+2}]),
    ok.

ain(ID, X, Y, W, H) ->
    hex_epx:init_event(out,
		       [{id,ID},{type,value},
			{halign,center},{valign,center},
			{x,X},{y,Y},{width,W},{height,H},
			{children_first,false},
			{font,[{name,"Arial"},{weight,bold},{size,10}]},
			{fill,solid},{color,white},
			{format,"~5w"},
			{value,0}
		       ]),
    hex_epx:init_event(out,
		       [{id,ID++".border"},
			{type,rectangle},
			{color,black},
			{relative,true},
			{x,-1},{y,-1},
			{width,W+2},{height,H+2}]),
    ok.

aout(ID, X, Y, W, H) ->
    SELF = self(),
    hex_epx:init_event(in,
		       [{id,ID},{type,slider},
			{x,X},{y,Y},{width,W},{height,8},
			{fill,solid},{color,lightBlue},
			{min,0},{max,65535},
			{orientation, horizontal},
			{border,1}
			%% {topimage, "$/gordon//priv/knob.png"}
		       ]),
    hex_epx:add_event([{id,ID}],analog,
		      fun(Signal,Env) ->
			      SELF ! {Signal,ID,Env}
		      end),
    ok.

pout(ID, X, Y, W, H) ->
    SELF = self(),
    hex_epx:init_event(in,
		       [{id,ID},{type,slider},
			{x,X},{y,Y},{width,W},{height,6},
			{fill,solid},{color,lightGreen},
			{min,0},{max,65535},
			{orientation, horizontal},
			{border, 1}
			%% {topimage, "$/gordon//priv/knob.png"}
		       ]),
    hex_epx:add_event([{id,ID}],analog,
		      fun(Signal,Env) ->
			      SELF ! {Signal,ID,Env}
		      end),
    ok.
    


pdo1_tx(CobID,Data,State) ->
    case Data of
	<<16#80,?MSG_UBOOT_ON:16/little,_Si:8,Value:32/little>> ->
	    Serial = Value bsr 8,
	    node_booted(CobID, Serial, State);
	<<16#80,?MSG_POWER_ON:16/little,_Si:8,Value:32/little>> ->
	    Serial = Value bsr 8,
	    node_started(CobID, Serial, State);
	<<16#80,?MSG_ECHO_REPLY:16/little,_Si:8,Value:32/little>> ->
	    Serial = Value bsr 8,
	    node_running(CobID, Serial, State);
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

node_started(_CobId, Serial, State) ->
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

node_running(_CobId, Serial, State) ->
    io:format("Node ~6.16.0B running\n", [Serial]),
    %% FIXME: fix boot check
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


node_message(CobID, Index, Si, Value, State) ->
    if CobID =:= State#state.selected_eff;
       CobID =:= State#state.selected_sff ->
	    node_data(Index, Si, Value, State);
       true ->
	    io:format("Value index=~w:~w value=~w\n", [Index,Si,Value]),
	    {noreply, State}
    end.

node_data(Index, Si, Value, State) ->
    case Index of
	?MSG_ANALOG ->
	    case Si of %% bridgeZone
		37 -> hex_epx:output([{id,"ain_1"}],[{value,Value}]);
		38 -> hex_epx:output([{id,"ain_2"}],[{value,Value}]);
		39 -> hex_epx:output([{id,"ain_3"}],[{value,Value}]);
		40 -> hex_epx:output([{id,"ain_4"}],[{value,Value}]);
		_ -> ignore
	    end;
	?MSG_DIGITAL ->
	    case Si of %% bridgeZone
		33 -> hex_epx:output([{id,"din_1"}],[{value,Value}]);
		34 -> hex_epx:output([{id,"din_2"}],[{value,Value}]);
		35 -> hex_epx:output([{id,"din_3"}],[{value,Value}]);
		46 -> hex_epx:output([{id,"din_4"}],[{value,Value}]);
		_ -> ignore
	    end;
	_ ->
	    ignore
    end,
    {noreply,State}.


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
	    set_by_cobid(CobId,id,Value,State);
	?IX_IDENTITY_OBJECT when SubInd =:= ?SI_IDENTITY_PRODUCT ->
	    Product = (Value bsr 16) band 16#ff,
	    %% _Variant = (Value bsr 24) band 16#ff,
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
		    set_text(Key,Pos,to_text(Value)),
		    N = #{ pos=>Pos, serial=>Serial, Key=>Value },
		    State#state { nodes=[N|Ns]};
		{value,N=#{ pos := Pos},Ns1} ->
		    set_text(Key,Pos,to_text(Value)),
		    N1 = N#{ Key => Value },
		    State#state { nodes=[N1|Ns1]}
	    end;
	false ->
	    ID = ?NODE_ID(CobID),
	    Ns = State#state.nodes,
	    case take_node_by_id(ID, Ns) of
		false ->
		    Pos = length(Ns)+1,
		    set_text(id,Pos,to_text(ID)),
		    set_text(Key,Pos,to_text(Value)),
		    N = #{ pos=>Pos, id=>ID, Key=>Value },
		    State#state { nodes=[N|Ns]};
		{value,N=#{ pos := Pos},Ns1} ->
		    set_text(Key,Pos,to_text(Value)),
		    N1 = N#{ Key => Value },
		    State#state { nodes=[N1|Ns1]}
	    end
    end.

to_text(Int) when is_integer(Int) -> integer_to_list(Int);
to_text(Text) when is_atom(Text) -> Text;
to_text(Text) when is_list(Text) -> Text.
	    
set_text(Key,Pos,Value) ->
    ID = atom_to_list(Key) ++ "_" ++ integer_to_list(Pos),
    io:format("set_text: id=~s, value=~s\n", [ID,Value]),
    hex_epx:output([{id,ID}],[{text,Value}]).

set_value(Key,Pos,Value) ->
    ID = atom_to_list(Key) ++ "_" ++ integer_to_list(Pos),
    io:format("set_value: id=~s, value=~p\n", [ID,Value]),
    hex_epx:output([{id,ID}],[{value,Value}]).

serial_to_text(Serial) ->
    tl(integer_to_list(16#1000000 + Serial, 16)).

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

%% find node with pos
find_node_by_pos(Pos, [Node=#{ pos := Pos}|Ns]) ->
    {value,Node};
find_node_by_pos(Pos, [Node|Ns]) ->
    find_node_by_pos(Pos, Ns);
find_node_by_pos(_Id, []) ->
    false.
