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
-export([start_rpi/0]).

-export([event/3]).  %% fixme better unique callback name

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
	  selected_id,     %% "pds"/"pdb"/"pdi"/"pdc"
	  row_height = 1,  %% height of row selection area
	  nodes = [] %% list of node maps
	}).

-define(TEXT_CELL_FONT_SIZE, 14).
-define(DIN_FONT_SIZE, 12).
-define(DOUT_FONT_SIZE, 12).
-define(AIN_FONT_SIZE, 12).
-define(GROUP_FONT_SIZE, 10).

-define(dbg(F,A), io:format((F),(A))).
-define(warn(F,A), io:format((F),(A))).
-define(error(F,A), io:format((F),(A))).

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
    application:load(gordon),
    application:load(can),
    application:set_env(can, wakeup, true),
    can_router:start(),
    can_udp:start(),
    hex_epx_server:start_link([{width,800}, {height,480}]),
    gen_server:start({local, ?SERVER}, ?MODULE, [{width,800}, {height,480}],
		     []).

%% start rpi with touch screen
start_rpi() ->
    ok = application:load(epx),
    application:set_env(epx, backend, "fb"),
    application:set_env(epx, pixel_format, 'argb/little'),
    application:set_env(epx, input_mouse_device, "/dev/input/event0"),
    application:load(gordon),
    application:load(can),
    application:set_env(can, wakeup, true),
    can_router:start(),
    can_udp:start(),
    %% can_usb:start(0),
    hex_epx_server:start_link([{width,800}, {height,480}]),
    gen_server:start({local, ?SERVER}, ?MODULE, [{width,800},{height,480}],
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
    hex_epx:output([{id,"screen"}],[{static,false}]), %% allow close
    hex_epx:add_event([{id,"screen"}],event,?MODULE),
    Width  = proplists:get_value(width, Options, 800),
    Height = proplists:get_value(height, Options, 480),
    RowHeight = node_table(Width div 2, Height),

    control_demo(Width div 2, 10, Width div 3, Height-32),

    can_router:attach(),

    %% request response from all nodes
    send_pdo1_tx(0, ?MSG_ECHO_REQUEST, 0, 0),

    {ok, #state{ row_height=RowHeight }}.

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
handle_info({event,"screen",[{closed,true}]}, State) ->
    %% fixme: try to terminate gracefully
    {stop, normal, State};

%% handle select in node list
handle_info({select,_ID,[{press,1},{x,_},{y,Y}|_]},State) ->
    R = (Y div State#state.row_height)+1,
    case find_node_by_pos(R, State#state.nodes) of
	false ->
	    io:format("deselect row=~w\n", [State#state.selected]),
	    {noreply, deselect_row(State#state.selected,State)};
	{value,Node} ->
	    Serial = maps:get(serial,Node,0),
	    EFF = case Serial of
		      0 -> 0;
		      N -> ?XCOB_ID(?PDO1_TX,N)
		  end,
	    SFF = case maps:get(id,Node,0) of
		      0 -> 0;
		      M -> ?COB_ID(?PDO1_TX,M)
		  end,
	    PDx = case maps:get(product,Node,undefined) of
		      undefined -> undefined;
		      powerZone -> "pds";
		      ioZone -> "pdi";
		      bridgeZone -> "pdb";
		      controlZone -> "pdc";
		      _ -> undefined
		  end,
	    io:format("deselect row=~w\n", [State#state.selected]),
	    State1 = deselect_row(State#state.selected,State),
	    State2 = State1#state { selected = Node, 
				    selected_eff = EFF,
				    selected_sff = SFF,
				    selected_id  = PDx
				  },
	    hex_epx:output([{id,PDx}],[{hidden,false},{disabled,false}]),
	    io:format("select row=~w, eff=~8.16.0B, sff=~3.16.0B id=~s\n",
		      [R, EFF, SFF, PDx]),
	    send_pdo1_tx(0, ?MSG_REFRESH, 0, 0),
	    {noreply, State2}
    end;
handle_info({select,_ID,[{press,0},{x,_},{y,_}|_]},State) ->
    %% ignore mouse release in node selection
    {noreply, State};

handle_info({switch,ID,[{value,Value}]},State) ->
    Si = case ID of
	     "pdb.pout.e1.onoff" -> 1;
	     "pdb.pout.e2.onoff" -> 2;
	     "pdb.pout.e3.onoff" -> 3;
	     "pdb.pout.e4.onoff" -> 4;
	     "pdb.aout.e5.onoff" -> 5;
	     "pdb.aout.e6.onoff" -> 6;
	     "pdb.dout.e7" -> 7;
	     "pdb.dout.e8" -> 8;
	     "pdb.dout.e9" -> 9;
	     "pdb.dout.e10" -> 10;
	     
	     "pds.pout.e1.onoff" -> 1;
	     "pds.pout.e2.onoff" -> 2;
	     "pds.pout.e3.onoff" -> 3;
	     "pds.pout.e4.onoff" -> 4;
	     "pds.pout.e5.onoff" -> 5;
	     "pds.pout.e6.onoff" -> 6;
	     "pds.pout.e7.onoff" -> 7;
	     "pds.pout.e8.onoff" -> 8;

	     "pdi.dout.e1" -> 1;
	     "pdi.dout.e2" -> 2;
	     "pdi.dout.e3" -> 3;
	     "pdi.dout.e4" -> 4;
	     "pdi.dout.e5" -> 5;
	     "pdi.dout.e6" -> 6;
	     "pdi.dout.e7" -> 7;
	     "pdi.dout.e8" -> 8;
	     
	     _ -> -1
	 end,
    if Si =/= -1 ->
	    ?dbg("switch send_digital2, ~s value=~w\n", [ID,Value]),
	    send_digital2(State#state.selected_eff,Si,Value);
       true ->
	    ok
    end,
    {noreply, State};


handle_info({analog,ID,[{value,Value}]},State) ->
    Si = case ID of
	     "pdb.aout.e5" -> 5;
	     "pdb.aout.e6" -> 6;
	     "pdb.pout.e1" -> 1;
	     "pdb.pout.e2" -> 2;
	     "pdb.pout.e3" -> 3;
	     "pdb.pout.e4" -> 4;

	     "pds.pout.e1" -> 1;
	     "pds.pout.e2" -> 2;
	     "pds.pout.e3" -> 3;
	     "pds.pout.e4" -> 4;
	     "pds.pout.e5" -> 5;
	     "pds.pout.e6" -> 6;
	     "pds.pout.e7" -> 7;
	     "pds.pout.e8" -> 8;
	     _ -> -1
	 end,
    if Si =/= -1 ->
	    ?dbg("analog send_analog2, ~s value=~w\n", [ID,Value]),
	    send_analog2(State#state.selected_eff,Si,Value);
       true ->
	    ok
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

%%
%% Event callback from epxy
%%
event(Signal,ID,Env) ->
    %% just send it as info message to access
    io:format("Got event callback ~p\n", [{Signal,ID,Env}]),
    ?SERVER ! {Signal,ID,Env}.

deselect_row(undefined, State) ->
    State;
deselect_row(_Row, State) ->
    case State#state.selected_id of
       undefined -> ok;
       ID -> hex_epx:output([{id,ID}],[{hidden,all},{disabled,all}])
    end,
    State#state { selected=undefined, selected_id=undefined }.

%% Node table
%% +------+---+----------+---+--------+
%% |Serial| ID|Product   |Vsn| Status |
%% +------+---+----------+---+--------+
%% |801001|100|bridgeZone|2.3|  Boot  |
%% +------+---+----------+---+--------+

node_table(W,_H) ->
    {TxW,TxH} = text_cell_dimension(),
    XOffs = 8,
    YOffs = 8,
    NRows = 16,
    TabX = header(0,XOffs,YOffs,TxW,TxH,W),
    TabWidth = TabX - XOffs,
    [row(I,XOffs,YOffs,TxW,TxH,W) || I <- lists:seq(1,NRows)],
    %% create a invisible overlay for row selection
    selection_layer(XOffs, YOffs, NRows, TxH, TabWidth),
    TxH.

selection_layer(XOffs,YOffs,NRows,RowHeight,TableWidth) ->
    ID = "row_select",
    hex_epx:init_event(out,
		       [{id,ID},{type,rectangle},
			{x,XOffs},{y,YOffs+RowHeight},
			{width,TableWidth},{height,NRows*RowHeight}]),
    hex_epx:add_event([{id,ID}],select,?MODULE).

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

text_cell_dimension() ->
    text("dummy", 0, 0, 10, 10, [{text,""}]),
    {ok,[{font,Font}]} = hex_epx:get_event("dummy", [font]),
    epx_gc:set_font(Font),
    {TxW0,TxH} = epx_font:dimension(epx_gc:current(), "Wy"),
    TxW = TxW0 div 2,
    {TxW,TxH}.

text(ID,X,Y,W,H,Opts) ->
    hex_epx:init_event(out,
		       [{id,ID},{type,text},
			{font,[{name,"Arial"},{slant,roman},{weight,bold},
			       {size,?TEXT_CELL_FONT_SIZE}]},
			{x,X},{y,Y},
			{width,W},{height,H},{valign,center}|Opts]).

border(ID,_X,_Y,W,H,_Opts) ->
    hex_epx:init_event(out,
		       [{id,ID++".border"},{type,rectangle},
			{relative, true},
			{color,black},{x,-1},{y,-1},{width,W+2},{height,H+2}]).

%% draw various "widgets"
control_demo(X, Y, W, H) ->
    bridgeZone(X,Y,W,H),
    ioZone(X,Y,W,H),
    powerZone(X,Y,W,H).

%% bridgeZone layout
bridgeZone(X,Y,W,H) ->
    Y1 = Y+10,
    X1 = X+10,
    X2 = X+10+64,
    YGap = 10,
    ID = "pdb",

    group_rectangle(ID,"bridgeZone",X,Y,W,H,all,false),

    %% Aout x 2 (row=Y1,column X1)
    {_,Y2,_W1,_H1} = aout_group("pdb.aout", 5, 6, X1, Y1),

    %% Ain x 4 (row=Y2,column=X1)
    {_,Y3,_W2,_H2} = ain_group("pdb.ain", 37, 40, X1, Y2+YGap),

    %% Din x 4 (row Y3,column=X1)
    {_,_,_W3,_H3} = din_group("pdb.din", 33, 36, X1, Y3+YGap),

    %% Pout x 4 (row=Y2,column=X2)
    {_,Y4,_W4,_H4} = pout_group("pdb.pout", 1, 4, X2, Y2+YGap),

    %% Dout x 4 (row Y3,column=X2)
    {_,_,_W5,_H5} = dout_group("pdb.dout", 7, 10, X2, Y4+YGap),
    ok.

ioZone(X,Y,W,H) ->
    Y1 = Y+10,
    X1 = X+10,
    X2 = X+10+64,
    YGap = 10,
    ID = "pdi",

    group_rectangle(ID,"ioZone",X,Y,W,H,all,false),

    %% Ain x 4 (row=Y1,column=X1)
    {_,Y3,_W2,_H2} = ain_group("pdi.ain", 65, 68, X1, Y1),

    %% Din x 12 (row Y3,column=X1) support iozone24?
    {_,_,_W3,_H3} = din_group("pdi.din", 33, 44, X1, Y3+YGap),

    %% Dout x 8 (row Y3,column=X2)
    {_,_,_W5,_H5} = dout_group("pdi.dout", 1, 8, X2, Y1),
    ok.

powerZone(X,Y,W,H) ->
    Y1 = Y+10,
    X1 = X+10,
    X2 = X+10+64,
    %% YGap = 10,
    ID = "pds",

    group_rectangle(ID,"powerZone",X,Y,W,H,all,false),

    %% Ain x 8 (row=Y1,column=X1)
    {_,_Y3,_W2,_H2} = ain_group("pds.ain", 1, 8, X1, Y1),

    %% Pout x 8 (row Y1,column=X2)
    {_,_,_W5,_H5} = pout_group("pds.pout", 1, 8, X2, Y1),
    ok.

%% build the analog out group return next Y value
aout_group(ID, Chan0, Chan1, X0, Y0) ->
    XLeft  = 12, XRight = 12,
    YTop   = 12, YBot   = 12,
    YGap   = 8,
    {Y2,W2} = lists:foldl(
		fun(Chan, {Yi,Wi}) ->
			{_,Y1,W1,_H1} = aout(ID,Chan,XLeft,Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0, Chan1)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Aout",X0,Y0,W,H,false,false),
    {X0,Y3,W,H}.

%% build the pwm out group return next Y value
pout_group(ID, Chan0, Chan1, X0, Y0) ->
    XLeft  = 12, XRight = 12,
    YTop   = 12, YBot  = 12,
    YGap   = 8,
    {Y2,W2} = lists:foldl(
		fun(Chan, {Yi,Wi}) ->
			{_,Y1,W1,_H1} = pout(ID,Chan,XLeft,Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0, Chan1)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Pout",X0,Y0,W,H,false,false),
    {X0,Y3,W,H}.

ain_group(ID, Chan0, Chan1, X0, Y0) ->
    XLeft  = 8, XRight = 8,
    YTop   = 12, YBot   = 8,
    YGap   = 4,
    {Y2,W2} = lists:foldl(
		fun(Chan, {Yi,Wi}) ->
			{_,Y1,W1,_H1} = ain(ID,Chan,XLeft,Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0, Chan1)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Ain",X0,Y0,W,H,false,false),
    {X0,Y3,W,H}.

din_group(ID, Chan0, Chan1, X0, Y0) ->
    XLeft  = 8, XRight = 8,
    YTop   = 12, YBot   = 8,
    YGap   = 4,
    {Y2,W2} = lists:foldl(
		fun(Chan, {Yi,Wi}) ->
			{_,Y1,W1,_H1} = din(ID,Chan,XLeft, Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0, Chan1)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Din",X0,Y0,W,H,false,false),
    {X0,Y3,W,H}.

dout_group(ID, Chan0, Chan1, X0, Y0) ->
    XLeft  = 12,  XRight = 12,
    YTop   = 12, YBot  = 12,
    YGap   = 8,
    {Y2,W2} = lists:foldl(
		fun(Chan, {Yi,Wi}) ->
			{_,Y1,W1,_H1} = dout(ID,Chan,XLeft,Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0, Chan1)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Dout",X0,Y0,W,H,false,false),
    {X0,Y3,W,H}.


dout(ID0,Chan,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 24, H = 12,
    hex_epx:init_event(in,
		       [{id,ID},{type,switch},
			{halign,center},
			{x,X},{y,Y},{width,W},{height,H},
			{relative,true},
			{shadow_x,2},{shadow_y,2},{children_first,false},
			{font,[{name,"Arial"},{weight,bold},{size,10}]},
			{fill,solid},{color,lightgray},
			{text,"OFF"}
		       ]),
    hex_epx:add_event([{id,ID}],switch,?MODULE),
    {X,Y+H,W,H}.

din(ID0,Chan,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 24, H = 12,
    hex_epx:init_event(out,
		       [{id,ID},{type,value},
			{halign,center},{valign,center},
			{x,X},{y,Y},{width,W},{height,H},
			{relative,true},
			{children_first,false},
			{font,[{name,"Arial"},{weight,bold},
			       {size,?DIN_FONT_SIZE}]},
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
    {X,Y+H,W,H}.

ain(ID0,Chan,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 32, H = 12,    
    hex_epx:init_event(out,
		       [{id,ID},{type,value},
			{halign,center},{valign,center},
			{x,X},{y,Y},{width,W},{height,H},
			{relative,true},
			{children_first,false},
			{font,[{name,"Arial"},{weight,bold},
			       {size,?AIN_FONT_SIZE}]},
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
    {X,Y+H,W,H}.
    
aout(ID0,Chan,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 100+32, H = 12,
    hex_epx:init_event(in,
		       [{id,ID},{type,slider},
			{x,X+40},{y,Y+2},{width,100},{height,8},
			{relative,true},
			{fill,solid},{color,lightBlue},
			{min,0},{max,65535},
			{orientation, horizontal},
			{border,1},
			{topimage, "$/gordon//priv/knob.png"}
		       ]),
    hex_epx:add_event([{id,ID}],analog,?MODULE),
    ID1 = ID++".onoff",
    hex_epx:init_event(in,
		       [{id,ID1},{type,switch},
			{halign,center},
			{x,-40},{y,-4},{width,24},{height,H},
			{relative,true},
			{shadow_x,2},{shadow_y,2},{children_first,false},
			{font,[{name,"Arial"},{weight,bold},{size,10}]},
			{fill,solid},{color,lightgray},
			{text,"OFF"}
		       ]),
    hex_epx:add_event([{id,ID1}],switch,?MODULE),
    {X,Y+H,W,H}.

pout(ID0,Chan,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 100+32, H = 12,
    hex_epx:init_event(in,
		       [{id,ID},{type,slider},
			{x,X+40},{y,Y+2},{width,100},{height,8},
			{relative,true},
			{fill,solid},{color,lightGreen},
			{min,0},{max,65535},
			{orientation, horizontal},
			{border, 1},
			{topimage, "$/gordon//priv/knob.png"}
		       ]),
    hex_epx:add_event([{id,ID}],analog,?MODULE),
    ID1 = ID++".onoff",
    hex_epx:init_event(in,
		       [{id,ID1},{type,switch},
			{halign,center},
			{x,-40},{y,-4},{width,24},{height,H},
			{relative,true},
			{shadow_x,2},{shadow_y,2},{children_first,false},
			{font,[{name,"Arial"},{weight,bold},{size,10}]},
			{fill,solid},{color,lightgray},
			{text,"OFF"}
		       ]),
    hex_epx:add_event([{id,ID1}],switch,?MODULE),
    {X,Y+H,W,H}.


group_rectangle(ID,Text,X,Y,W,H,Status,Relative) ->
    hex_epx:init_event(out,
		       [{id,ID},
			{hidden,Status},
			{disabled,Status},
			{relative,Relative},
			{type,rectangle},
			{children_first, false},
			{color,black},{x,X},{y,Y},
			{width,W},{height,H}]),
    hex_epx:init_event(out,
		       [{id,ID++".tag"},
			{type,text},
			{font,[{name,"Arial"},{slant,roman},
			       {size,?GROUP_FONT_SIZE}]},
			{font_color, black},
			{text,Text},
			{relative, true},
			{color,white},{fill,solid},
			{halign,left},
			{x,5},{y,-5},
			{height,10}  %% width,25
		       ]).

send_digital2(CobId, Si, Value) ->
    send_pdo2_tx(CobId,?MSG_DIGITAL,Si,Value).

send_analog2(CobId, Si, Value) ->
    send_pdo2_tx(CobId,?MSG_ANALOG,Si,Value).

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
	    ?dbg("sdo_rx: CobID=~s, SET index=~w, si=~w, value=~w\n", 
		 [integer_to_list(CobID,16), Index,SubInd,Value]);

	?ma_ccs_initiate_upload_request(Index,SubInd) ->
	    ?dbg("sdo_rx: CobID=~s, GET index=~w, si=~w\n", 
		 [integer_to_list(CobID,16), Index,SubInd]);

	_ ->
	    ?warn("sdo_rx: CobID=~s, only  expedited mode supported\n",
		  [integer_to_list(CobID,16)])
    end,
    {noreply,State}.

%% send PDO1_TX message
send_pdo1_tx(CobId, Index, SubInd, Value) ->
    Bin = <<16#80:8,Index:16/little,SubInd:8,Value:32/little>>,
    CobId1 = case ?is_cobid_extended(CobId) of
		 true ->
		     NodeId = ?XNODE_ID(CobId),
		     ?XCOB_ID(?PDO1_TX,NodeId);
		 false ->
		     NodeId = ?NODE_ID(CobId),
		     ?COB_ID(?PDO1_TX,NodeId)
	     end,
    CanId = ?COBID_TO_CANID(CobId1),
    Frame = #can_frame { id=CanId,len=8,data=Bin},
    can:send(Frame).


%% send PDO2_TX message (analog/digital etc)
send_pdo2_tx(0, _Index, _SubInd, _Value) ->
    ok;
send_pdo2_tx(_CobId, _Index, -1, _Value) ->
    ok;
send_pdo2_tx(CobId, Index, SubInd, Value) ->
    Bin = <<16#80:8,Index:16/little,SubInd:8,Value:32/little>>,
    CobId1 = case ?is_cobid_extended(CobId) of
		 true ->
		     NodeId = ?XNODE_ID(CobId),
		     ?XCOB_ID(?PDO2_TX,NodeId);
		 false ->
		     NodeId = ?NODE_ID(CobId),
		     ?COB_ID(?PDO2_TX,NodeId)
	     end,
    CanId = ?COBID_TO_CANID(CobId1),
    Frame = #can_frame { id=CanId,len=8,data=Bin},
    can:send(Frame).



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
sdo_tx(CobId,Bin,State) ->  
    case Bin of
	?ma_scs_initiate_download_response(Index,SubInd) ->
	    ?dbg("sdo_tx: CobId=~s, SET RESP index=~w, si=~w\n",
		 [integer_to_list(CobId,16),Index,SubInd]),
	    {noreply,State};

	?ma_scs_initiate_upload_response(N,E,S,Index,SubInd,Data) when
	      E =:= 1 ->
	    Value = sdo_value(S,N,Data),
	    ?dbg("sdo_tx: CobId=~s, GET RESP index=~w, si=~w, value=~w\n", 
		 [integer_to_list(CobId,16),Index,SubInd,Value]),
	    State1 = set_value_by_cobid(CobId,Index,SubInd,Value,State),
	    {noreply,State1};

	?ma_abort_transfer(Index,_SubInd,Code) ->
	    if Code =:= ?ABORT_NO_SUCH_OBJECT, Index =:= ?INDEX_BOOT_VSN ->
		    State1 = set_by_cobid(CobId,status,up,State),
		    {noreply,State1};
	       true ->
		    {noreply,State}
	    end;
	_ ->
	    ?warn("sdo_tx: CobId=~s, only  expedited mode supported\n",
		  [integer_to_list(CobId,16)]),
	    {noreply,State}
    end.

sdo_value(0,_N,_Bin) -> <<>>;
sdo_value(1,0,<<Value:32/little>>) -> Value;
sdo_value(1,1,<<Value:24/little,_:8>>) -> Value;
sdo_value(1,2,<<Value:16/little,_:16>>) -> Value;
sdo_value(1,3,<<Value:8/little,_:24>>) -> Value.

node_booted(_CobID, Serial, State) ->
    ?dbg("Node ~s booted\n", [integer_to_list(Serial,16)]),
    Nodes = set_status_by_serial(Serial, boot, State#state.nodes),
    {noreply, State#state { nodes=Nodes }}.

node_started(_CobId, Serial, State) ->
    ?dbg("Node ~6.16.0B started\n", [Serial]),
    Nodes = set_status_by_serial(Serial, up, State#state.nodes),
    spawn(
      fun() ->
	      XCobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
	      ?dbg("XCobId = ~8.16.0B\n", [XCobId]),
	      send_sdo_rx(XCobId, ?INDEX_ID, 0),
	      send_sdo_rx(XCobId, ?IX_IDENTITY_OBJECT, ?SI_IDENTITY_PRODUCT)
	      %% ...
      end),
    {noreply, State#state { nodes=Nodes }}.

node_running(_CobId, Serial, State) ->
    ?dbg("Node ~6.16.0B running\n", [Serial]),
    case find_node_by_serial(Serial, State#state.nodes) of
	false ->
	    spawn(
	      fun() ->
		      XCobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
		      io:format("XCobId = ~8.16.0B\n", [XCobId]),
		      send_sdo_rx(XCobId, ?INDEX_ID, 0),
		      send_sdo_rx(XCobId, ?IX_IDENTITY_OBJECT,
				  ?SI_IDENTITY_PRODUCT),
		      send_sdo_rx(XCobId, ?INDEX_BOOT_VSN, 0),
		      if XCobId =:= State#state.selected_eff ->
			      send_pdo1_tx(0, ?MSG_REFRESH, 0, 0);
			 true ->
			      ok
		      end
	      end),
	    {noreply, State};
	{value,_Node} ->
	    %% Node is present in node list
	    %% XCobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
	    %% send_pdo1_tx(0, ?MSG_REFRESH, 0, 0)
	    {noreply, State}
    end.

node_message(CobID, Index, Si, Value, State) ->
    if CobID =:= State#state.selected_eff;
       CobID =:= State#state.selected_sff ->
	    node_data(Index, Si, Value, State);
       true ->
	    ?dbg("Value index=~w:~w value=~w\n", [Index,Si,Value]),
	    {noreply, State}
    end.

node_data(Index, Si, Value, State) ->
    case Index of
	?MSG_ANALOG ->
	    ?dbg("MSG_ANALOG: [~s], si=~w, value=~w\n",
		 [State#state.selected_id,Si,Value]),
	    case State#state.selected_id of
		"pdb" ->
		    case Si of
			37 -> set_value("pdb.ain.e37", Value);
			38 -> set_value("pdb.ain.e38", Value);
			39 -> set_value("pdb.ain.e39", Value);
			40 -> set_value("pdb.ain.e40", Value);
			_ -> ok
		    end;
		"pds" ->
		    case Si of
			1 -> set_value("pds.ain.e1", Value);
			2 -> set_value("pds.ain.e2", Value);
			3 -> set_value("pds.ain.e3", Value);
			4 -> set_value("pds.ain.e4", Value);
			5 -> set_value("pds.ain.e5", Value);
			6 -> set_value("pds.ain.e6", Value);
			7 -> set_value("pds.ain.e7", Value);
			8 -> set_value("pds.ain.e8", Value);
			_ -> ok
		    end;
		"pdi" ->
		    case Si of
			65 -> set_value("pdi.ain.e65", Value);
			66 -> set_value("pdi.ain.e66", Value);
			67 -> set_value("pdi.ain.e67", Value);
			68 -> set_value("pdi.ain.e68", Value);
			_ -> ok
		    end;
		_ ->
		    ok
	    end;

	?MSG_DIGITAL ->
	    ?dbg("MSG_DIGITAL: [~s], si=~w, value=~w\n", 
		 [State#state.selected_id,Si,Value]),
	    case State#state.selected_id of
		"pdb" ->
		    case Si of
			33 -> set_value("pdb.din.e33", Value);
			34 -> set_value("pdb.din.e34", Value);
			35 -> set_value("pdb.din.e35", Value);
			36 -> set_value("pdb.din.e36", Value);
			_ -> ok
		    end;
		"pdi" ->
		    case Si of
			33 -> set_value("pdi.din.e33", Value);
			34 -> set_value("pdi.din.e34", Value);
			35 -> set_value("pdi.din.e35", Value);
			36 -> set_value("pdi.din.e36", Value);
			37 -> set_value("pdi.din.e37", Value);
			38 -> set_value("pdi.din.e38", Value);
			39 -> set_value("pdi.din.e39", Value);
			40 -> set_value("pdi.din.e40", Value);
			41 -> set_value("pdi.din.e41", Value);
			42 -> set_value("pdi.din.e42", Value);
			43 -> set_value("pdi.din.e43", Value);
			44 -> set_value("pdi.din.e44", Value);
			_ -> ok
		    end;
		_ ->
		    ok
	    end;

	?MSG_OUTPUT_ACTIVE ->
	    ?dbg("MSG_OUTPUT_ACTIVE: [~s], si=~w, value=~w\n", 
		 [State#state.selected_id,Si,Value]),
	    case State#state.selected_id of
		"pdb" ->
		    case Si of
			1 -> switch_state("pdb.pout.e1.onoff",Value);
			2 -> switch_state("pdb.pout.e2.onoff",Value);
			3 -> switch_state("pdb.pout.e3.onoff",Value);
			4 -> switch_state("pdb.pout.e4.onoff",Value);
			5 -> switch_state("pdb.aout.e5.onoff",Value);
			6 -> switch_state("pdb.aout.e6.onoff",Value);
			7 -> switch_state("pdb.dout.e7",Value);
			8 -> switch_state("pdb.dout.e8",Value);
			9 -> switch_state("pdb.dout.e9",Value);
			10 -> switch_state("pdb.dout.e10",Value);
			_ -> undefined
		    end;

		"pds" ->
		    case Si of
			1 -> switch_state("pds.pout.e1.onoff",Value);
			2 -> switch_state("pds.pout.e2.onoff",Value);
			3 -> switch_state("pds.pout.e3.onoff",Value);
			4 -> switch_state("pds.pout.e4.onoff",Value);
			5 -> switch_state("pds.pout.e5.onoff",Value);
			6 -> switch_state("pds.pout.e6.onoff",Value);
			7 -> switch_state("pds.pout.e7.onoff",Value);
			8 -> switch_state("pds.pout.e8.onoff",Value);
			_ -> undefined
		    end;

		"pdi" ->
		    case Si of
			1 -> switch_state("pdi.dout.e1",Value);
			2 -> switch_state("pdi.dout.e2",Value);
			3 -> switch_state("pdi.dout.e3",Value);
			4 -> switch_state("pdi.dout.e4",Value);
			5 -> switch_state("pdi.dout.e5",Value);
			6 -> switch_state("pdi.dout.e6",Value);
			7 -> switch_state("pdi.dout.e7",Value);
			8 -> switch_state("pdi.dout.e8",Value);
			_ -> undefined
		    end;
		_ ->
		    undefined
	    end;

	?MSG_OUTPUT_STATE ->
	    OnOff = (Value bsr 24) band 16#ff,
	    OutSt = (Value bsr 16) band 16#ff,
	    Duty  = Value band 16#ffff,
	    ?dbg("MSG_OUTPUT_STATE: [~s], si=~w,on=~w,state=~w,duty=~w\n", 
		 [State#state.selected_id,Si,OnOff,OutSt,Duty]),
	    case Si of
		%% bridge zone: Pout=1-4, Aout =5-6, Dout=7-10
		%% ioZone:      Dout=1-8
		%% powerZone:   Pout=1-8
		_ -> ok
	    end;
	_ ->
	    ignore
    end,
    {noreply,State}.

switch_state(ID,0) ->
    hex_epx:output([{id,ID}],[{color,lightgray},{text,"OFF"}]);
switch_state(ID,_) ->
    hex_epx:output([{id,ID}],[{color,green},{text,"ON"}]).

set_value(ID, Value) ->
    hex_epx:output([{id,ID}],[{value,Value}]).

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
	?INDEX_BOOT_VSN ->
	    set_by_cobid(CobId,status,boot,State);
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

find_node_by_pos(Pos, Ns) ->
    find_node_by_key(pos, Pos, Ns).

find_node_by_serial(Serial, Ns) ->
    find_node_by_key(serial, Serial, Ns).

%% find node with key and Value
find_node_by_key(Key,Value,[Node|Ns]) ->
    case Node of
	#{ Key := Value} ->
	    {value,Node};
	_ ->
	    find_node_by_key(Key,Value,Ns)
    end;
find_node_by_key(_Key, _Value, []) ->
    false.
