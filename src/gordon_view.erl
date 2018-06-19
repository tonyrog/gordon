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

-compile(export_all).

-export([firmware_info/0]).
%%-export([serial_flash_bootloader/0]).
%%-export([can_flash_bootloader/1]).
%%-export([can_flash_application/1, can_flash_application/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
	{
	  uart,            %% serial flash uart device
	  timer,           %% CANbus ping timer
	  hold_mode=false, %% hold the (selected) booting node
	  selected_pos,    %% Selected row | undefined
	  selected_eff=0,  %% extended canid of selected node
	  selected_sff=0,  %% short canid of selected node
	  selected_id,     %% "pds"/"pdb"/"pdi"/"pdc"
	  sdo_request,     %% current outstanding sdo_request
	  sdo_error,       %% last sdo_error
	  row_height = 1,  %% height of row selection area
	  firmware = [],   %% list of available firmware for upgrade
	  nodes = [] %% list of node maps
	}).

-define(TEXT_CELL_FONT_SIZE, 18).
-define(DIN_FONT_SIZE, 14).
-define(DOUT_FONT_SIZE, 14).
-define(AIN_FONT_SIZE, 14).
-define(ALOAD_FONT_SIZE, 14).
-define(BUTTON_FONT_SIZE, 14).
-define(ONOFF_FONT_SIZE, 14).
-define(GROUP_FONT_SIZE, 12).
-define(SLIDER_WIDTH,  100).
-define(SLIDER_HEIGHT, 8).
-define(ONOFF_WIDTH, 40).
-define(BUTTON_WIDTH, 64).
-define(BUTTON_HEIGHT, 15).

-define(dbg(F,A), io:format((F),(A))).
%%-define(dbg(F,A), ok).
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
    epxy:start_link([{width,800}, {height,480}]),
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
    epxy:start_link([{width,800}, {height,480}]),
    gen_server:start({local, ?SERVER}, ?MODULE, [{width,800},{height,480}],
		     []).

firmware_info() ->
    gen_server:call(?SERVER, firmware_info).

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
    epxy:set("screen",[{static,false}]), %% allow close
    epxy:add_callback("screen",event,?MODULE),
    Width  = proplists:get_value(width, Options, 800),
    Height = proplists:get_value(height, Options, 480),

    {_XOffset,_YOffset,RowHeight} = node_table(Width div 2, Height),

    %% define various layouts
    X = Width div 2,
    Y = 10,
    W = Width div 3,
    H = Height - 32,
    bridgeZone(X,Y,W,H),
    ioZone(X,Y,W,H),
    powerZone(X,Y,W,H),

    can_router:attach(),

    %% request response from all nodes
    send_pdo1_tx(0, ?MSG_ECHO_REQUEST, 0, 0),

    Firmware = load_firmware(),

    {ok, #state{ row_height = RowHeight, firmware = Firmware }}.

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
handle_call(firmware_info, _From, State) ->
    lists:foreach(
      fun({uapp,{Product,Variant,Major,Minor},Banks}) ->
	      io:format("~s ~w ~w.~w size=~wK\n",
			[Product,Variant,Major,Minor,
			 (firmware_size(Banks)+1023) div 1024]);
	 ({ihex, Banks}) ->
	      io:format("uboot firmware size=~wK\n", 
			[(firmware_size(Banks)+1023) div 1024])
      end, State#state.firmware),
    {reply,ok,State};
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
    ?dbg("Frame = ~p\n", [Frame]),
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
	    ?dbg("Frame = ~p\n", [Frame]),
	    {noreply, State}
    end;
    
handle_info({event,"screen",[{closed,true}]}, State) ->
    %% fixme: try to terminate gracefully
    {stop, normal, State};

%% handle select in node list
handle_info({select,"nodes.r"++RTxt,[{press,1},{x,_X},{y,_Y}|_]},State) ->
    Pos = list_to_integer(RTxt),
    case find_node_by_pos(Pos,State#state.nodes) of
	false ->
	    ?dbg("deselect row=~w\n", [State#state.selected_pos]),
	    {noreply, deselect_row(State#state.selected_pos,State)};
	Node ->
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
	    ?dbg("deselect row=~w\n", [State#state.selected_pos]),
	    State1 = deselect_row(State#state.selected_pos,State),
	    State2 = State1#state { selected_pos = Pos,
				    selected_eff = EFF,
				    selected_sff = SFF,
				    selected_id  = PDx
				  },
	    select_row(Pos),
	    epxy:set(PDx,[{hidden,false},{disabled,false}]),
	    State3 = refresh_node_state(Node, State2),
	    ?dbg("select row=~w, eff=~8.16.0B, sff=~3.16.0B id=~s\n",
		 [Pos, EFF, SFF, PDx]),
	    send_pdo1_tx(0, ?MSG_REFRESH, 0, 0),
	    {noreply, State3}
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

%% send a reset and set hold mode
handle_info({button,[_,_,_|".hold"],[{value,1}]},State) ->
    case find_node_by_pos(State#state.selected_pos,State#state.nodes) of
	false ->
	    {noreply,State};
	Node ->
	    Serial = maps:get(serial,Node,0),
	    send_pdo1_tx(0, ?MSG_RESET, 0, Serial),
	    {noreply,State#state{hold_mode = true }}
    end;

%% leave boot mode
handle_info({button,[_,_,_|".go"],[{value,1}]},State) ->
    WDT = 1,
    io:format("Go: hold=~w,selected=~w\n", [State#state.hold_mode,
					    State#state.selected_pos]),
    {noreply,action_sdo(State,boot,?INDEX_UBOOT_GO,0,WDT)};

handle_info({button,[_,_,_|".upgrade"],[{value,1}]},State) ->
    %% locate firmware image given selected product
    %% check that firmware is an upgrade or first firmware
    %% must be an uapp.
    %% start process disable all input (except flash dialog)
    %% show progress remove flash dialog and enable input
    {noreply,State};

%% reset the node
handle_info({button,[_,_,_|".reset"],[{value,1}]},State) ->
    %% send a reset and set hold mode
    case find_node_by_pos(State#state.selected_pos,State#state.nodes) of
	undefined ->
	    {noreply,State};
	Node ->
	    Serial = maps:get(serial,Node,0),
	    send_pdo1_tx(0, ?MSG_RESET, 0, Serial),
	    {noreply,State#state{hold_mode = false }}
    end;

handle_info({button,[_,_,_|".setup"],[{value,1}]},State) ->
    case find_node_by_pos(State#state.selected_pos,State#state.nodes) of
	undefined ->
	    {noreply,State};
	Node ->
	    Status = maps:get(status,Node,undefined),
	    Serial = maps:get(serial,Node,0),
	    if Status =:= up ->
		    XCobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
		    case State#state.selected_id of
			"pdb" -> bridgeZone_setup(XCobId,State);
			"pds" -> powerZone_setup(XCobId,State);
			"pdi" -> ioZone_setup(XCobId,State);
			"pdc" -> controlZone_setup(XCobId,State);
			_ -> {noreply,State}
		    end;
	       true ->
		    {noreply,State}
	    end
    end;

handle_info({button,[_,_,_|".factory"],[{value,1}]},State) ->
    {noreply,action_sdo(State,up,?IX_RESTORE_DEFAULT_PARAMETERS,4,<<"daol">>)};

handle_info({button,[_,_,_|".save"],[{value,1}]},State) ->
    {noreply,action_sdo(State,up,?IX_STORE_PARAMETERS,1,<<"evas">>)};

handle_info({button,[_,_,_|".restore"],[{value,1}]},State) ->
    {noreply, action_sdo(State,up,?IX_RESTORE_DEFAULT_PARAMETERS,1,<<"daol">>)};

handle_info({button,_ID,[{value,0}]},State) ->
    %% ignore button release
    {noreply,State};

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


%% FIXME: update button status according to state
%%  upgrade iff state=boot and firmware is available and correct
%%  factory/save/hold iff state=up
%%  go state=boot
%%  setup state=up
%%
refresh_node_state(State) ->
    case find_node_by_pos(State#state.selected_pos,State#state.nodes) of
	false -> State;
	Node -> refresh_node_state(Node,State)
    end.

refresh_node_state(Node, State) ->
    AppVsn = maps:get(app_vsn,Node,0),
    PDx = State#state.selected_id,
    epxy:set(PDx++".app_vsn", [{text,format_value(app_vsn,AppVsn)}]),
    case maps:get(status,Node,undefined) of
	undefined -> 
	    State;
	boot ->
	    disable_buttons(PDx,["hold","setup","factory","save","restore"]),
	    enable_buttons(PDx,["reset","go"]),
	    case match_uapp_firmware(Node, State) of
		false ->
		    disable_buttons(PDx,["upgrade"]);
		_UApp ->
		    enable_buttons(PDx,["upgrade"])
	    end,
	    State;
	up ->
	    enable_buttons(PDx, ["hold","setup","factory","save","restore"]),
	    enable_buttons(PDx,["reset"]),
	    disable_buttons(PDx,["go","upgrade"]),
	    State
    end.

disable_buttons(PDx, Bs) ->
    [epxy:set(PDx++"."++B, [{font_color,white},{disabled,true}]) ||
	B <- Bs].

enable_buttons(PDx, Bs) ->
    [epxy:set(PDx++"."++B, [{font_color,black},{disabled,false}]) ||
	B <- Bs].

match_uapp_firmware(Node, State) ->
    Product = maps:get(product,Node,undefined),
    Vsn = maps:get(vsn, Node, undefined),
    AppVsn = maps:get(app_vsn, Node, undefined),
    AppVersion = gordon_uapp:decode_app_vsn(AppVsn),
    io:format("Match firmware, ~p\n",
	      [{Product,Vsn,AppVersion,AppVersion}]),
    match_uapp_firmware_(Product,Vsn,AppVersion,State#state.firmware).

match_uapp_firmware_(Product,Vsn={Major,Minor},AppVersion,
		     [UApp={uapp,{Product,_Variant,Major,Minor},Banks}|Fs]) ->
    case match_uapp_vsn(AppVersion, Banks) of
	true -> UApp;
	false -> match_uapp_firmware_(Product,Vsn,AppVersion,Fs)
    end;
match_uapp_firmware_(Product,Vsn,AppVersion,[_UApp|Fs]) ->
    match_uapp_firmware_(Product,Vsn,AppVersion,Fs);
match_uapp_firmware_(_Product,_Vsn,_AppVersion,[]) ->
    false.

match_uapp_vsn(AppVersion, [{banks,Match,_Bs}|Banks]) ->
    case proplists:get_value(version,Match) of
	undefined -> 
	    match_uapp_vsn(AppVersion,Banks);
	Version ->
	    io:format("Node AppVersion=~p, File Version=~p\n",
		      [AppVersion,Version]),
	    if AppVersion =:= undefined;
	       Version > AppVersion -> true;
	       true -> match_uapp_vsn(AppVersion, Banks)
	    end
    end;
match_uapp_vsn(_AppVersion, []) ->
    false.
    

action_sdo(State, Status, Index, Si, Value) ->
    case find_node_by_pos(State#state.selected_pos,State#state.nodes) of
	false ->
	    State;
	Node ->
	    CurrentStatus = maps:get(status,Node,undefined),
	    Serial = maps:get(serial,Node,0),
	    %% io:format("Serial = ~w, Status = ~w\n", [Serial, Status]),
	    if CurrentStatus =:= Status ->
		    XCobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
		    co_sdo_cli:send_sdo_set(XCobId, Index, Si, Value),
		    State#state { sdo_request = { Index, Si }};
	       true ->
		    State
	    end
    end.

%%
%% powerZone test setup
%% set output-type:1-8 dimmer
%% set output-max-step:1-8 255
%% set output-flags:1-8 anload,outact
%% set input-flags:32 active
%% set input-out:32 1..8
%%
powerZone_setup(_XCobId,_State) ->
    ok.
%%
%% bridgeZone test setup
%% set output-type:1-6 dimmer
%% set output-max-step:1-6 255
%% set output-type:7-10 onoff
%% set output-flags:1-10 outact
%% set input-flags:32 active
%% set input-out:32 1..10
%%
bridgeZone_setup(_XCobId, _State) ->
    ok.

%%
%% ioZone test setup
%% set output-type:1-8 onoff
%% set output-flags:7-10 outact
%% set input-flags:32 active
%% set input-out:32 1..8
%%
ioZone_setup(_XCobId,_State) ->
    ok.

%%
%% controlZone test setup
%%
controlZone_setup(_XCobId,_State) ->
    ok.

%%
%% Event callback from epxy
%%
event(Signal,ID,Env) ->
    %% just send it as info message to access
    ?dbg("Got event callback ~p\n", [{Signal,ID,Env}]),
    ?SERVER ! {Signal,ID,Env}.

select_row(Row) ->
    RowID = "nodes.r"++integer_to_list(Row),
    epxy:set(RowID,[{hidden,false}]).

deselect_row(undefined, State) ->
    State;
deselect_row(Pos, State) ->
    case State#state.selected_id of
	undefined -> 
	    ok;
	PDx ->
	    RowID = "nodes.r"++integer_to_list(Pos),
	    epxy:set(PDx,[{hidden,all},{disabled,all}]),
	    epxy:set(RowID,[{hidden,true}])
    end,
    State#state { selected_pos=undefined, selected_id=undefined }.

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
    ID = "nodes",
    TabX = table_header(ID,TxW,TxH,W),
    TabWidth = TabX - XOffs,
    %% create parent after header, order only important at the time of draw
    table(ID,XOffs,YOffs,TabWidth,NRows*TxH),
    [table_row(ID,I,TxW,TxH,W) || I <- lists:seq(1,NRows)],
    {XOffs,YOffs,TxH}.

%% install table header return next X location
table_header(Parent,TxW,TxH,_W) ->
    ID = Parent++".h", %%
    X = 0,
    Y = 0,
    H = TxH,
    Opts = [{font_color,white},{color,black},{fill,solid}],

    row(ID,X,Y,(6+3+10+3+5)*TxW+(1+1+1+1+1),H,true),

    X0 = 0,
    W0 = 6*TxW,
    text_cell(ID++".serial", X0, Y, W0, H,
	      [{text,"Serial"},{halign,right}|Opts]),
    X1 = X0 + W0 + 1,
    W1 = 3*TxW,
    text_cell(ID++".id", X1, Y, W1, H, 
	      [{text,"ID"},{halign,right}|Opts]),
    X2 = X1 + W1 + 1,
    W2 = 10*TxW,
    text_cell(ID++".product", X2, Y, W2, H,
	      [{text,"Product"},{halign,center}|Opts]),
    X3 = X2 + W2 + 1,
    W3 = 3*TxW,
    text_cell(ID++".vsn", X3, Y, W3, H,
	      [{text,"Vsn"},{halign,center}|Opts]),
    X4 = X3 + W3 + 1,
    W4 = 5*TxW,
    text_cell(ID++".status", X4, Y, W4, H,
	      [{text,"Status"},{halign,center}|Opts]),
    X4 + W4 + 1.

%% install table row return next X location
table_row(Parent,I,TxW,TxH,_W) ->
    ID = Parent++[$.,$r|integer_to_list(I)], %% <id>.r<i>
    X = 0, Y = I*TxH, H = TxH,

    %% parent to table cells
    row(ID,X,Y,(6+3+10+3+5)*TxW+(1+1+1+1+1),H,false),
    epxy:add_callback(ID,select,?MODULE),
    
    %% and now the cells
    X0 = 0,
    W0 = 6*TxW,
    text_cell(ID++".serial", X0, 0, W0, H,
	      [{text,""},{halign,right}]),
    X1 = X0 + W0 + 1,
    W1 = 3*TxW,
    text_cell(ID++".id", X1, 0, W1, H,
	      [{text,""},{halign,right}]),
    X2 = X1 + W1 + 1,
    W2 = 10*TxW,
    text_cell(ID++".product", X2, 0, W2, H,
	      [{text,""},{halign,center}]),
    X3 = X2 + W2 + 1,
    W3 = 3*TxW,
    text_cell(ID++".vsn", X3, 0, W3, H,
	      [{text,""},{halign,center}]),
    X4 = X3 + W3 + 1,
    W4 = 5*TxW,
    text_cell(ID++".status", X4, 0, W4, H,
	      [{text,""},{halign,center}]),
    X4 + W4 + 1.

%% parent to all rows
table(ID, X, Y, W, H) ->
    epxy:new(ID,[{type,rectangle},
		 {hidden,true},{disabled,true},
		 {x,X},{y,Y},{width,W},{height,H}]).

%% parent to all cells maybe used for row selections? 
row(ID, X, Y, W, H, Disabled) ->
    epxy:new(ID,[{type,rectangle},
		 {hidden,true},{disabled,Disabled},
		 {children_first,false},
		 {fill,blend},{color,red},
		 {x,X},{y,Y},{width,W},{height,H}]).

text_cell(ID,X,Y,W,H,Opts) ->
    text(ID,X,Y,W,H,Opts),
    border(ID,W,H,Opts).

text_cell_dimension() ->
    text("dummy", 0, 0, 10, 10, [{text,""}]),
    {ok,[{font,Font}]} = epxy:get("dummy", [font]),
    epx_gc:set_font(Font),
    {TxW0,TxH} = epx_font:dimension(epx_gc:current(), "Wy"),
    TxW = TxW0 div 2,
    {TxW,TxH}.

text(ID,X,Y,W,H,Opts) ->
    epxy:new(ID,[{type,text},
		 {font,[{name,"Arial"},{slant,roman},{weight,bold},
			{size,?TEXT_CELL_FONT_SIZE}]},
		 {x,X},{y,Y},
		 {width,W},{height,H},{valign,center}|Opts]).

border(ID,W,H,_Opts) ->
    epxy:new(ID++".border",
	     [{type,rectangle},
	      {color,black},{x,-1},{y,0},{width,W+2},{height,H+1}]).

%% bridgeZone layout
bridgeZone(X,Y,_W,_H) ->
    XGap = 12,
    YGap = ?GROUP_FONT_SIZE,
    Y0 = YGap,
    X1 = XGap,
    ID = "pdb",

    {_,Y1,_W0,H0} = tagged_text("pdb.app_vsn", "Version", X1, Y0),

    %% Aout x 2 (row=Y1,column X1)
    {_,Y2,W1,H1} = aout_group("pdb.aout", 5, 6, X1, Y1+YGap),

    %% Ain x 4 (row=Y2,column=X1)
    {_,Y3,W2,H2} = ain_group("pdb.ain", 37, 40, X1, Y2+YGap),

    %% Din x 4 (row Y3,column=X1)
    {_,_,W3,H3} = din_group("pdb.din", 33, 36, X1, Y3+YGap),

    X2 = X1+max(W2,W3)+XGap,

    %% Pout x 4 (row=Y2,column=X2)
    {_,Y4,W4,H4} = pout_group("pdb.pout", 4, 1, X2, Y2+YGap),

    %% Dout x 4 (row Y3,column=X2)
    {_,Y5,W5,H5} = dout_group("pdb.dout", 10, 7, X2, Y4+YGap),

    {W6,H6} = add_buttons(ID, X1, Y5+YGap),

    Wt = XGap+max(max(W1,W6),max(W2+W4+XGap, W3+W5+XGap))+XGap,
    Ht = YGap+H0+YGap+H1+YGap+max(H2+YGap+H3, H4+YGap+H5)+YGap+H6+YGap,
    group_rectangle(ID,"bridgeZone",X,Y,Wt,Ht,all),
    ok.

ioZone(X,Y,_W,_H) ->
    XGap = 12,
    YGap = ?GROUP_FONT_SIZE,
    Y0 = YGap,
    X1 = XGap,
    X2 = X1+64,
    ID = "pdi",

    {_,Yn,_W0,H0} = tagged_text("pdi.app_vsn", "Version", X1, Y0),
    Y1 = Yn,

    %% Din x 12 (row Y3,column=X1) support iozone24?
    {_,Y2,W2,H2} = din_group("pdi.din", 33, 44, X1, Y1+YGap),

    %% Ain x 4 (row=Y1,column=X1)
    {_,Y3,W3,H3} = ain_group("pdi.ain", 65, 68, X1, Y2+YGap),

    %% Dout x 8 (row Y3,column=X2)
    {_,_,W4,H4} = dout_group("pdi.dout", 1, 8, X2, Y1+YGap),

    {W6,H6} = add_buttons(ID, X1, Y3+YGap),

    Wt = XGap+max(W2,max(W3+W4+XGap,W6))+XGap,
    Ht = YGap+H0+YGap+max(H2+H3+YGap,H4)+YGap+H6+2*YGap,

    group_rectangle(ID,"ioZone",X,Y,Wt,Ht,all),

    ok.

powerZone(X,Y,_W,_H) ->
    XGap = 12,
    YGap = ?GROUP_FONT_SIZE,
    Y0 = YGap,
    X1 = XGap,
    ID = "pds",

    {_,Y1,_W0,H0} = tagged_text("pds.app_vsn", "Version", X1, Y0),

    %% Ain x 8 (row=Y1,column=X1)
    {_,_Y3,W2,H2} = ain_group("pds.ain", 1, 8, X1, Y1+YGap),

    X2 = X1+W2+XGap,
    %% Pout x 8 (row Y1,column=X2)
    {_,_,W3,H3} = pout_group("pds.pout", 1, 8, X2, Y1+YGap),

    X3 = X2+W3+XGap,
    %% Aload x 8 (row=Y1,column=X3)
    {_,_Y4,W4,H4} = aload_group("pds.aload", 33, 40, X3, Y1),

    H5 = max(H2,max(H3,H4)),

    Y5 = Y1 + H5 + YGap,

    {W6,H6} = add_buttons(ID, X1, Y5),

    Wt = XGap+max(W2+XGap+W3+XGap+W4,W6)+XGap,
    Ht = YGap+H0+YGap+max(H2,max(H3,H4))+YGap+H6+YGap,

    group_rectangle(ID,"powerZone",X,Y,Wt,Ht,all),

    ok.


%% add hold and go buttons
add_buttons(ID, X, Y) ->
    W = ?BUTTON_WIDTH,
    H = ?BUTTON_HEIGHT,
    YGap = 10,
    XGap = 8,
    X0 = X,
    X1 = X0 + W + XGap,
    X2 = X1 + W + XGap,
    X3 = X2 + W + XGap,
    Y0 = Y,
    Y1 = Y0+H+YGap,

    add_text_button(ID++".hold",    "Hold",    X0, Y0, W, H),
    add_text_button(ID++".go",      "Go",      X1, Y0, W, H),
    add_text_button(ID++".upgrade", "Upgrade", X2, Y0, W, H),
    add_text_button(ID++".reset",   "Reset",   X3, Y0, W, H),

    add_text_button(ID++".setup",   "Setup",   X0, Y1, W, H),
    add_text_button(ID++".factory", "Factory", X1, Y1, W, H),
    add_text_button(ID++".save",    "Save",    X2, Y1, W, H),
    add_text_button(ID++".restore", "Restore",    X3, Y1, W, H),
    {4*W+3*XGap,2*H+2*YGap}.

add_text_button(ID, Text, X, Y, W, H) ->
    epxy:new(ID,
	     [{type,button},
	      {halign,center},
	      {x,X},{y,Y},{width,W},{height,H},
	      {shadow_x,2},{shadow_y,2},{children_first,false},
	      {font,[{name,"Arial"},{weight,bold},
		     {size,?BUTTON_FONT_SIZE}]},
	      {fill,solid},{color,lightgray},
	      {text,Text}
	     ]),
    epxy:add_callback(ID,button,?MODULE).


%% build the analog out group return next Y value
aout_group(ID, Chan0, Chan1, X0, Y0) ->
    Step = if Chan0 < Chan1 -> 1; true -> -1 end,
    XLeft  = 12, XRight = 12,
    YTop   = 12, YBot   = 12,
    YGap   = 8,
    {Y2,W2} = lists:foldl(
		fun(Chan, {Yi,Wi}) ->
			{_,Y1,W1,_H1} = aout(ID,Chan,XLeft,Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0,Chan1,Step)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Aout",X0,Y0,W,H,false),
    {X0,Y3,W,H}.

%% build the pwm out group return next Y value
pout_group(ID, Chan0, Chan1, X0, Y0) ->
    Step = if Chan0 < Chan1 -> 1; true -> -1 end,
    XLeft  = 12, XRight = 12,
    YTop   = 12, YBot  = 12,
    YGap   = 8,
    {Y2,W2} = lists:foldl(
		fun(Chan, {Yi,Wi}) ->
			{_,Y1,W1,_H1} = pout(ID,Chan,XLeft,Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0,Chan1,Step)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Pout",X0,Y0,W,H,false),
    {X0,Y3,W,H}.

ain_group(ID, Chan0, Chan1, X0, Y0) ->
    Step = if Chan0 < Chan1 -> 1; true -> -1 end,
    XLeft  = 12, XRight = 12,
    YTop   = 12, YBot   = 12,
    YGap   = 8,
    {Y2,W2} = lists:foldl(
		fun(Chan, {Yi,Wi}) ->
			{_,Y1,W1,_H1} = ain(ID,Chan,XLeft,Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0,Chan1,Step)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Ain [%]",X0,Y0,W,H,false),
    {X0,Y3,W,H}.

aload_group(ID, Chan0, Chan1, X0, Y0) ->
    Step = if Chan0 < Chan1 -> 1; true -> -1 end,
    XLeft  = 12, XRight = 12,
    YTop   = 12, YBot   = 12,
    YGap   = 8,
    {Y2,W2} = lists:foldl(
		fun(Chan, {Yi,Wi}) ->
			{_,Y1,W1,_H1} = aload(ID,Chan,XLeft,Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0,Chan1,Step)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Load [A]",X0,Y0,W,H,false),
    {X0,Y3,W,H}.

din_group(ID, Chan0, Chan1, X0, Y0) ->
    Step = if Chan0 < Chan1 -> 1; true -> -1 end,
    XLeft  = 12, XRight = 12,
    YTop   = 12, YBot   = 12,
    YGap   = 8,
    {Y2,W2} = lists:foldl(
		fun(Chan, {Yi,Wi}) ->
			{_,Y1,W1,_H1} = din(ID,Chan,XLeft, Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0,Chan1,Step)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Din",X0,Y0,W,H,false),
    {X0,Y3,W,H}.

dout_group(ID, Chan0, Chan1, X0, Y0) ->
    Step = if Chan0 < Chan1 -> 1; true -> -1 end,
    XLeft  = 12,  XRight = 12,
    YTop   = 12, YBot  = 12,
    YGap   = 8,
    {Y2,W2} = lists:foldl(
		fun(Chan, {Yi,Wi}) ->
			{_,Y1,W1,_H1} = dout(ID,Chan,XLeft,Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0,Chan1,Step)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Dout",X0,Y0,W,H,false),
    {X0,Y3,W,H}.


dout(ID0,Chan,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = ?ONOFF_WIDTH, H = ?ONOFF_FONT_SIZE,
    epxy:new(ID,[{type,switch},
		 {halign,center},
		 {x,X},{y,Y},{width,W},{height,H},
		 {shadow_x,2},{shadow_y,2},{children_first,false},
		 {font,[{name,"Arial"},{weight,bold},{size,?ONOFF_FONT_SIZE}]},
		 {fill,solid},{color,lightgray},
		 {text,"OFF"}
		]),
    epxy:add_callback(ID,switch,?MODULE),
    {X,Y+H,W,H}.

din(ID0,Chan,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 24, H = 14,
    epxy:new(ID,[{type,value},
		 {halign,center},{valign,center},
		 {x,X},{y,Y},{width,W},{height,H},
		 {children_first,false},
		 {font,[{name,"Arial"},{weight,bold},
			{size,?DIN_FONT_SIZE}]},
		 {fill,solid},{color,white},
		 {format,"~w"}
		]),
    epxy:new(ID++".border",
	     [{type,rectangle},
	      {color,black},
	      {x,-1},{y,-1},
	      {width,W+2},{height,H+2}]),
    {X,Y+H,W,H}.

ain(ID0,Chan,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 40, H = ?AIN_FONT_SIZE,
    epxy:new(ID,[{type,value},
		 {halign,center},{valign,center},
		 {x,X},{y,Y},{width,W},{height,H},
		 {children_first,false},
		 {font,[{name,"Arial"},{weight,bold},
			{size,?AIN_FONT_SIZE}]},
		 {fill,solid},{color,white},
		 {format,"~.1f"},
		 {value,0},
		 {vscale, 100/65535}
		]),
    epxy:new(ID++".border",
	     [{type,rectangle},
	      {color,black},
	      {x,-1},{y,-1},
	      {width,W+2},{height,H+2}]),
    {X,Y+H,W,H}.

aload(ID0,Chan,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 32, H = ?ALOAD_FONT_SIZE,
    epxy:new(ID,
	     [{type,value},
	      {halign,center},{valign,center},
	      {x,X},{y,Y},{width,W},{height,H},
	      {children_first,false},
	      {font,[{name,"Arial"},{weight,bold},
		     {size,?ALOAD_FONT_SIZE}]},
	      {fill,solid},{color,white},
	      {format,"~.2f"},
	      {value,0},
	      {vscale, 1/100}
	     ]),
    epxy:new(ID++".border",
	     [{type,rectangle},
	      {color,black},
	      {x,-1},{y,-1},
	      {width,W+2},{height,H+2}]),
    {X,Y+H,W,H}.
    
aout(ID0,Chan,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = ?SLIDER_WIDTH+?ONOFF_WIDTH+16, H = ?ONOFF_FONT_SIZE,

    epxy:new(ID,[{type,slider},
		 {x,X+?ONOFF_WIDTH+16},{y,Y+2},
		 {width,?SLIDER_WIDTH},{height,?SLIDER_HEIGHT},
		 {fill,solid},{color,lightBlue},
		 {min,0},{max,65535},
		 {orientation, horizontal},
		 {border,1},
		 {topimage, "$/gordon//priv/knob.png"}
		]),
    epxy:add_callback(ID,analog,?MODULE),
    ID1 = ID++".onoff",
    epxy:new(ID1,[{type,switch},
		  {halign,center},
		  {x,-(?ONOFF_WIDTH+16)},{y,-4},{width,?ONOFF_WIDTH},{height,H},
		  {shadow_x,2},{shadow_y,2},{children_first,false},
		  {font,[{name,"Arial"},{weight,bold},{size,?ONOFF_FONT_SIZE}]},
		  {fill,solid},{color,lightgray},
		  {text,"OFF"}
		 ]),
    epxy:add_callback(ID1,switch,?MODULE),
    {X,Y+H,W,H}.

pout(ID0,Chan,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = ?SLIDER_WIDTH+?ONOFF_WIDTH+16, H = ?ONOFF_FONT_SIZE,
    epxy:new(ID,[{type,slider},
		 {x,X+?ONOFF_WIDTH+16},{y,Y+2},
		 {width,?SLIDER_WIDTH},{height,?SLIDER_HEIGHT},
		 {fill,solid},{color,lightGreen},
		 {min,0},{max,65535},
		 {orientation, horizontal},
		 {border, 1},
		 {topimage, "$/gordon//priv/knob.png"}
		]),
    epxy:add_callback(ID,analog,?MODULE),
    ID1 = ID++".onoff",
    epxy:new(ID1,[{type,switch},
		  {halign,center},
		  {x,-(?ONOFF_WIDTH+16)},{y,-4},{width,?ONOFF_WIDTH},{height,H},
		  {shadow_x,2},{shadow_y,2},{children_first,false},
		  {font,[{name,"Arial"},{weight,bold},{size,?ONOFF_FONT_SIZE}]},
		  {fill,solid},{color,lightgray},
		  {text,"OFF"}
		 ]),
    epxy:add_callback(ID1,switch,?MODULE),
    {X,Y+H,W,H}.

group_rectangle(ID,Text,X,Y,W,H,Status) ->
    epxy:new(ID,
	     [{type,rectangle},
	      {hidden,Status},{disabled,Status},
	      {children_first, false},
	      {color,black},{x,X},{y,Y},
	      {width,W},{height,H}]),
    epxy:new(ID++".tag",
	     [{type,text},
	      {font,[{name,"Arial"},{slant,roman},
		     {size,?GROUP_FONT_SIZE}]},
	      {font_color, black},
	      {text,Text},
	      {color,white},{fill,solid},
	      {halign,left},
	      {x,5},{y,-5},
	      {height,10}  %% width,25
	     ]).

tagged_text(ID, TagText, X, Y) ->
    XOffs = length(TagText)*10,
    W = 8*10,
    H = 12,
    epxy:new(ID,
	     [{type,text},
	      {font,[{name,"Arial"},{slant,roman},
		     {size,?GROUP_FONT_SIZE}]},
	      {font_color, black},
	      {text,""},
	      {color,white},{fill,solid},
	      {halign,left},
	      {x,X+XOffs},{y,Y},
	      {height,10}, {width,W}
	     ]),
    epxy:new(ID++".border",
	     [{type,rectangle},
	      {color,black},
	      {x,-1},{y,-1},
	      {width,W+2},{height,H+2}]),
    epxy:new(ID++".tag",
	     [{type,text},
	      {font,[{name,"Arial"},{slant,roman},
		     {size,?GROUP_FONT_SIZE}]},
	      {font_color, black},
	      {text,TagText},
	      {color,white},{fill,solid},
	      {halign,left},
	      {x,-XOffs},{y,0},
	      {height,10}
	     ]),
    {X,Y+H, XOffs + W, H}.

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
	?ma_ccs_initiate_download_request(N,E,S,_Index,_SubInd,Data) when 
	      E =:= 1->
	    _Value = sdo_value(S,N,Data),
	    ?dbg("sdo_rx: CobID=~s, SET index=~w, si=~w, value=~w\n", 
		 [integer_to_list(CobID,16), _Index,_SubInd,_Value]);

	?ma_ccs_initiate_upload_request(_Index,_SubInd) ->
	    ?dbg("sdo_rx: CobID=~s, GET index=~w, si=~w\n", 
		 [integer_to_list(CobID,16),_Index,_SubInd]);

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

%% reply from node
sdo_tx(CobId,Bin,State) ->  
    case Bin of
	?ma_scs_initiate_download_response(Index,SubInd) ->
	    ?dbg("sdo_tx: CobId=~s, SET RESP index=~w, si=~w\n",
		 [integer_to_list(CobId,16),Index,SubInd]),
	    case State#state.sdo_request of
		{Index,SubInd} ->
		    %% SDO request ok
		    {noreply,State#state { sdo_request = undefined }};
		undefined ->
		    {noreply,State}
	    end;

	?ma_scs_initiate_upload_response(N,E,S,Index,SubInd,Data) when
	      E =:= 1 ->
	    Value = sdo_value(S,N,Data),
	    ?dbg("sdo_tx: CobId=~s, GET RESP index=~w, si=~w, value=~w\n", 
		 [integer_to_list(CobId,16),Index,SubInd,Value]),
	    State1 = set_value_by_cobid(CobId,Index,SubInd,Value,State),
	    {noreply,State1};

	?ma_abort_transfer(Index,SubInd,Code) ->
	    ?dbg("sdo_tx: CobId=~s, ABORT index=~w, si=~w, code=~w\n",
		 [integer_to_list(CobId,16),Index,SubInd,Code]),
	    case State#state.sdo_request of
		{Index,SubInd} ->
		    {noreply,State#state { sdo_request = undefined,
					   sdo_error = Code }};
		_ ->
		    if Code =:= ?ABORT_NO_SUCH_OBJECT,
		       Index =:= ?INDEX_BOOT_VSN ->
			    State1 = set_by_cobid(CobId,status,up,State),
			    {noreply,State1};
		       true ->
			    {noreply,State}
		    end
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
    io:format("hold=~w, Nodes = ~p\n", [State#state.hold_mode, Nodes]),
    if State#state.hold_mode ->
	    XCobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
	    WDT = 0,
	    co_sdo_cli:send_sdo_set(XCobId, ?INDEX_UBOOT_HOLD, 0, WDT),
	    State1 = State#state { nodes=Nodes, 
				   sdo_request={?INDEX_UBOOT_HOLD, 0},
				   hold_mode=false },
	    State2 = refresh_node_state(State1),
	    {noreply, State2};
       true ->
	    State1 = State#state { nodes=Nodes },
	    State2 = refresh_node_state(State1),
	    {noreply,State2}
    end.


node_started(_CobId, Serial, State) ->
    ?dbg("Node ~6.16.0B started\n", [Serial]),
    Nodes = set_status_by_serial(Serial, up, State#state.nodes),
    spawn(
      fun() ->
	      XCobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
	      ?dbg("XCobId = ~8.16.0B\n", [XCobId]),
	      co_sdo_cli:send_sdo_get(XCobId, ?INDEX_ID, 0),
	      co_sdo_cli:send_sdo_get(XCobId, ?IX_IDENTITY_OBJECT, ?SI_IDENTITY_PRODUCT)
	      %% ...
      end),
    State1 = State#state { nodes=Nodes },
    State2 = refresh_node_state(State1),
    {noreply,State2}.

node_running(_CobId, Serial, State) ->
    ?dbg("Node ~6.16.0B running\n", [Serial]),
    case find_node_by_serial(Serial, State#state.nodes) of
	false ->
	    spawn(
	      fun() ->
		      XCobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
		      ?dbg("XCobId = ~8.16.0B\n", [XCobId]),
		      co_sdo_cli:send_sdo_get(XCobId, ?INDEX_ID, 0),
		      co_sdo_cli:send_sdo_get(XCobId, ?IX_IDENTITY_OBJECT,
					      ?SI_IDENTITY_PRODUCT),
		      co_sdo_cli:send_sdo_get(XCobId, ?INDEX_BOOT_APP_VSN, 0),
		      co_sdo_cli:send_sdo_get(XCobId, ?INDEX_BOOT_VSN, 0),
		      if XCobId =:= State#state.selected_eff ->
			      send_pdo1_tx(0, ?MSG_REFRESH, 0, 0);
			 true ->
			      ok
		      end
	      end),
	    {noreply, State};
	_Node ->
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
			
			%% load values
			33 -> set_value("pds.aload.e33", Value);
			34 -> set_value("pds.aload.e34", Value);
			35 -> set_value("pds.aload.e35", Value);
			36 -> set_value("pds.aload.e36", Value);
		        37 -> set_value("pds.aload.e37", Value);
			38 -> set_value("pds.aload.e38", Value);
			39 -> set_value("pds.aload.e39", Value);
			40 -> set_value("pds.aload.e40", Value);
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
	    _OnOff = (Value bsr 24) band 16#ff,
	    _OutSt = (Value bsr 16) band 16#ff,
	    _Duty  = Value band 16#ffff,
	    ?dbg("MSG_OUTPUT_STATE: [~s], si=~w,on=~w,state=~w,duty=~w\n", 
		 [State#state.selected_id,Si,_OnOff,_OutSt,_Duty]),
	    case Si of
		%% bridge zone: Pout=1-4, Aout =5-6, Dout=7-10
		%% ioZone:      Dout=1-8
		%% powerZone:   Pout=1-8  Ain=1-8,  Aload=33-40
		_ -> ok
	    end;
	_ ->
	    ignore
    end,
    {noreply,State}.

switch_state(ID,0) ->
    epxy:set(ID,[{color,lightgray},{text,"OFF"}]);
switch_state(ID,_) ->
    epxy:set(ID,[{color,green},{text,"ON"}]).

set_value(ID, Value) ->
    epxy:set(ID,[{value,Value}]).

set_status_by_serial(Serial, Status, Ns) ->
    case take_node_by_serial(Serial, Ns) of
	{value,N=#{ pos := Pos },Ns1} ->
	    set_text(status,Pos,Status),
	    [ N#{ status => Status } | Ns1];
	false ->
	    Pos = length(Ns)+1,
	    set_text(status,Pos,Status),
	    set_text(serial,Pos,Serial),
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
	    Major = (Value bsr 8) band 16#ff,
	    Minor = Value band 16#ff,
	    State1 = set_by_cobid(CobId,vsn,{Major,Minor},State),
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
	?INDEX_BOOT_APP_VSN ->
	    set_by_cobid(CobId,app_vsn,Value,State);
	?INDEX_BOOT_VSN ->
	    set_by_cobid(CobId,status,boot,State);
	_ ->
	    State
    end.

set_by_cobid(CobID,Key,Value,State) ->
    State1 = set_by_cobid_(CobID,Key,Value,State),
    refresh_node_state(State1).

set_by_cobid_(CobID,Key,Value,State) ->
    io:format("set_by_cobid: key=~p, value=~p\n", [Key,Value]),
    case ?is_cobid_extended(CobID) of
	true ->
	    Serial = ?XNODE_ID(CobID),
	    Ns = State#state.nodes,
	    case take_node_by_serial(Serial, Ns) of
		false ->
		    Pos = length(Ns)+1,
		    set_text(serial,Pos,Serial),
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
		    set_text(id,Pos,ID),
		    set_text(Key,Pos,Value),
		    N = #{ pos=>Pos, id=>ID, Key=>Value },
		    State#state { nodes=[N|Ns]};
		{value,N=#{ pos := Pos},Ns1} ->
		    set_text(Key,Pos,Value),
		    N1 = N#{ Key => Value },
		    State#state { nodes=[N1|Ns1]}
	    end
    end.

%% set text value in nodes table
%% nodes.r<pos>.<key>
set_text(Key,Pos,Value) ->
    case is_node_item(Key) of
	true ->
	    ID = "nodes.r"++integer_to_list(Pos)++"."++atom_to_list(Key),
	    ?dbg("set_text: id=~s, value=~p\n", [ID,Value]),
	    epxy:set(ID,[{text,format_value(Key,Value)}]);
	false ->
	    ok
    end.

format_value(serial,Value) -> 
    tl(integer_to_list(16#1000000+Value, 16));
format_value(vsn,{Major,Minor}) ->
    integer_to_list(Major)++"."++integer_to_list(Minor);
format_value(app_vsn,Value) ->
    case Value of
	16#00000000 -> "zero";
	16#2F5EBD7A -> "empty";
	16#FFFFFFFF -> "none";
	_ ->
	    {{Year,Mon,Day},{_H,_M,_S}} = 
		calendar:gregorian_seconds_to_datetime(Value+62167219200),
	    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", 
					[Year,Mon,Day]))
    end;
format_value(_Key,Int) when is_integer(Int) -> integer_to_list(Int);
format_value(_Key,Text) when is_atom(Text) -> Text;
format_value(_Key,Text) when is_list(Text) -> Text.


is_node_item(serial) -> true;
is_node_item(id) -> true;
is_node_item(product) -> true;
is_node_item(vsn) -> true;
is_node_item(status) -> true;
is_node_item(_) -> false.



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


find_node_by_pos(Pos, Ns) ->  find_node_by_key(pos, Pos, Ns).
find_node_by_serial(Serial, Ns) -> find_node_by_key(serial, Serial, Ns).

%% find node with key and Value
find_node_by_key(Key,Value,[Node|Ns]) ->
    case Node of
	#{ Key := Value} ->
	    Node;
	_ ->
	    find_node_by_key(Key,Value,Ns)
    end;
find_node_by_key(_Key, _Value, []) ->
    false.

load_firmware() ->
    Dir = code:priv_dir(gordon),
    case file:list_dir(Dir) of
	{ok,L} ->
	    load_firm(Dir, L, []);
	Error ->
	    ?warn("unable to read priv dir ~p\n", [Error]),
	    []
    end.

load_firm(Dir,[F|Fs],Acc) ->
    File = filename:join(Dir,F),
    try gordon_uapp:load(File) of
	{ok, Uapp} ->
	    load_firm(Dir,Fs,Uapp++Acc);
	_Error ->
	    load_firm_hex(Dir,Fs,File,Acc)
    catch
	error:_ ->
	    load_firm_hex(Dir,Fs,File,Acc)
    end;
load_firm(_Dir,[],Acc) ->
    Acc.

load_firm_hex(Dir,Fs,File,Acc) ->
    try elpcisp_ihex:load(File) of
	{ok,Hex} ->
	    load_firm(Dir,Fs,[{ihex,Hex}|Acc]);
	_Error ->
	    load_firm(Dir,Fs,Acc)
    catch
	error:_ ->
	    load_firm(Dir,Fs,Acc)
    end.

firmware_size([{banks,_Match,B}|_]) ->
    banks_size(B);
firmware_size(SegmentList=[{Addr,Seg}|_]) when is_integer(Addr),
					       is_binary(Seg) ->
    banks_size(SegmentList);
firmware_size([]) ->
    0.

banks_size(Bs) ->
    lists:sum([byte_size(Segment) || {_Addr,Segment} <- Bs]).

    
