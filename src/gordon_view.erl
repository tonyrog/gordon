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
-include_lib("elpcisp/src/elpcisp.hrl").
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
	  selected_tab,    %% "nodes" | "uarts" | undefined
	  selected_pos,    %% Selected row | undefined
	  selected_eff=0,  %% extended canid of selected node
	  selected_sff=0,  %% short canid of selected node
	  sdo_request,     %% current outstanding sdo_request
	  sdo_error,       %% last sdo_error
	  row_height = 1,  %% height of row selection area
	  firmware = [],   %% list of available firmware for upgrade
	  nodes = [],      %% list of node maps
	  uarts = [],      %% list of uarts
	  elpc,            %% Elpc options
	  dev_type,        %% Elpc devict type
	  dev_info = []    %% Elpc last scanned info
	}).

-define(TEXT_CELL_FONT_SIZE, 18).
-define(DIN_FONT_SIZE, 14).
-define(DOUT_FONT_SIZE, 14).
-define(AIN_FONT_SIZE, 14).
-define(ALOAD_FONT_SIZE, 14).
-define(BUTTON_FONT_SIZE, 16).
-define(ONOFF_FONT_SIZE, 14).
-define(ONOFF_ROUND_WH, 4).
-define(GROUP_FONT_SIZE, 12).
-define(SLIDER_WIDTH,  100).
-define(SLIDER_HEIGHT, 8).
-define(ONOFF_WIDTH, 40).
-define(BUTTON_WIDTH, 72).
-define(BUTTON_HEIGHT, 18).
-define(BUTTON_ROUND_WH, 4).

-define(NUM_TABLE_NODES, 16).
-define(NUM_TABLE_UARTS, 2).

-define(TYPE_NONE,       16#00).
-define(TYPE_BACKLIGHT,  16#81).
-define(TYPE_BLOCK,       16#83).
-define(TYPE_ALARM,       16#84).
-define(TYPE_ONOFF,       16#01).
-define(TYPE_DIMMER,      16#02).
-define(TYPE_INTERVAL,    16#03).
-define(TYPE_MOMENT,      16#04).
-define(TYPE_PULSE,       16#05).
-define(TYPE_EXTERNAL,    16#06).
-define(TYPE_ALWAYSON,    16#07).
-define(TYPE_OUT_ACTIVE,  16#09).
-define(TYPE_OUT_ALARM,   16#0A).
-define(TYPE_OUT_BACKLIGHT, 16#0B).
-define(TYPE_DMX,         16#08).
-define(TYPE_INPUT,       16#FF).

%%-define(dbg(F,A), io:format((F),(A))).
-define(dbg(F,A), ok).
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
    (catch error_logger:tty(false)),
    application:start(lager),
    application:load(gordon),
    application:load(can),
    application:set_env(can, wakeup, true),
    application:start(can),
    can_udp:start(),
    epxy:start_link([{width,800}, {height,480}]),
    gen_server:start({local, ?SERVER}, ?MODULE, [{width,800}, {height,480}],
		     []).

%% start rpi with touch screen
start_rpi() ->
    (catch error_logger:tty(false)),
    application:start(lager),
    ok = application:load(epx),
    application:set_env(epx, backend, "fb"),
    application:set_env(epx, pixel_format, 'argb/little'),
    application:set_env(epx, input_mouse_device, "/dev/input/event0"),
    application:load(gordon),
    application:load(can),
    application:set_env(can, wakeup, true),
    application:start(can),
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
    XOffs = 8,
    YOffs = 8,
    {_X1,Y1,RH1} = node_table(XOffs,YOffs,Width div 2, Height),
    {_X2,_Y2,_RH2} = uart_table(XOffs,Y1+2*YOffs,Width div 2, Height),

    %% define various layouts
    X = Width div 2,
    Y = 10,
    W = Width div 3,
    H = Height - 32,
    bridgeZone(X,Y,W,H),
    ioZone(X,Y,W,H),
    powerZone(X,Y,W,H),
    uBoot(X,Y,W,H),
    lpcBoot(X,Y,W,H),

    can_router:attach(),

    %% request response from all nodes
    send_pdo1_tx(0, ?MSG_ECHO_REQUEST, 0, 0),

    Firmware = load_firmware(),

    Elpc = case application:get_env(gordon, elpc) of
	       {ok,ElpcEnv} -> ElpcEnv;
	       undefined -> undefined
	   end,
    UARTS = set_elpc_row(Elpc),
    {ok, #state{ row_height = RH1, firmware = Firmware, 
		 nodes = [], uarts = UARTS, elpc = Elpc }}.

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
    State1 = deselect_row(State#state.selected_tab,
			  State#state.selected_pos,State),
    ?dbg("deselect row=~w\n", [State#state.selected_pos]),
    Tab = "nodes",
    case selected_id(Tab,Pos,State1) of
	undefined ->
	    {noreply, State1};
	_SID ->
	    Node = find_node_by_pos(Tab,Pos,State#state.nodes),
	    Serial = maps:get(serial,Node,0),
	    EFF = case Serial of
		      0 -> 0;
		      N -> ?XCOB_ID(?PDO1_TX,N)
		  end,
	    SFF = case maps:get(id,Node,0) of
		      0 -> 0;
		      M -> ?COB_ID(?PDO1_TX,M)
		  end,
	    State2 = State1#state { selected_tab = Tab,
				    selected_pos = Pos,
				    selected_eff = EFF,
				    selected_sff = SFF
				  },
	    highlight_row(Tab,Pos),
	    show_pos(Tab,Pos,State2),
	    State3 = refresh_node_state(Node, State2),
	    ?dbg("select row=~w, eff=~8.16.0B, sff=~3.16.0B id=~s\n",
		 [Pos, EFF, SFF, _SID]),
	    send_pdo1_tx(0, ?MSG_REFRESH, 0, 0),
	    {noreply, State3}
    end;
%% handle select in uart list
handle_info({select,"uarts.r"++RTxt,[{press,1},{x,_X},{y,_Y}|_]},State) ->
    Pos = list_to_integer(RTxt),
    State1 = deselect_row(State#state.selected_tab,
			  State#state.selected_pos,State),
    ?dbg("deselect row=~w\n", [State#state.selected_pos]),
    Tab = "uarts",
    case selected_id(Tab,Pos,State1) of
	undefined ->
	    {noreply, State1};
	"lpc" ->
	    highlight_row(Tab,Pos),
	    State2 = State1#state { selected_tab = Tab,
				    selected_pos = Pos,
				    selected_eff = undefined,
				    selected_sff =  undefined
				    },
	    show_pos(Tab,Pos,State2),
	    {noreply, State2};
	_ ->
	    {noreply, State1}
    end;

handle_info({select,_ID,[{press,0},{x,_},{y,_}|_]},State) ->
    %% ignore mouse release in node selection
    {noreply, State};

handle_info({node_flashed,ID,Nid,Status,_Result},State) ->
    State1 = set_by_cobid(Nid,status,Status,State),
    epxy:set(ID,[{value,0.0}]),
    %% if Result == error then signal in window
    {noreply, State1};

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
    case find_node_by_pos(State#state.selected_tab,
			  State#state.selected_pos,
			  State#state.nodes) of
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
    {noreply,action_sdo(State,boot,?INDEX_UBOOT_GO,0,WDT)};
%%
%% locate firmware image given selected product
%% check that firmware is an upgrade or first firmware
%% must be an uapp.
%% start process disable all input (except flash dialog)
%% show progress remove flash dialog and enable input
%%
handle_info({button,[_,_,_|".upgrade"],[{value,1}]},State) ->
    case find_node_by_pos(State#state.selected_tab,
			  State#state.selected_pos,
			  State#state.nodes) of
	false -> 
	    {noreply,State};
	Node ->
	    case match_uapp_firmware(Node, State) of
		false ->
		    {noreply,State};
		{Version,Bs,_UApp} ->
		    State1 = flash_node(Node,Version,Bs,State),
		    {noreply,State1}
	    end
    end;

%% reset the node
handle_info({button,[_,_,_|".reset"],[{value,1}]},State) ->
    %% send a reset and set hold mode
    case find_node_by_pos(State#state.selected_tab,
			  State#state.selected_pos,
			  State#state.nodes) of
	false ->
	    {noreply,State};
	Node ->
	    Serial = maps:get(serial,Node,0),
	    send_pdo1_tx(0, ?MSG_RESET, 0, Serial),
	    {noreply,State#state{hold_mode = false }}
    end;

handle_info({button,[_,_,_|".setup"],[{value,1}]},State) ->
    case find_node_by_pos(State#state.selected_tab,
			  State#state.selected_pos,
			  State#state.nodes) of
	false ->
	    {noreply,State};
	Node ->
	    Status = maps:get(status,Node,undefined),
	    Serial = maps:get(serial,Node,0),
	    if Status =:= up ->
		    XCobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
		    case selected_id(State) of
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

handle_info({button,[_,_,_|".lpc_scan"],[{value,1}]},State) ->
    if State#state.uart =:= undefined, 
       is_list(State#state.elpc) ->
	    %% fixme open using correct device if multiple!
	    Elpc = State#state.elpc,
	    ElpcDevice = proplists:get_value(device,Elpc),
	    ElpcBaud   = proplists:get_value(baud,Elpc,38400),
	    ElpcControl = proplists:get_value(control,Elpc,false),
	    ElpcControlSwap = proplists:get_value(control_swap,Elpc,false),
	    ElpcControlInv = proplists:get_value(control_inv,Elpc,false),
	    put(control, ElpcControl),
	    put(control_swap, ElpcControlSwap),
	    put(control_inv, ElpcControlInv),
	    set_elpc_status(sync),
	    case elpcisp:open(ElpcDevice, ElpcBaud) of
		{ok,U} ->
		    case elpcisp:sync(U, 3) of
			{ok,_} ->
			    DevType = case elpcisp:read_device_type(U) of
					  {ok,DT} -> DT;
					  _ -> undefined
				      end,
			    Info = elpcisp:info(U),
			    set_elpc_info(Info),
			    set_elpc_status(idle),
			    {noreply, State#state { uart = U,
						    dev_info = Info,
						    dev_type = DevType }};
			_Error ->
			    set_elpc_status(error),
			    set_elpc_info([]),
			    {noreply, State}
		    end;
		_Error ->
		    set_elpc_status(error),
		    set_elpc_info([]),
		    {noreply, State}
	    end;
       is_port(State#state.uart) ->
	    DevType = case elpcisp:read_device_type(State#state.uart) of
			  {ok,DT} -> DT;
			  _ -> undefined
		      end,
	    Info = elpcisp:info(State#state.uart),
	    set_elpc_info(Info),
	    {noreply, State#state { dev_info = Info, dev_type = DevType }};
       true ->
	    set_elpc_info([]),
	    {noreply, State#state { dev_info = [], dev_type = undefined }}
    end;

handle_info({button,[_,_,_|".lpc_flash"],[{value,1}]},State) ->
    case lists:keyfind(ihex,1,State#state.firmware) of
	{ihex,Firmware} when State#state.uart =/= undefined ->
	    case elpcisp:unlock(State#state.uart) of
		{ok,_} ->
		    case flash_firmware(State#state.uart, Firmware, 
					State#state.dev_type) of
			ok ->
			    set_elpc_status(ok);
			_Error ->
			    set_elpc_status(error)
		    end;
		_Error ->
		    set_elpc_status(error)
	    end,
	    {noreply, State};
	_ ->
	    {noreply, State}
    end;

handle_info({button,[_,_,_|".lpc_go"],[{value,1}]},State) ->
    if State#state.uart =:= undefined ->
	    {noreply, State};
       true ->
	    case elpcisp:go(State#state.uart, 0) of
		{ok,_} -> set_elpc_status(idle);
		_Error -> set_elpc_status(error)
	    end,
	    {noreply, State}
    end;
handle_info({button,[_,_,_|".lpc_reset"],[{value,1}]},State) ->
    if State#state.uart =:= undefined ->
	    {noreply, State};
       true ->
	    case elpcisp:reset(State#state.uart) of
		{ok,_} -> set_elpc_status(idle);
		_Error -> set_elpc_status(error)
	    end,
	    {noreply, State}
    end;
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

set_elpc_row(undefined) ->
    [];
set_elpc_row(Elpc) ->
    ElpcDevice = proplists:get_value(device,Elpc),
    ElpcBaud   = proplists:get_value(baud,Elpc,38400),
    ElpcControl = proplists:get_value(control,Elpc,false),
    ElpcControlSwap = proplists:get_value(control_swap,Elpc,false),
    ElpcControlInv = proplists:get_value(control_inv,Elpc,false),
    %%
    {Manuf0,Prod0,Serial0} = get_name_info(ElpcDevice),
    Manuf = trunc_text(Manuf0,12),
    _Prod  = trunc_text(Prod0,12),
    Serial = trunc_text(Serial0,8),
    set_uart_text(device,1,Manuf++" "++Serial),
    set_uart_text(baud,1,ElpcBaud),
    set_uart_text(control,1,uint1(ElpcControl)),
    set_uart_text(swap,1,uint1(ElpcControlSwap)),
    set_uart_text(invert,1,uint1(ElpcControlInv)),
    set_elpc_status(idle),
    [ #{ pos => 1, device => ElpcDevice, baud => ElpcBaud,
	 control => ElpcControl, control_swap => ElpcControlSwap,
	 control_inv => ElpcControlInv, status => scan } ].

uint1(true) -> 1;
uint1(false) -> 0.

trunc_text(Text,MaxLen) ->
    string:substr(Text, 1, MaxLen).

get_name_info("/dev/serial/by-id/usb-"++Name) ->
    %% remove port info part
    Name1 = case string:chr(Name, $-) of
		0 -> Name;
		I -> string:substr(Name,1,I-1)
	    end,
    %% extract serial from name
    Ts = string:tokens(Name1,"_"),
    {Serial,Ts1} = case lists:reverse(Ts) of
		       [Ser|RTs2] -> {Ser,lists:reverse(RTs2)};
		       _ -> {"", Ts}
		   end,
    %% rest is normally manuf and product
    case Ts1 of
	["Silicon","Labs" | Prod] ->
	    {"Silicon_Labs", string:join(Prod,"_"), Serial};
	[Manuf | Prod] ->
	    {Manuf, string:join(Prod,"_"), Serial}
    end;
get_name_info("/dev/"++Name) ->
    {"","USB",Name}.


set_elpc_status(Status) ->
    set_uart_text(status,1,Status).

set_elpc_info(Info) ->
    Vsn = proplists:get_value(version, Info),
    Product = proplists:get_value(product, Info),
    FlashSize = proplists:get_value(flashSize, Info),
    RamSize = proplists:get_value(ramSize, Info),
    FlashSectors = proplists:get_value(flashSectors, Info),
    MaxCopySize = proplists:get_value(maxCopySize, Info),
    Variant = proplists:get_value(variant, Info),
    epxy:set("lpc.vsn", [{text,format_value(vsn,Vsn)}]),
    epxy:set("lpc.product", [{text,Product}]),
    epxy:set("lpc.flashSize", [{text,format_value(flashSize,FlashSize)}]),
    epxy:set("lpc.ramSize", [{text,format_value(ramSize,RamSize)}]),
    epxy:set("lpc.flashSectors", [{text,format_value(flashSectors,FlashSectors)}]),
    epxy:set("lpc.maxCopySize", [{text,format_value(maxCopySize,MaxCopySize)}]),
    epxy:set("lpc.variant", [{text,format_value(variant,Variant)}]),
    ok.

%% FIXME: update button status according to state
%%  upgrade iff state=boot and firmware is available and correct
%%  factory/save/hold iff state=up
%%  go state=boot
%%  setup state=up
%%
refresh_node_state(State) ->
    case find_node_by_pos(State#state.selected_tab,
			  State#state.selected_pos,
			  State#state.nodes) of
	false -> State;
	Node -> refresh_node_state(Node,State)
    end.

refresh_node_state(Node, State) ->
    refresh_node_state(selected_id(State), Node, State).

refresh_node_state(undefined, _Node, State) ->
    State;
refresh_node_state(SID, Node, State) ->
    AppVsn = maps:get(app_vsn,Node,0),
    epxy:set(SID++".app_vsn", [{text,format_value(app_vsn,AppVsn)}]),
    UAppMatch =  match_uapp_firmware(Node, State),
    case UAppMatch of
	false ->
	    epxy:set(SID++".uapp_vsn", [{text,""}]);
	{Version,_Bs,_UApp} ->
	    VersText = format_value(app_vsn,Version),
	    epxy:set(SID++".uapp_vsn", [{text,VersText}])
    end,
    case maps:get(status,Node,undefined) of
	undefined ->
	    State;
	flash ->
	    disable_buttons(SID,["go","upgrade"]),
	    enable_buttons(SID,["reset"]),
	    State;
	boot ->
	    enable_buttons(SID,["reset","go"]),
	    case UAppMatch of
		false ->
		    disable_buttons(SID,["upgrade"]);
		_Match ->
		    enable_buttons(SID,["upgrade"])
	    end,
	    State;
	up ->
	    enable_buttons(SID, ["hold","reset"]),
	    enable_buttons(SID,["setup","factory","save","restore"]),
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
	      [{Product,Vsn,AppVersion}]),
    match_uapp_firmware_(Product,Vsn,AppVersion,State#state.firmware).

match_uapp_firmware_(Product,Vsn={Major,Minor},AppVersion,
		     [UApp={uapp,{Product,_Variant,Major,Minor},Banks}|Fs]) ->
    case match_uapp_vsn(AppVersion, Banks) of
	false -> match_uapp_firmware_(Product,Vsn,AppVersion,Fs);
	{true,{Version,Bs}} -> {Version,Bs,UApp}
    end;
match_uapp_firmware_(Product,Vsn,AppVersion,[_UApp|Fs]) ->
    match_uapp_firmware_(Product,Vsn,AppVersion,Fs);
match_uapp_firmware_(_Product,_Vsn,_AppVersion,[]) ->
    false.

match_uapp_vsn(AppVersion, [{banks,Match,Bs}|Banks]) ->
    io:format("Match ~p with ~p\n", [AppVersion,Match]),
    case proplists:get_value(version,Match) of
	undefined -> 
	    match_uapp_vsn(AppVersion,Banks);
	Version ->
	    io:format("Node AppVersion=~p, File Version=~p\n",
		      [AppVersion,Version]),
	    if AppVersion =:= undefined;
	       Version > AppVersion -> {true,{Version,Bs}};
	       true -> match_uapp_vsn(AppVersion, Banks)
	    end
    end;
match_uapp_vsn(_AppVersion, []) ->
    false.
    
action_sdo(State, Status, Index, Si, Value) ->
    case find_node_by_pos(State#state.selected_tab,
			  State#state.selected_pos,
			  State#state.nodes) of
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

-define(OUTPUT_FLAG_OUTACT,      16#0020).
-define(OUTPUT_FLAG_ANLOAD,      16#0800).
-define(OUTPUT_FLAG_VALUE,       16#1000).

-define(INPUT_ACTIVE,            16#0001).
-define(INPUT_ANALOG,            16#0002).
-define(INPUT_DIGITAL,           16#0004).
-define(INPUT_ENCODER,           16#0008).

%%
%% powerZone test setup
%% set output-type:1-8 dimmer
%% set output-max-step:1-8 255
%% set output-flags:1-8 anload,outact,value
%% set input-flags:32 active
%% set input-out:32 1..8
%%

powerZone_setup(Nid,State) ->
    OutputFlags = ?OUTPUT_FLAG_ANLOAD bor 
	          ?OUTPUT_FLAG_OUTACT bor
	          ?OUTPUT_FLAG_VALUE,
    co_sdo_cli:set_batch([{Nid,?INDEX_OUTPUT_TYPE,{1,8},?TYPE_DIMMER},
			  {Nid,?INDEX_OUTPUT_STEPMAX,{1,8},255},
			  {Nid,?INDEX_OUTPUT_FLAGS,{1,8}, OutputFlags},
			  {Nid,?INDEX_INPUT_FLAGS,32,?INPUT_ACTIVE},
			  {Nid,?INDEX_INPUT_OUT,32,16#ff}
			 ],1000),
    {noreply,State}.

%%
%% bridgeZone test setup
%% set output-type:1-6 dimmer
%% set output-max-step:1-6 255
%% set output-type:7-10 onoff
%% set output-flags:1-10 outact,value
%% set input-flags:32 active
%% set input-out:32 1..10
%%
bridgeZone_setup(Nid, State) ->
    OutputFlags = ?OUTPUT_FLAG_OUTACT bor
	          ?OUTPUT_FLAG_VALUE,
    co_sdo_cli:set_batch([{Nid,?INDEX_OUTPUT_TYPE,{1,6},?TYPE_DIMMER},
			  {Nid,?INDEX_OUTPUT_TYPE,{7,10},?TYPE_ONOFF},
			  {Nid,?INDEX_OUTPUT_STEPMAX,{1,6},255},
			  {Nid,?INDEX_OUTPUT_FLAGS,{1,10},OutputFlags},
			  {Nid,?INDEX_INPUT_FLAGS,32,?INPUT_ACTIVE},
			  {Nid,?INDEX_INPUT_OUT,32,16#3ff}
			 ],1000),
    {noreply,State}.

%%
%% ioZone test setup
%% set output-type:1-8 onoff
%% set output-flags:1-8 outact,value
%% set input-flags:32 active
%% set input-out:32 1..8
%%
ioZone_setup(Nid,State) ->
    OutputFlags = ?OUTPUT_FLAG_OUTACT bor
	          ?OUTPUT_FLAG_VALUE,
    co_sdo_cli:set_batch([{Nid,?INDEX_OUTPUT_TYPE,{1,8},?TYPE_ONOFF},
			  {Nid,?INDEX_OUTPUT_FLAGS,{1,8},OutputFlags},
			  {Nid,?INDEX_INPUT_FLAGS,32,?INPUT_ACTIVE},
			  {Nid,?INDEX_INPUT_OUT,32,16#ff}
			 ],1000),
    {noreply,State}.

%%
%% controlZone test setup
%%
controlZone_setup(_Nid,State) ->
    {noreply,State}.

%%
%% Event callback from epxy
%%
event(Signal,ID,Env) ->
    %% just send it as info message to access
    ?dbg("Got event callback ~p\n", [{Signal,ID,Env}]),
    ?SERVER ! {Signal,ID,Env}.

deselect_row(undefined,_Pos, State) -> State;
deselect_row(_Tab,undefined, State) -> State;
deselect_row(Tab,Pos,State) ->
    case selected_id(Tab,Pos,State) of
	undefined ->
	    ok;
	SID ->
	    RowID = Tab++".r"++integer_to_list(Pos),
	    io:format("HIDE (row) id=~p\n", [SID]),
	    epxy:set(SID,[{hidden,all},{disabled,all}]),
	    epxy:set(RowID,[{hidden,true}])
    end,
    State#state { selected_tab=undefined, selected_pos=undefined }.

%% show the row 
highlight_row(Table,Row) ->
    RowID = Table++".r"++integer_to_list(Row),
    epxy:set(RowID,[{hidden,false}]).

hide_if_selected(Serial,State) ->
    case is_selected_by_serial(Serial,State) of
	true ->
	    hide(State);
	false ->
	    ok
    end.

hide(State) ->
    hide_pos(State#state.selected_tab,State#state.selected_pos, State).
hide_pos(Tab,Pos,State) ->
    case selected_id(Tab,Pos,State) of
	undefined ->
	    ok;
	SID ->
	    io:format("HIDE id=~p\n", [SID]),
	    epxy:set(SID,[{hidden,all},{disabled,all}])
    end.

show_if_selected(Serial,State) ->
    case is_selected_by_serial(Serial,State) of
	true ->
	    show(State);
	false ->
	    ok
    end.

show(State) ->
    show_pos(State#state.selected_tab,State#state.selected_pos, State).
show_pos(Tab,Pos, State) ->
    case selected_id(Tab,Pos,State) of
	undefined ->
	    ok;
	SID ->
	    io:format("SHOW id=~p\n", [SID]),
	    epxy:set(SID,[{hidden,false},{disabled,false}])
    end.


%% Node table
%% +------+---+----------+---+--------+
%% |Serial| ID|Product   |Vsn| Status |
%% +------+---+----------+---+--------+
%% |801001|100|bridgeZone|2.3|  Boot  |
%% +------+---+----------+---+--------+
%%
%% +------+---+----------+---+--------+
%% |USB1  |   |          |   |  scan  |
%% +------+---+----------+---+--------+
%%

node_table(X,Y,W,_H) ->
    {TxW,TxH} = text_cell_dimension(),
    NRows = ?NUM_TABLE_NODES,
    ID = "nodes",
    TabX = node_table_header(ID,TxW,TxH,W),
    TabWidth = TabX - X,
    %% create parent after header, order only important at the time of draw
    table(ID,X,Y,TabWidth,NRows*TxH),
    Xs = [table_row(ID,I,TxW,TxH,W)|| I <- lists:seq(1,NRows)],
    X1 = lists:max(Xs),
    Y1 = NRows*TxH + TxH,
    {X1,Y1,TxH}.

%% install table header return next X location
node_table_header(Parent,TxW,TxH,_W) ->
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

%% Uart table
%% +--------------+------+-+-+-+------+
%% |Device        | Baud |C|S|I|Satus |
%% +------+---+----------+-+-+-+------+
%% |/dev/ttyUSB1  |38400 |1|0|0| Scan |
%% +------+---+----------+-+-+-+------+
%%

uart_table(X,Y,W,_H) ->
    {TxW,TxH} = text_cell_dimension(),
    NRows = ?NUM_TABLE_UARTS,
    ID = "uarts",
    TabX = uart_table_header(ID,TxW,TxH,W),
    TabWidth = TabX - X,
    %% create parent after header, order only important at the time of draw
    table(ID,X,Y,TabWidth,NRows*TxH),
    Xs = [uart_table_row(ID,I,TxW,TxH,W) || I <- lists:seq(1,NRows)],
    X1 = lists:max(Xs),
    Y1 = NRows*TxH + TxH,
    {X1,Y1,TxH}.

%% install table header return next X location
%% Device (short) baud control swap inv
uart_table_header(Parent,TxW,TxH,_W) ->
    ID = Parent++".h", %%
    X = 0,
    Y = 0,
    H = TxH,
    Opts = [{font_color,white},{color,black},{fill,solid}],

    row(ID,X,Y,(6+3+10+3+5)*TxW+(1+1+1+1+1),H,true),

    X0 = 0,
    W0 = 12*TxW,
    text_cell(ID++".device", X0, Y, W0, H,
	      [{text,"Device"},{halign,center}|Opts]),
    X1 = X0 + W0 + 1,
    W1 = 6*TxW,
    text_cell(ID++".baud", X1, Y, W1, H, 
	      [{text,"Baud"},{halign,center}|Opts]),
    X2 = X1 + W1 + 1,
    W2 = 1*TxW,
    text_cell(ID++".control", X2, Y, W2, H,
	      [{text,"C"},{halign,center}|Opts]),
    X3 = X2 + W2 + 1,
    W3 = 1*TxW,
    text_cell(ID++".swap", X3, Y, W3, H,
	      [{text,"W"},{halign,center}|Opts]),
    X4 = X3 + W3 + 1,
    W4 = 1*TxW,
    text_cell(ID++".invert", X4, Y, W4, H,
	      [{text,"I"},{halign,center}|Opts]),

    X5 = X4 + W4 + 1,
    W5 = 6*TxW,
    text_cell(ID++".status", X5, Y, W5, H,
	      [{text,"Status"},{halign,center}|Opts]),
    X5 + W5 + 1.

%% install table row return next X location
uart_table_row(Parent,I,TxW,TxH,_W) ->
    ID = Parent++[$.,$r|integer_to_list(I)], %% <id>.r<i>
    X = 0, Y = I*TxH, H = TxH,

    %% parent to table cells
    row(ID,X,Y,(6+3+10+3+5)*TxW+(1+1+1+1+1),H,false),
    epxy:add_callback(ID,select,?MODULE),
    
    %% and now the cells, smaller font for device (to make it fit)
    DeviceFont = [{name,"Arial"},{slant,roman},{weight,bold},{size,12}],
    X0 = 0,
    W0 = 12*TxW,
    text_cell(ID++".device", X0, 0, W0, H,
	      [{text,""},{halign,center},{font,DeviceFont}]),
    X1 = X0 + W0 + 1,
    W1 = 6*TxW,
    text_cell(ID++".baud", X1, 0, W1, H,
	      [{text,""},{halign,center}]),
    X2 = X1 + W1 + 1,
    W2 = 1*TxW,
    text_cell(ID++".control", X2, 0, W2, H,
	      [{text,""},{halign,center}]),
    X3 = X2 + W2 + 1,
    W3 = 1*TxW,
    text_cell(ID++".swap", X3, 0, W3, H,
	      [{text,""},{halign,center}]),
    X4 = X3 + W3 + 1,
    W4 = 1*TxW,
    text_cell(ID++".invert", X4, 0, W4, H,
	      [{text,""},{halign,center}]),
    X5 = X4 + W4 + 1,
    W5 = 6*TxW,
    text_cell(ID++".status", X5, 0, W5, H,
	      [{text,""},{halign,center}]),
    X5 + W5 + 1.



%% parent to all rows
table(ID, X, Y, W, H) ->
    epxy:new(ID,[{type,rectangle},
		 {hidden,true},{disabled,true},
		 {x,X},{y,Y},{width,W},{height,H}]).

%% parent to all cells maybe used for row selections? 
row(ID, X, Y, W, H, Disabled) ->
    epxy:new(ID,[{type,rectangle},
		 {hidden,true},{disabled,Disabled},
		 {orientation, horizontal},
		 {children_first,false},
		 {fill,blend},
		 {color,green},
		 {color2,red},
		 {min, 0.0}, {max, 1.0}, {value,0.0},
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

    {X11,Y1,_W0,H0} = tagged_text("pdb.app_vsn", "Version", X1, Y0, 0),
    {_,_,_,_} = tagged_text("pdb.uapp_vsn", "UApp", X11+XGap, Y0, 0),

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

    {X11,Yn,_W0,H0} = tagged_text("pdi.app_vsn", "Version", X1, Y0, 0),
    {_,_,_,_} = tagged_text("pdi.uapp_vsn", "UApp", X11+XGap, Y0, 0),
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

    {X11,Y1,_W0,H0} = tagged_text("pds.app_vsn", "Version", X1, Y0, 0),
    {_,_,_,_} = tagged_text("pds.uapp_vsn", "UApp", X11+XGap, Y0, 0),

    %% Ain x 8 (row=Y1,column=X1)
    {_,_Y3,W2,H2} = ain_group("pds.ain", 1, 8, X1, Y1+YGap),

    X2 = X1+W2+XGap,
    %% Pout x 8 (row Y1,column=X2)
    {_,_,W3,H3} = pout_group("pds.pout", 1, 8, X2, Y1+YGap),

    X3 = X2+W3+XGap,
    %% Aload x 8 (row=Y1,column=X3)
    {_,_Y4,W4,H4} = aload_group("pds.aload", 33, 40, X3, Y1+YGap),

    H5 = H0+YGap+max(H2,max(H3,H4)),

    Y5 = Y1 + H5 + YGap,

    {W6,H6} = add_buttons(ID, X1, Y5),

    Wt = XGap+max(W2+XGap+W3+XGap+W4,W6)+XGap,
    Ht = YGap+H0+YGap+max(H2,max(H3,H4))+YGap+H6+YGap,

    group_rectangle(ID,"powerZone",X,Y,Wt,Ht,all),

    ok.

%%
%% uBoot mode dialog
%% Buttons 
%%    Go  Upgrade Reset
%%
uBoot(X,Y,_W,_H) ->
    XGap = 12,
    YGap = ?GROUP_FONT_SIZE,
    Y0 = YGap,
    X1 = XGap,
    ID = "ubt",

    {X11,Y1,W0,H0} = tagged_text("ubt.app_vsn", "Version", X1, Y0, 0),
    {_,_,W1,_} = tagged_text("ubt.uapp_vsn", "UApp", X11+XGap, Y0, 0),

    {W6,H6} = add_uboot_buttons(ID, X1, Y1+YGap),

    Wt = XGap+max(W0+W1,W6)+XGap,
    Ht = YGap+H0+YGap+H6+YGap,

    group_rectangle(ID,"uBoot",X,Y,Wt,Ht,all),
    ok.

%% Serial boot dialog
%% boot version {Major,Minor} 
%% product
%% flashSize
%% ramSize
%% flashSectors
%% maxCopySize
%% variant
%%
lpcBoot(X,Y,_W,_H) ->
    XGap = 12,
    YGap = ?GROUP_FONT_SIZE,
    Y0 = YGap,
    X1 = XGap,
    ID = "lpc",
    TW = 96,
    {_,Y1,W0,H0} = tagged_text("lpc.vsn", "Version", X1, Y0, TW),
    {_,Y2,W1,H1} = tagged_text("lpc.product", "Product", X1, Y1+YGap, TW),
    {_,Y3,W2,H2} = tagged_text("lpc.flashSize", "FlashSize", X1, Y2+YGap, TW),
    {_,Y4,W3,H3} = tagged_text("lpc.ramSize", "RamSize", X1, Y3+YGap, TW),
    {_,Y5,W4,H4} = tagged_text("lpc.flashSectors", "Sectors", X1, Y4+YGap, TW),
    {_,Y6,W5,H5} = tagged_text("lpc.maxCopySize", "MaxCopySize",X1,Y5+YGap,TW),
    {_,Y7,W6,H6} = tagged_text("lpc.variant", "Variant", X1, Y6+YGap,TW),

    {W7,H7} = add_lpc_buttons(ID, X1, Y7+YGap, 0, 0),

    Wt = XGap+lists:max([W0,W1,W2,W3,W4,W5,W6,W7])+XGap,
    Ht = YGap+lists:sum([H0,H1,H2,H3,H4,H5,H6,H7])+8*YGap,

    group_rectangle(ID,"lpcBoot",X,Y,Wt,Ht,all),
    ok.

%% add hold and go buttons
add_lpc_buttons(ID, X, Y, _W, _H) ->
    W = ?BUTTON_WIDTH,
    H = ?BUTTON_HEIGHT,
    YGap = 10,
    XGap = 8,
    X0 = X,
    X1 = X0 + W + XGap,
    X2 = X1 + W + XGap,
    X3 = X2 + W + XGap,
    Y0 = Y,
    add_text_button(ID++".lpc_go",      "Go",      X0, Y0, W, H),
    add_text_button(ID++".lpc_flash",   "Flash",   X1, Y0, W, H),
    add_text_button(ID++".lpc_reset",   "Reset",   X2, Y0, W, H),
    add_text_button(ID++".lpc_scan",    "Scan",    X3, Y0, W, H),
    {4*W+4*XGap,1*H+1*YGap}.

%% add hold and go buttons
add_uboot_buttons(ID, X, Y) ->
    W = ?BUTTON_WIDTH,
    H = ?BUTTON_HEIGHT,
    YGap = 10,
    XGap = 8,
    X0 = X,
    X1 = X0 + W + XGap,
    X2 = X1 + W + XGap,
    Y0 = Y,
    add_text_button(ID++".go",      "Go",      X0, Y0, W, H),
    add_text_button(ID++".upgrade", "Upgrade", X1, Y0, W, H),
    add_text_button(ID++".reset",   "Reset",   X2, Y0, W, H),
    {3*W+1*XGap,1*H+1*YGap}.

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
    add_text_button(ID++".reset",   "Reset",   X1, Y0, W, H),

    add_text_button(ID++".setup",   "Setup",   X0, Y1, W, H),
    add_text_button(ID++".factory", "Factory", X1, Y1, W, H),
    add_text_button(ID++".save",    "Save",    X2, Y1, W, H),
    add_text_button(ID++".restore", "Restore",    X3, Y1, W, H),
    {4*W+3*XGap,2*H+2*YGap}.

add_text_button(ID, Text, X, Y, W, H) ->
    FontSpec = [{name,"Arial"},{weight,bold},{size,?BUTTON_FONT_SIZE}],
    {ok,Font} = epx_font:match(FontSpec),
    epxy:new(ID,
	     [{type,button},
	      {halign,center},
	      {x,X},{y,Y},{width,W},{height,H},
	      {shadow_x,2},{shadow_y,2},{children_first,false},
	      {round_h,?BUTTON_ROUND_WH},{round_w,?BUTTON_ROUND_WH},
	      {font,Font},
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
			Num = (Chan - min(Chan1,Chan0))+1,
			{_,Y1,W1,_H1} = aout(ID,Chan,Num,XLeft,Yi),
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
			Num = (Chan - min(Chan1,Chan0))+1,
			{_,Y1,W1,_H1} = pout(ID,Chan,Num,XLeft,Yi),
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
			Num = (Chan - min(Chan1,Chan0))+1,
			{_,Y1,W1,_H1} = ain(ID,Chan,Num,XLeft,Yi),
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
			Num = (Chan - min(Chan1,Chan0))+1,
			{_,Y1,W1,_H1} = aload(ID,Chan,Num,XLeft,Yi),
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
			Num = (Chan - min(Chan1,Chan0))+1,
			{_,Y1,W1,_H1} = din(ID,Chan,Num,XLeft,Yi),
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
			Num = (Chan - min(Chan1,Chan0))+1,
			{_,Y1,W1,_H1} = dout(ID,Chan,Num,XLeft,Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0,Chan1,Step)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Dout",X0,Y0,W,H,false),
    {X0,Y3,W,H}.


dout(ID0,Chan,Num,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = ?ONOFF_WIDTH, H = ?ONOFF_FONT_SIZE,
    LW = 12,
    FontSpecL = [{name,"Arial"},{slant,roman},{size,?ONOFF_FONT_SIZE}],
    FontSpec  = [{name,"Arial"},{weight,bold},{size,?ONOFF_FONT_SIZE}],
    epxy:new(ID,[{type,switch},
		 {halign,center},
		 {x,X+LW},{y,Y},{width,W},{height,H},
		 {shadow_x,2},{shadow_y,2},{children_first,false},
		 {round_h,?ONOFF_ROUND_WH},{round_w,?ONOFF_ROUND_WH},
		 {font,FontSpec},
		 {fill,solid},{color,lightgray},
		 {text,"OFF"}
		]),
    epxy:new(ID++".label",[{type,text},
			   {font,FontSpecL},
			   {font_color,black},
			   {x,-LW},{y,0},
			   {width,10},{height,H},
			   {halign,left},
			   {text,integer_to_list(Num)}]),
    epxy:add_callback(ID,switch,?MODULE),
    {X,Y+H,W+LW,H}.

din(ID0,Chan,Num,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 24, H = 14,
    LW = 12,
    FontSpecL = [{name,"Arial"},{slant,roman},{size,?ONOFF_FONT_SIZE}],
    FontSpec = [{name,"Arial"},{weight,bold},{size,?DIN_FONT_SIZE}],
    epxy:new(ID,[{type,value},
		 {halign,center},{valign,center},
		 {x,X+LW},{y,Y},{width,W},{height,H},
		 {children_first,false},
		 {font,FontSpec},
		 {fill,solid},{color,white},
		 {format,"~w"}
		]),
    epxy:new(ID++".border",
	     [{type,rectangle},
	      {color,black},
	      {x,-1},{y,-1},
	      {width,W+2},{height,H+2}]),
    epxy:new(ID++".label",[{type,text},
			   {font,FontSpecL},
			   {font_color,black},
			   {x,-LW},{y,0},
			   {width,LW},{height,H},
			   {halign,left},
			   {text,integer_to_list(Num)}]),
    {X,Y+H,W+LW,H}.

ain(ID0,Chan,Num,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 40, H = ?AIN_FONT_SIZE,
    LW = 12,
    FontSpecL = [{name,"Arial"},{weight,medium},{size,?AIN_FONT_SIZE}],
    FontSpec = [{name,"Arial"},{weight,bold},{size,?AIN_FONT_SIZE}],
    epxy:new(ID,[{type,value},
		 {halign,center},{valign,center},
		 {x,X+LW},{y,Y},{width,W},{height,H},
		 {children_first,false},
		 {font,FontSpec},
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
    epxy:new(ID++".label",[{type,text},
			   {font,FontSpecL},
			   {font_color,black},
			   {x,-LW},{y,0},
			   {width,LW},{height,H},
			   {halign,left},
			   {text,integer_to_list(Num)}]),
    {X,Y+H,W+LW,H}.

aload(ID0,Chan,Num,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 32, H = ?ALOAD_FONT_SIZE,
    LW = 12,
    FontSpecL = [{name,"Arial"},{weight,medium},{size,?ALOAD_FONT_SIZE}],
    FontSpec = [{name,"Arial"},{weight,bold},{size,?ALOAD_FONT_SIZE}],
    epxy:new(ID,
	     [{type,value},
	      {halign,center},{valign,center},
	      {x,X},{y,Y},{width,W},{height,H},
	      {children_first,false},
	      {font,FontSpec},
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
    epxy:new(ID++".label",[{type,text},
			   {font,FontSpecL},
			   {font_color,black},
			   {x,-LW},{y,0},
			   {width,LW},{height,H},
			   {halign,left},
			   {text,integer_to_list(Num)}]),
    {X,Y+H,W+LW,H}.
    
aout(ID0,Chan,Num,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = ?SLIDER_WIDTH+?ONOFF_WIDTH+16, 
    H = ?ONOFF_FONT_SIZE,
    LW = 12,
    FontSpecL = [{name,"Arial"},{weight,medium},{size,?ONOFF_FONT_SIZE}],
    FontSpec = [{name,"Arial"},{weight,bold},{size,?ONOFF_FONT_SIZE}],
    epxy:new(ID,[{type,slider},
		 {x,X+LW+?ONOFF_WIDTH+16},{y,Y+2},
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
		  {font,FontSpec},
		  {fill,solid},{color,lightgray},
		  {round_h,?ONOFF_ROUND_WH},{round_w,?ONOFF_ROUND_WH},
		  {text,"OFF"}
		 ]),
    epxy:add_callback(ID1,switch,?MODULE),
    epxy:new(ID++".label",[{type,text},
			   {font,FontSpecL},
			   {font_color,black},
			   {x,-(LW+?ONOFF_WIDTH+16)},{y,-4},
			   {width,LW},{height,H},
			   {halign,left},
			   {text,integer_to_list(Num)}]),
    {X,Y+H,W+LW,H}.

pout(ID0,Chan,Num,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = ?SLIDER_WIDTH+?ONOFF_WIDTH+16, 
    H = ?ONOFF_FONT_SIZE,
    LW = 12,
    FontSpecL = [{name,"Arial"},{weight,medium},{size,?ONOFF_FONT_SIZE}],
    FontSpec = [{name,"Arial"},{weight,bold},{size,?ONOFF_FONT_SIZE}],
    epxy:new(ID,[{type,slider},
		 {x,X+LW+?ONOFF_WIDTH+16},{y,Y+2},
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
		  {font,FontSpec},
		  {fill,solid},{color,lightgray},
		  {round_h,?ONOFF_ROUND_WH},{round_w,?ONOFF_ROUND_WH},
		  {text,"OFF"}
		 ]),
    epxy:add_callback(ID1,switch,?MODULE),
    epxy:new(ID++".label",[{type,text},
			   {font,FontSpecL},
			   {font_color,black},
			   {x,-(LW+?ONOFF_WIDTH+16)},{y,-4},
			   {width,LW},{height,H},
			   {halign,left},
			   {text,integer_to_list(Num)}]),
    {X,Y+H,W+LW,H}.

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
	      {font_color,black},
	      {text,Text},
	      {color,white},{fill,solid},
	      {halign,left},
	      {x,5},{y,-5},
	      {height,10}  %% width,25
	     ]).

tagged_text(ID,TagText,X,Y,TagWidth) ->
    FontSpec = [{name,"Arial"},{slant,roman},{size,?GROUP_FONT_SIZE}],
    {ok,Font} = epx_font:match(FontSpec),
    {Wt,Ht} = epx_font:dimension(Font,TagText),
    XOffs = max(Wt+4,TagWidth),
    H = Ht+2,
    W = 8*10,
    epxy:new(ID,
	     [{type,text},
	      {font,Font},
	      {font_color, black},
	      {text,""},
	      {color,white},{fill,solid},
	      {halign,left},
	      {x,X+XOffs},{y,Y},
	      {height,Ht}, {width,W}
	     ]),
    epxy:new(ID++".border",
	     [{type,rectangle},
	      {color,black},
	      {x,-1},{y,-1},
	      {width,W+2},{height,H+2}]),
    epxy:new(ID++".tag",
	     [{type,text},
	      {font,Font},
	      {font_color, black},
	      {text,TagText},
	      {color,white},{fill,solid},
	      {halign,left},
	      {x,-XOffs},{y,0},
	      {height,10}
	     ]),
    {X+XOffs+W+2,Y+H,XOffs+W+2,H}.

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
    hide_if_selected(Serial,State),
    Nodes = set_status_by_serial(Serial, boot, State#state.nodes),
    State1 = State#state { nodes=Nodes },
    State3 = 
	if State1#state.hold_mode ->
		XCobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
		WDT = 0,
		co_sdo_cli:send_sdo_set(XCobId, ?INDEX_UBOOT_HOLD, 0, WDT),
		State2 = State1#state { sdo_request={?INDEX_UBOOT_HOLD, 0},
					hold_mode=false },
		refresh_node_state(State2);
	   true ->
		refresh_node_state(State1)
	end,
    show_if_selected(Serial,State3),
    {noreply,State3}.


node_started(_CobId, Serial, State) ->
    ?dbg("Node ~6.16.0B started\n", [Serial]),
    hide_if_selected(Serial,State),
    Nodes = set_status_by_serial(Serial, up, State#state.nodes),
    State1 = State#state { nodes=Nodes },
    show_if_selected(Serial,State1),
    spawn(
      fun() ->
	      XCobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
	      ?dbg("XCobId = ~8.16.0B\n", [XCobId]),
	      co_sdo_cli:send_sdo_get(XCobId, ?INDEX_ID, 0),
	      co_sdo_cli:send_sdo_get(XCobId, ?IX_IDENTITY_OBJECT, ?SI_IDENTITY_PRODUCT)
	      %% ...
      end),
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
	    SID = selected_id(State),
	    ?dbg("MSG_ANALOG: [~s], si=~w, value=~w\n",
		 [SID,Si,Value]),
	    case SID of
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
	    SID = selected_id(State),
	    ?dbg("MSG_DIGITAL: [~s], si=~w, value=~w\n", 
		 [SID,Si,Value]),
	    case SID of
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
	    SID = selected_id(State),
	    ?dbg("MSG_OUTPUT_ACTIVE: [~s], si=~w, value=~w\n", 
		 [SID,Si,Value]),
	    case SID of
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

	?MSG_OUTPUT_VALUE ->
	    SID = selected_id(State),
	    ?dbg("MSG_OUTPUT_VALUE: [~s], si=~w, value=~w\n", 
		 [SID,Si,Value]),
	    case SID of
		"pdb" ->
		    case Si of
			1 -> set_value("pdb.pout.e1", Value);
			2 -> set_value("pdb.pout.e2", Value);
			3 -> set_value("pdb.pout.e3", Value);
			4 -> set_value("pdb.pout.e4", Value);
			5 -> set_value("pdb.aout.e5", Value);
			6 -> set_value("pdb.aout.e6", Value);
			_ -> ok
		    end;
		"pds" ->
		    case Si of
			1 -> set_value("pds.pout.e1", Value);
			2 -> set_value("pds.pout.e2", Value);
			3 -> set_value("pds.pout.e3", Value);
			4 -> set_value("pds.pout.e4", Value);
			5 -> set_value("pds.pout.e5", Value);
			6 -> set_value("pds.pout.e6", Value);
			7 -> set_value("pds.pout.e7", Value);
			8 -> set_value("pds.pout.e8", Value);
			_ -> ok
		    end;
		_ ->
		    ok
	    end;

	?MSG_OUTPUT_STATE ->
	    _OnOff = (Value bsr 24) band 16#ff,
	    _OutSt = (Value bsr 16) band 16#ff,
	    _Duty  = Value band 16#ffff,
	    _SID = selected_id(State),
	    ?dbg("MSG_OUTPUT_STATE: [~s], si=~w,on=~w,state=~w,duty=~w\n", 
		 [_SID,Si,_OnOff,_OutSt,_Duty]),
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

is_selected_by_serial(Serial, State) ->
    case find_node_by_serial(Serial, State#state.nodes) of
	false ->
	    false;
	Node ->
	    maps:get(pos,Node) =:= State#state.selected_pos
    end.

selected_status(State) ->
    selected_status(State#state.selected_pos,State).
selected_status(Pos,State) ->
    case find_node_by_pos(State#state.selected_tab,Pos,State#state.nodes) of
	false -> undefined;
	Node -> maps:get(status,Node,undefined)
    end.

selected_id(State) ->
    selected_id(State#state.selected_tab,State#state.selected_pos,State).

selected_id("uarts",Pos,State) ->
    case find_by_pos(Pos,State#state.uarts) of
	false -> undefined;
	Uart -> "lpc"
    end;
selected_id("nodes",Pos,State) ->
    case find_by_pos(Pos,State#state.nodes) of
	false -> undefined;
	Node ->
	    case maps:get(status,Node,undefined) of
		undefined -> undefined;
		up ->
		    case maps:get(product,Node,undefined) of
			undefined -> undefined;
			powerZone ->   "pds";
			ioZone ->      "pdi";
			bridgeZone ->  "pdb";
			controlZone -> "pdc";
			_ -> undefined
		    end;
		boot  -> "ubt";
		flash -> "ubt"
	    end
    end;
selected_id(undefined,_Pos,_State) ->
    undefined.


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
	    Pos = allocate_node_pos(Ns),
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
    Status = selected_status(State),
    Status1 = selected_status(State1),
    if Status =:= Status1 ->
	    ok;
       true ->
	    hide(State),
	    show(State1)
    end,
    refresh_node_state(State1).

set_by_cobid_(CobID,Key,Value,State) ->
    %% ?dbg("set_by_cobid: key=~p, value=~p\n", [Key,Value]),
    case ?is_cobid_extended(CobID) of
	true ->
	    Serial = ?XNODE_ID(CobID),
	    Ns = State#state.nodes,
	    case take_node_by_serial(Serial, Ns) of
		false ->
		    Pos = allocate_node_pos(Ns),
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
		    Pos = allocate_node_pos(Ns),
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

allocate_node_pos(Ns) ->
    PosList = [ maps:get(pos,N) || N <- Ns] -- [(?NUM_TABLE_NODES+1)],
    io:format("PosList = ~w\n", [PosList]),
    Next = if PosList =:= [] -> 1;
	      true -> lists:max(PosList)+1
	   end,
    if Next >= ?NUM_TABLE_NODES ->
	    ?NUM_TABLE_NODES;  %% FIXME: find free pos
       true ->
	    Next
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

set_uart_text(Key,Pos,Value) ->
    case is_uart_item(Key) of
	true ->
	    ID = "uarts.r"++integer_to_list(Pos)++"."++atom_to_list(Key),
	    ?dbg("set_uart_text: id=~s, value=~p\n", [ID,Value]),
	    epxy:set(ID,[{text,format_value(Key,Value)}]);
	false ->
	    ok
    end.

format_value(serial,Value) when is_integer(Value) ->
    tl(integer_to_list(16#1000000+Value, 16));
format_value(device,Value) when is_list(Value) ->
    Value;
format_value(vsn,{Major,Minor}) ->
    integer_to_list(Major)++"."++integer_to_list(Minor);
format_value(app_vsn,{{Year,Mon,Day},{_H,_M,_S}}) ->
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", 
				[Year,Mon,Day]));
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

is_uart_item(device) -> true;
is_uart_item(baud) -> true;
is_uart_item(control) -> true;
is_uart_item(swap) -> true;
is_uart_item(invert) -> true;
is_uart_item(status) -> true;
is_uart_item(_) -> false.

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

find_by_pos(Pos, Maps) ->  
    find_by_key(pos, Pos, Maps).

find_node_by_pos("nodes",Pos, Ns) ->  find_by_key(pos, Pos, Ns);
find_node_by_pos(_Tab,_Pos,_Ns) -> false.

find_node_by_serial(Serial, Ns) -> find_by_key(serial, Serial, Ns).

%% find node with key and Value
find_by_key(Key,Value,[M|Ms]) ->
    case M of
	#{ Key := Value} ->
	    M;
	_ ->
	    find_by_key(Key,Value,Ms)
    end;
find_by_key(_Key, _Value, []) ->
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

flash_node(Node,_Version,Bs,State) ->
    SELF = self(),
    Pos = maps:get(pos,Node,undefined),
    ID = "nodes.r"++integer_to_list(Pos),
    Serial = maps:get(serial,Node,0),
    Nid = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
    State1 = set_by_cobid(Nid,status,flash,State),
    %% FIXME: monitor this process and update when crashes etc
    spawn(
      fun() ->
	      Progress = fun(V) -> epxy:set(ID,[{value,V}]) end,
	      case gordon_can_flash:upload(Nid,Bs,1024,Progress) of
		  ok ->
		      SELF ! {node_flashed,ID,Nid,boot,ok};
		  Error ->
		      SELF ! {node_flashed,ID,Nid,boot,Error}
	      end
      end),
    State1.

flash_firmware(U, Firmware, DevType) ->
    BlockList = elpcisp:block_list(Firmware, DevType),
    flash_block_list(U, DevType, BlockList).

flash_block_list(U, DevType, [{Start,StartBlock,EndBlock,Data}|Bs]) ->
    {ok,_} = elpcisp:prepare_sector(U, StartBlock, EndBlock),
    {ok,_} = elpcisp:erase_sector(U, StartBlock, EndBlock),
    Pos = 1,  %% fixme multiple uarts
    End = Start + byte_size(Data),
    ID = "uarts.r"++integer_to_list(Pos),
    elpcisp:flash_block(U, DevType, Start, Data,
			fun(A) ->
				V = (A-Start)/(End-Start),
				epxy:set(ID,[{value,V}])
			end),
    flash_block_list(U, DevType, Bs);
flash_block_list(_U, _DevType, []) ->
    ok.
