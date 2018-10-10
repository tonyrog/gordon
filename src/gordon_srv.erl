%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Gordon flash programmer / node viewer
%%% @end
%%% Created :  4 Jun 2018 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(gordon_srv).

-behaviour(gen_server).

-include_lib("can/include/can.hrl").
-include_lib("canopen/include/canopen.hrl").
-include_lib("canopen/include/sdo.hrl").
-include_lib("canopen/include/co_app.hrl").
-include_lib("elpcisp/src/elpcisp.hrl").
%% API
-export([start_link/0]).

-export([event/3]).        %% fixme better unique callback name
-export([event_filter/6]).

-compile(export_all).

-export([firmware_info/0]).
%%-export([serial_flash_bootloader/0]).
%%-export([can_flash_bootloader/1]).
%%-export([can_flash_application/1, can_flash_application/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type uart() :: 
	#{
	  pos => integer(),
	  uart => atom(),
	  device => string(),
	  baud => integer(),
	  control => boolean(),
	  control_swap => boolean(),
	  control_inv => boolean(),
	  status => idle | scan | open | error | ok,
	  lpc_type => integer(),         %% LPC device type
	  lpc_info => #{ version => integer(),
			 product => string(),
			 flashSize => integer(),
			 ramSize => integer(),
			 flashSectors => integer(),
			 maxCopySize => integer(),
			 variant => atom()
		       },
	  hold => boolean(),
	  %% serial uboot information
	  ubt_info => #{ serial   => integer(),
			 product  => integer(),
			 creation => integer(),
			 app_vsn  => integer(),
			 app_addr => integer() }
	 }.

-type cannode() ::
	#{
	  pos => integer(),
	  id  => integer(),
	  serial => integer(),
	  product => integer(),
	  creation => integer(),
	  app_vsn => integer(),
	  app_addr => integer(),
	  vsn => {Major::integer(),Minor::integer()},
	  status => up | down | boot | flash | ok | error | free,
	  activity => integer()        %% system time of last activity
	 }.

-record(state,
	{
	 echo_timer,      %% CANbus ping timer
	 hold_mode=false, %% hold the (selected) booting node
	 selected_tab,    %% "nodes" | "uarts" | undefined
	 selected_pos,    %% Selected row | undefined
	 selected_eff=0,  %% extended canid of selected node
	 selected_sff=0,  %% short canid of selected node
	 sdo_request,     %% current outstanding sdo_request
	 sdo_error,       %% last sdo_error
	 row_height = 1,  %% height of row selection area
	 firmware = [],   %% list of available firmware for upgrade
	 nodes = [] :: [cannode()],    %% list of can nodes
	 uarts = [] :: [uart()],       %% list of uarts
	 use_virtual_keyboard
	}).

-define(ACTIVITY_TIMEOUT, (10*1000000)). %% time when node is considered down
-define(REMOVE_TIMEOUT,  30*1000000).   %% time after wich node is deleted
-define(ECHO_INTERVAL, 5000).           %% ping every 5s
-define(SAVE_TIMEOUT, 20*1000000).      %% special for save operation

-define(TEXT_CELL_FONT_SIZE, 20).
-define(DIN_FONT_SIZE, 14).
-define(DOUT_FONT_SIZE, 14).
-define(AIN_FONT_SIZE, 14).
-define(ALOAD_FONT_SIZE, 14).
-define(BUTTON_FONT_SIZE, 18).
-define(LABEL_FONT_SIZE, 14).
-define(ONOFF_FONT_SIZE, 14).
-define(ONOFF_ROUND_WH, 4).
-define(ONOFF_WIDTH,  40).
-define(ONOFF_HEIGHT, 18).
-define(GROUP_FONT_SIZE, 12).
-define(SLIDER_WIDTH,  100).
-define(SLIDER_HEIGHT, 8).
-define(BUTTON_WIDTH, 84).
-define(BUTTON_HEIGHT, 18).
-define(BUTTON_ROUND_WH, 4).
-define(TOP_XGAP, 12).
-define(TOP_YGAP, 12).
-define(GROUP_XGAP, 8).
-define(GROUP_YGAP, 12).
-define(BUTTON_XGAP, 8).
-define(BUTTON_YGAP, 12).

-define(NUM_TABLE_NODES, 14).
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

-define(PRODUCT_ANY,         16#0000).
-define(PRODUCT_PDS,         16#0001).
-define(PRODUCT_PDS_1A,      16#0101).
-define(PRODUCT_PDS_DMX,     16#0201).
-define(PRODUCT_PDS_DMX_1A,  16#0301).
-define(PRODUCT_PDC_80,      16#8002).
-define(PRODUCT_PDC_08,      16#0802).
-define(PRODUCT_PDC_44,      16#4402).
-define(PRODUCT_PDC_71,      16#7102).
-define(PRODUCT_PDC_8F,      16#8F02).
-define(PRODUCT_PDC,         16#0002).
-define(PRODUCT_PANEL,       16#0002).
-define(PRODUCT_PDD_25,      16#2503).
-define(PRODUCT_PDD,         16#0003).
-define(PRODUCT_PDI,         16#0004).
-define(PRODUCT_PDI_24,      16#0004).
-define(PRODUCT_PDI_12,      16#0104).
-define(PRODUCT_INPUT,       16#0004).
-define(PRODUCT_FUSE,        16#0005).
-define(PRODUCT_ADC,         16#0006).
-define(PRODUCT_REMOTE,      16#0007).
-define(PRODUCT_RFID,        16#0008).
-define(PRODUCT_PDB,         16#0009).
-define(PRODUCT_PDB_DMX,     16#0209).
-define(PRODUCT_PDB_MARINCO, 16#0409).
-define(PRODUCT_PDB_KELLY,   16#0809).
-define(PRODUCT_VPULSE,      16#000A).
-define(PRODUCT_ISOTTA_WHEEL,16#000B).
-define(PRODUCT_DEV,         16#00FF).
-define(PRODUCT_DEV_MCB2100, 16#21FF).

-define(PRODUCT_ID_MASK,     16#FFFF0000).
-define(PRODUCT_VSN_MASK,    16#0000FFFF).
-define(PRODUCT_MK(P,V),     ((((P) bsl 16) band ?PRODUCT_ID_MASK) bor 
				  ((V) band ?PRODUCT_VSN_MASK))).

-define(CN_CRC_DISABLED,     16#2BAD2BAD).
-define(CN_CRC_NONE,         16#FFFFFFFF).
-define(CN_APP_ZERO,         16#00000000).
-define(CN_APP_NONE,         16#FFFFFFFF).
-define(CN_APP_EMPTY,        16#2F5EBD7A). %% 0101111010111101011110101111010

%% -define(dbg(F,A), io:format((F),(A))).
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
    %% options list override env
    Env = Options ++ application:get_all_env(gordon),
    epxy:set("screen",[{static,false}]), %% allow close
    epxy:add_callback("screen",event,?MODULE),
    Width  = proplists:get_value(screen_width, Env, 800),
    Height = proplists:get_value(screen_height, Env, 480),
    XOffs = 8,
    YOffs = 8,
    {_X1,Y1,RH1} = node_table(XOffs,YOffs,Width div 2, Height),
    {_X2,_Y2,_RH2} = uart_table(XOffs,Y1+2*YOffs,Width div 2, Height),

    epxy:new("qwerty", [{type,user},{user,{gordon_keyboard,alpha,[]}},
			{hidden, true}, {disabled, true}, {last, true},
			{border,2}, {border_color, green}
		       ]),
    [{width,Qw},{height,Qh}] = epxy:get("qwerty", [width,height]),
    epxy:set("qwerty", [{x,(800-Qw) div 2}, {y,(480-Qh)}]),

    epxy:new("decimal_keyboard", 
	     [{type,user},{user,{gordon_keyboard,decimal,[]}},
	      {hidden, true}, {disabled, true}, {last, true},
	      {border,2}, {border_color, blue}
	     ]),
    [{width,Nw1},{height,Nh1}] = epxy:get("decimal_keyboard", [width,height]),
    epxy:set("decimal_keyboard", [{x,(800-Nw1) div 2}, {y,(480-Nh1)}]),

    epxy:new("hexadecimal_keyboard",
	     [{type,user},{user,{gordon_keyboard,hexadecimal,[]}},
	      {hidden, true}, {disabled, true}, {last, true},
	      {border,2}, {border_color, blue}
	     ]),
    [{width,Nw2},{height,Nh2}]=epxy:get("hexadecimal_keyboard", [width,height]),
    epxy:set("hexadecimal_keyboard", [{x,(800-Nw2) div 2}, {y,(480-Nh2)}]),

    UseVirtualKeyboard = proplists:get_value(use_virtual_keyboard, Env, false),

    %% define various layouts
    X = Width div 2,
    Y = 10,
    W = Width div 3,
    H = Height - 32,

    bridgeZone(X,Y,W,H),
    ioZone(X,Y,W,H),
    powerZone(X,Y,W,H),
    controlZone(X,Y,W,H),
    uBoot(X,Y,W,H),
    uartBoot(X,Y,W,H),

    can_router:attach(),

    %% request response from all nodes
    send_pdo1_tx(0, ?MSG_ECHO_REQUEST, 0, 0),

    Firmware = load_firmware(),

    Devices = proplists:get_value(devices,Env,[]),
    UARTS = make_uart_list(Devices),

    EchoTimer = erlang:start_timer(?ECHO_INTERVAL, self(), echo_timeout),

    {ok, #state{ echo_timer = EchoTimer, 
		 row_height = RH1, 
		 firmware = Firmware, 
		 nodes = [],
		 uarts = UARTS, 
		 use_virtual_keyboard = UseVirtualKeyboard
	       }}.

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
    ?dbg("got cast ~p\n", [_Msg]),
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
    %% ?dbg("Frame = ~p\n", [Frame]),
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
	    {noreply, State}
    end;
    
handle_info({event,"screen",#{event:=closed}}, State) ->
    %% fixme: try to terminate gracefully
    {stop, normal, State};

%% handle select in node list
handle_info({select,"nodes.r"++RTxt,#{event:=button_press}},State) ->
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
handle_info({select,"uarts.r"++RTxt,#{event:=button_press}},State) ->
    Pos = list_to_integer(RTxt),
    State1 = deselect_row(State#state.selected_tab,
			  State#state.selected_pos,State),
    ?dbg("deselect row=~w\n", [State#state.selected_pos]),
    Tab = "uarts",
    case selected_id(Tab,Pos,State1) of
	undefined ->
	    {noreply, State1};
	"uart" ->
	    case find_uart_by_pos(Tab,Pos,State#state.uarts) of
		false ->
		    {noreply, State1};
		Uart ->
		    highlight_row(Tab,Pos),
		    State2 = State1#state { selected_tab = Tab,
					    selected_pos = Pos,
					    selected_eff = undefined,
					    selected_sff = undefined
					  },
		    show_pos(Tab,Pos,State2),
		    refresh_uart_state(Uart,State2),
		    {noreply, State2}
	    end;
	_ ->
	    {noreply, State1}
    end;
handle_info({decimal_item,_ID,#{event:=focus_in}},State) ->
    %% show numeric keyboard if requested
    if State#state.use_virtual_keyboard ->
	    epxy:set("decimal_keyboard", [{hidden,false}, {disabled,rest}]);
       true ->
	    ok
    end,
    {noreply, State};
handle_info({decimal_item,_ID,#{event:=focus_out}},State) ->
    %% hide numeric keyboard if requested
    if State#state.use_virtual_keyboard ->
	    epxy:set("decimal_keyboard", [{hidden,true}, {disabled,true}]);
       true ->
	    ok
    end,
    {noreply, State};
handle_info({hexadecimal_item,_ID,#{event:=focus_in}},State) ->
    %% show numeric keyboard if requested
    if State#state.use_virtual_keyboard ->
	    epxy:set("hexadecimal_keyboard", [{hidden,false}, {disabled,rest}]);
       true ->
	    ok
    end,
    {noreply, State};

handle_info({hexadecimal_item,_ID,#{event:=focus_out}},State) ->
    %% hide numeric keyboard if requested
    if State#state.use_virtual_keyboard ->
	    epxy:set("hexadecimal_keyboard", [{hidden,true}, {disabled,true}]);
       true ->
	    ok
    end,
    {noreply, State};

handle_info({menu,"ubt.product",#{event:=changed,value:=I}}, State) ->
    case find_node_by_pos(State) of
	false -> {noreply, State};
	Node = #{ pos := Pos } ->
	    Product = maps:get(product, Node, 0),
	    Type = (Product bsr 16) band 16#ff,
	    case product(I) of
		false ->
		    {noreply,State};
		Product -> %% nochange
		    {noreply,State};
		NewProduct ->
		    CobId = State#state.selected_eff,
		    NewType = (NewProduct bsr 16) band 16#ff,
		    if Type =/= NewType ->
			    Value1 = (maps:get(serial,Node,0) bsl 8) bor 
				((NewProduct bsr 16) band 16#ff),
			    co_sdo_cli:send_sdo_set(CobId,?INDEX_BOOT_SERIAL,
						    0,Value1);
		       true  ->
			    ok
		    end,
		    co_sdo_cli:send_sdo_set(CobId,?INDEX_BOOT_PRODUCT,
					    0,NewProduct),
		    set_product_menu("ubt.product", NewProduct),
		    Vsn = { (NewProduct bsr 8) band 16#ff, 
			    NewProduct band 16#ff},
		    Node1 = Node#{ vsn=> Vsn, product=>NewProduct },
		    State1 = update_node(Node1, State),
		    refresh_node_row(vsn,Pos,Vsn),
		    refresh_node_row(product,Pos,NewProduct),
		    {noreply, State1}
	    end
    end;
handle_info({menu,"uart.ubt.product",
	     #{event:=changed,value:=I}},State) ->
    case find_uart_by_pos(State) of
	false -> {noreply, State};
	#{ uart := undefined } -> {noreply, State};
	Uart = #{ uart := U, ubt_info := Info, hold := true } ->
	    Product = maps:get(product,Info,0),
	    Serial  = maps:get(serial,Info,0),
	    Type = (Product bsr 16) band 16#ff,
	    case product(I) of
		false -> 
		    {noreply,State};
		Product -> %% nochange
		    {noreply,State};
		NewProduct ->
		    NewType = (NewProduct bsr 16) band 16#ff,
		    %% update the full serial if product type changed
		    if Type =/= NewType ->
			    Value1 = (Serial bsl 8) bor 
				((NewProduct bsr 16) band 16#ff),
			    Hex1 = integer_to_list(Value1,16),
			    uart_ubt_command(U,["serial 0x",Hex1]);
		       true ->
			    ok
		    end,
		    Hex = integer_to_list(NewProduct,16),
		    uart_ubt_command(U,["product 0x",Hex]),
		    set_product_menu("uart.ubt.product", NewProduct),
		    Info1 = Info#{ product=>NewProduct },
		    Uart1 = Uart#{ product => NewProduct, ubt_info=>Info1 },
		    State1 = update_uart(Uart1, State),
		    %% Update node list as well
		    State3 = case find_by_key(serial,Serial,
					      State1#state.nodes) of
				 false -> State1;
				 Node = #{ pos := NPos } ->
				     Vsn = { (NewProduct bsr 8) band 16#ff,
					     NewProduct band 16#ff},
				     Node1 = Node#{ product => NewProduct,
						    vsn => Vsn },
				     State2 = update_node(Node1, State1),
				     refresh_node_row(vsn,NPos,Vsn),
				     refresh_node_row(product,NPos,NewProduct),
				     State2
			     end,
		    {noreply, State3 }
	    end
    end;

handle_info({decimal_item,"ubt.serial",#{event:=changed,value:=Text}},State) ->
    case find_node_by_pos(State) of
	false -> {noreply, State};
	Node = #{ pos := Pos } ->
	    try list_to_integer(Text, 16) of
		Serial ->
		    Product = maps:get(product,Node,0),
		    CobId = State#state.selected_eff,
		    Value = (Serial bsl 8) bor ((Product bsr 16) band 16#ff),
		    co_sdo_cli:send_sdo_set(CobId,?INDEX_BOOT_SERIAL,0,Value),
		    Node1 = Node#{ serial=>Serial },
		    State1 = update_node(Node1, State),
		    refresh_node_row(serial,Pos,Serial),
		    EFF = ?XCOB_ID(?PDO1_TX,Serial),
		    {noreply, State1#state{selected_eff=EFF}}
	    catch
		error:_ ->
		    lager:error("bad hex serial ~p", [Text]),
		    {noreply, State}
	    end
    end;
handle_info({decimal_item,"uart.ubt.serial",
	     #{event:=changed,value:=Text}},State) ->
    case find_uart_by_pos(State) of
	false -> {noreply, State};
	#{ uart := undefined } -> {noreply, State};
	Uart = #{ uart := U, ubt_info := Info, hold := true } ->
	    try list_to_integer(Text, 16) of
		Serial ->
		    Product = maps:get(product,Info,0),
		    Value = (Serial bsl 8) bor ((Product bsr 16) band 16#ff),
		    HexValue = integer_to_list(Value,16),
		    uart_ubt_command(U,["serial 0x",HexValue]),
		    Info1 = Info#{serial=>Serial},
		    Uart1 = Uart#{ ubt_info => Info1},
		    State1 = update_uart(Uart1, State),
		    {noreply, State1}
	    catch
		error:_ ->
		    lager:error("bad hex serial ~p\n", [Text]),
		    {noreply, State}
	    end;
	_Uart ->
	    lager:error("Uart not in hold mode\n", []),
	    {noreply, State}	    
    end;

handle_info({decimal_item,"ubt.creation",#{event:=changed,value:=Text}},State) ->
    case find_node_by_pos(State) of
	false -> {noreply, State};
	Node ->
	    try list_to_integer(Text, 10) of
		Date10 ->
		    Value = date10_to_utc_seconds(Date10),
		    CobId = State#state.selected_eff,
		    co_sdo_cli:send_sdo_set(CobId,?INDEX_BOOT_DATETIME,0,Value),
		    Node1 = Node#{ creation=>Value },
		    State1 = update_node(Node1, State),
		    epxy:set("ubt.creation", [{text,format_date(Value)}]),
		    {noreply, State1}
	    catch
		error:_ ->
		    lager:error("bad creation ~p\n", [Text]),
		    {noreply, State}
	    end
    end;
handle_info({decimal_item,"uart.ubt.creation",
	     #{event:=changed,value:=Text}},State) ->
    case find_uart_by_pos(State) of
	false -> {noreply, State};
	#{ uart := undefined } -> {noreply, State};
	Uart = #{ uart := U, ubt_info := Info, hold := true } ->
	    try list_to_integer(Text, 10) of
		Date10 ->
		    Value = date10_to_utc_seconds(Date10),
		    DecValue = integer_to_list(Value),
		    uart_ubt_command(U,["datetime ",DecValue]),
		    Info1 = Info#{ creation => Value},
		    Uart1 = Uart#{ ubt_info => Info1},
		    State1 = update_uart(Uart1, State),
		    epxy:set("uart.ubt.creation", [{text,format_date(Value)}]),
		    {noreply, State1}
	    catch
		error:_ ->
		    lager:error("bad hex serial ~p\n", [Text]),
		    {noreply, State}
	    end;
	_Uart ->
	    lager:error("Uart not in hold mode\n", []),
	    {noreply, State}	    
    end;

handle_info({hexadecimal_item,"ubt.addr",#{event:=changed,value:=Text}},State) ->
    case find_node_by_pos(State) of
	false -> {noreply, State};
	Node ->
	    try list_to_integer(Text, 16) of
		Value ->
		    CobId = State#state.selected_eff,
		    co_sdo_cli:send_sdo_set(CobId,?INDEX_BOOT_APP_ADDR,
					    0,Value),
		    Node1 = Node#{ app_addr=>Value },
		    State1 = update_node(Node1, State),
		    {noreply, State1}
	    catch
		error:_ ->
		    lager:error("bad hex app_addr ~p\n", [Text]),
		    {noreply, State}
	    end
    end;
handle_info({hexadecimal_item,"uart.ubt.addr",
	     #{event:=changed,value:=Text}},State) ->
    case find_uart_by_pos(State) of
	false -> {noreply, State};
	#{ uart := undefined } -> {noreply, State};
	Uart = #{ uart := U, ubt_info := Info, hold := true } ->
	    try list_to_integer(Text, 16) of
		Value ->
		    HexValue = integer_to_list(Value,16),
		    uart_ubt_command(U,["addr 0x",HexValue]),
		    Info1 = Info#{ app_addr => Value},
		    Uart1 = Uart#{ ubt_info => Info1},
		    State1 = update_uart(Uart1, State),
		    {noreply, State1}
	    catch
		error:_ ->
		    lager:error("bad hex addr ~p\n", [Text]),
		    {noreply, State}
	    end;
	_Uart ->
	    lager:error("Uart not in hold mode\n", []),
	    {noreply, State}	    
    end;
handle_info({select,_ID,#{event:=button_release}},State) ->
    %% ignore mouse release in node selection
    {noreply, State};
handle_info({switch,ID,#{event:=changed,value:=Value}},State) ->
    {SID,Si} = 
	case ID of
	    "pdb.pout.e1.onoff" -> {"pdb",1};
	    "pdb.pout.e2.onoff" -> {"pdb",2};
	    "pdb.pout.e3.onoff" -> {"pdb",3};
	    "pdb.pout.e4.onoff" -> {"pdb",4};
	    "pdb.aout.e5.onoff" -> {"pdb",5};
	    "pdb.aout.e6.onoff" -> {"pdb",6};
	    "pdb.dout.e7" -> {"pdb",7};
	    "pdb.dout.e8" -> {"pdb",8};
	    "pdb.dout.e9" -> {"pdb",9};
	    "pdb.dout.e10" -> {"pdb",10};
	     
	    "pds.pout.e1.onoff" -> {"pds",1};
	    "pds.pout.e2.onoff" -> {"pds",2};
	    "pds.pout.e3.onoff" -> {"pds",3};
	    "pds.pout.e4.onoff" -> {"pds",4};
	    "pds.pout.e5.onoff" -> {"pds",5};
	    "pds.pout.e6.onoff" -> {"pds",6};
	    "pds.pout.e7.onoff" -> {"pds",7};
	    "pds.pout.e8.onoff" -> {"pds",8};

	    "pdi.dout.e1" -> {"pdi",1};
	    "pdi.dout.e2" -> {"pdi",2};
	    "pdi.dout.e3" -> {"pdi",3};
	    "pdi.dout.e4" -> {"pdi",4};
	    "pdi.dout.e5" -> {"pdi",5};
	    "pdi.dout.e6" -> {"pdi",6};
	    "pdi.dout.e7" -> {"pdi",7};
	    "pdi.dout.e8" -> {"pdi",8};
	    _ -> {"",-1}
	end,
    if Si =/= -1 ->
	    SwitchState = get_switch_state(ID),
	    if SID =:= "pds", SwitchState =:= alarm ->
		    ?dbg("send_alarm_cnfrm\n", []),
		    send_alarm_cnfrm(State#state.selected_eff,Si);
	       true ->
		    ?dbg("switch send_digital2, ~s value=~w\n", [ID,Value]),
		    send_digital2(State#state.selected_eff,Si,Value)
	    end;
       true ->
	    ok
    end,
    {noreply, State};
%% send a reset and set hold mode
handle_info({button,[_,_,_|".hold"],#{event:=button_press}},State) ->
    case find_node_by_pos(State) of
	false ->
	    {noreply,State};
	Node ->
	    Serial = maps:get(serial,Node,0),
	    send_pdo1_tx(0, ?MSG_RESET, 0, Serial),
	    {noreply,State#state{hold_mode = true }}
    end;
%% leave boot mode
handle_info({button,[_,_,_|".go"],#{event:=button_press}},State) ->
    WDT = 1,
    {noreply,action_sdo(State,boot,?INDEX_UBOOT_GO,0,WDT)};
%%
%% locate firmware image given selected product
%% check that firmware is an upgrade or first firmware
%% must be an uapp.
%% start process disable all input (except flash dialog)
%% show progress remove flash dialog and enable input
%%
handle_info({button,[_,_,_|".upgrade"],#{event:=button_press}},State) ->
    case find_node_by_pos(State) of
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
handle_info({button,[_,_,_|".reset"],#{event:=button_press}},State) ->
    %% send a reset and set hold mode
    case find_node_by_pos(State) of
	false ->
	    {noreply,State};
	Node ->
	    Serial = maps:get(serial,Node,0),
	    send_pdo1_tx(0, ?MSG_RESET, 0, Serial),
	    {noreply,State#state{hold_mode = false }}
    end;

handle_info({button,[_,_,_|".setup"],#{event:=button_press}},State) ->
    case find_node_by_pos(State) of
	false ->
	    {noreply,State};
	Node ->
	    Status = maps:get(status,Node,undefined),
	    Serial = maps:get(serial,Node,0),
	    if Status =:= up ->
		    CobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
		    case selected_id(State) of
			"pdb" -> bridgeZone_setup(CobId,State);
			"pds" -> powerZone_setup(CobId,State);
			"pdi" -> ioZone_setup(CobId,State);
			"pdc" -> controlZone_setup(CobId,State);
			_ -> {noreply,State}
		    end;
	       true ->
		    {noreply,State}
	    end
    end;

handle_info({button,[_,_,_|".factory"],#{event:=button_press}},State) ->
    {noreply,action_sdo(State,up,?IX_RESTORE_DEFAULT_PARAMETERS,4,<<"daol">>)};

handle_info({button,[_,_,_|".save"],#{event:=button_press}},State) ->
    case find_node_by_pos(State) of
	false -> {noreply, State};
	Node -> 
	    Activity = erlang:system_time(micro_seconds) + ?SAVE_TIMEOUT,
	    Node1 = Node#{ activity => Activity },
	    State1 = update_node(Node1, State),
	    State2 = action_sdo(State1,up,?IX_STORE_PARAMETERS,1,<<"evas">>),
	    {noreply,State2}
    end;
handle_info({button,[_,_,_|".restore"],#{event:=button_press}},State) ->
    {noreply, action_sdo(State,up,?IX_RESTORE_DEFAULT_PARAMETERS,1,<<"daol">>)};

handle_info({button,"uart.lpc_sync",#{event:=button_press}},State) ->
    case find_uart_by_pos(State) of
	false ->
	    {noreply,State};
	Uart = #{ pos := I, uart := undefined, device := Device } ->
	    Baud = maps:get(baud,Uart,38400),
	    ?dbg("open device ~s @ ~w baud\n", [Device, Baud]),
	    case elpcisp:open(Device, Baud) of
		{ok,U} ->
		    State1 = uart_sync(Uart#{ uart := U}, State),
		    {noreply, State1};
		_Error ->
		    ?dbg("elpcisp open error ~p\n", [_Error]),
		    refresh_uart_row(status,I,error),
		    refresh_lpc_info(#{}),
		    {noreply, State}
	    end;
	Uart = #{ uart := U } when is_port(U) ->
	    State1 = uart_sync(Uart, State),
	    {noreply, State1}
    end;

handle_info({button,"uart.lpc_flash",#{event:=button_press}},State) ->
    ?dbg("FLASH\n",[]),
    case lists:keyfind(ihex,1,State#state.firmware) of
	{ihex,Firmware} ->
	    ?dbg("elpcisp flash firmware size ~p\n", 
		 [firmware_size(Firmware)]),
	    case find_uart_by_pos(State) of
		false ->
		    {noreply, State};
		#{ pos := I, uart := U, baud := Baud, lpc_type := DevType } 
		  when is_port(U) ->
		    ?dbg("DevType = ~p\n",[DevType]),
		    case elpcisp:unlock(U) of
			{ok,_} ->
			    ?dbg("Unlock ok\n",[]),
			    elpcisp:set_baud_rate(U,Baud,1),
			    refresh_uart_row(status,I,flash),
			    case flash_firmware(U, I, Firmware, DevType) of
				ok ->
				    refresh_uart_row(status,I,sync);
				_Error ->
				    ?dbg("elpcisp flash error ~p\n", [_Error]),
				    refresh_uart_row(status,I,error)
			    end;
			_Error ->
			    ?dbg("elpcisp unlock error ~p\n", [_Error]),
			    refresh_uart_row(status,I,error)
		    end,
		    {noreply, State};
		_ ->
		    {noreply, State}
	    end;
	_ ->
	    {noreply, State}
    end;

handle_info({button,"uart.lpc_go",#{event:=button_press}},State) ->
    case find_uart_by_pos(State) of
	false ->
	    {noreply, State};
	#{ uart := undefined } ->
	    {noreply, State};
	#{ pos := I, uart := U } ->
	    case elpcisp:go(U, 0) of
		{ok,_} -> refresh_uart_row(status,I,idle);
		_Error -> refresh_uart_row(status,I,error)
	    end,
	    {noreply, State}
    end;
handle_info({button,"uart.lpc_reset",#{event:=button_press}},State) ->
    case find_uart_by_pos(State) of
	false ->
	    {noreply, State};
	#{ uart := undefined } ->
	    {noreply, State};
	#{ pos := I, uart := U } ->	
	    case elpcisp:reset(U) of
		{ok,_} -> refresh_uart_row(status,I,idle);
		_Error -> refresh_uart_row(status,I,error)
	    end,
	    {noreply, State}
    end;
handle_info({button,"uart.ubt.hold",#{event:=button_press}},State) ->
    case find_uart_by_pos(State) of
	false ->
	    {noreply, State};
	#{ uart := undefined } ->
	    {noreply, State};
	Uart = #{ pos := I, uart := U } ->
	    %% try send "show vsn\n" and look for answer
	    case detect_uart_hold(U,1000) of
		true ->
		    ?dbg("HOLD=already\n",[]),
		    refresh_uart_row(status,I,hold),
		    Info = uart_ubt_info(U),
		    Uart1 = Uart#{ hold=>true, ubt_info=>Info },
		    refresh_uart_state(Uart1, State),
		    State1 = update_uart(Uart1, State),
		    {noreply, State1};
		false ->
		    %% check if DMX then uart is not enabled!!!
		    elpcisp:reset(U),
		    timer:sleep(500), %% wait for boot
		    case detect_uart_hold(U,1000) of
			true ->
			    ?dbg("HOLD=true\n",[]),
			    refresh_uart_row(status,I,hold),
			    Info = uart_ubt_info(U),
			    Uart1 = Uart#{ hold=>true, ubt_info=>Info },
			    refresh_uart_state(Uart1, State),
			    State1 = update_uart(Uart1, State),
			    {noreply, State1};
			false ->
			    ?dbg("HOLD=false\n",[]),
			    refresh_uart_row(status,I,open),
			    Uart1 = Uart#{ hold=>false, ubt_info=>#{} },
			    refresh_uart_state(Uart1, State),
			    State1 = update_uart(Uart1, State),
			    {noreply, State1}
		    end
	    end
    end;

handle_info({button,"uart.ubt.go",#{event:=button_press}},State) ->
    case find_uart_by_pos(State) of
	false ->
	    {noreply, State};
	#{ uart := undefined } ->
	    {noreply, State};
	Uart = #{ pos := I, uart := U, hold := true } when is_port(U) ->
	    uart_ubt_command(U, "go"),
	    refresh_uart_row(status,I,open),
	    Uart1 = Uart#{ hold=>false },
	    State1 = update_uart(Uart1, State),
	    {noreply, State1};
	_ ->
	    {noreply, State}
    end;
handle_info({button,"uart.ubt.reset",#{event:=button_press}},State) ->
    case find_uart_by_pos(State) of
	false ->
	    {noreply, State};
	#{ uart := undefined } ->
	    {noreply, State};
	Uart = #{ pos := I, uart := U, hold := true } when is_port(U) ->
	    uart_ubt_command(U, "reset"),
	    Uart1 = Uart#{ hold=>false },
	    refresh_uart_row(status,I,open),
	    State1 = update_uart(Uart1, State),
	    {noreply, State1};
	_ ->
	    {noreply, State}
    end;
handle_info({button,_ID,#{event:=button_release}},State) ->
    %% ignore button release
    {noreply,State};

handle_info({analog,ID,#{value:=Value}},State) ->
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

handle_info({node_flashed,ID,Nid,Status,_Result},State) ->
    State1 = set_by_cobid(Nid,status,Status,State),
    epxy:set(ID,[{value,0.0}]),
    %% if Result == error then signal in window
    {noreply, State1};

handle_info({timeout,Timer,echo_timeout}, State) 
  when Timer =:= State#state.echo_timer ->
    %% request response from all nodes
    Now = erlang:system_time(micro_seconds),
    State1 = node_activity(Now, State),
    send_pdo1_tx(0, ?MSG_ECHO_REQUEST, 0, 0),
    EchoTimer = erlang:start_timer(?ECHO_INTERVAL, self(), echo_timeout),
    {noreply, State1#state { echo_timer = EchoTimer }};

handle_info({uart_error,U,Reason}, State) ->
    case take_by_key(uart, U, State#state.uarts) of
	false ->
	    lager:warning("uart error ~p unknown device", 
			  [Reason]),
	    {noreply, State};
	Uart = #{ device := Device } ->
	    if Reason =:= enxio ->
		    lager:error("uart error ~p device ~s unplugged?", 
				[Reason,Device]);
	       true ->
		    lager:error("uart error ~p for device ~s", 
				[Reason,Device])
	    end,
	    elpcisp:close(U),
	    Uart1 = Uart#{ uart => undefined,
			   status => idle,
			   lpc_info => #{},
			   ubt_info => #{},
			   hold => false },
	    refresh_uart_state(Uart1, State),
	    State1 = update_uart(Uart1, State),
	    {noreply, State1}
    end;

handle_info(_Info, State) ->
    lager:debug("gordon_view: got info ~p\n", [_Info]),
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

%% try syncronize the uart
uart_sync(Uart=#{ uart:=U, pos:=I}, State) ->
    put(control, maps:get(control,Uart,false)),
    put(control_swap, maps:get(control_swap,Uart,false)),
    put(control_inv, maps:get(control_inv,Uart,false)),
    refresh_uart_row(status,I,sync),
    case elpcisp:sync_osc(U, 4, 1000, "12000") of
	{ok,_} ->
	    case elpcisp:read_device_type(U) of
		{ok,DevType} ->
		    Info = maps:from_list(elpcisp:info(U)),
		    Uart1 = Uart#{ uart => U,
				   status => sync,
				   hold => false,
				   lpc_info => Info,
				   ubt_info => #{},
				   lpc_type => DevType },
		    refresh_uart_state(Uart1, State),
		    update_uart(Uart1, State);
		_Error ->
		    elpcisp:close(U),
		    ?dbg("elpcisp read error ~p\n", [_Error]),
		    refresh_uart_row(status,I,error),
		    Uart1 = Uart#{ uart=>undefined,
				   status => error,
				   hold => false,
				   lpc_info => #{},
				   ubt_info => #{},
				   lpc_type => 0 },
		    update_uart(Uart1, State)
	    end;
	_Error ->
	    elpcisp:close(U),
	    ?dbg("elpcisp read error ~p\n", [_Error]),
	    refresh_uart_row(status,I,error),
	    Uart1 = Uart#{ uart => undefined,
			   status => error,
			   hold => false,
			   lpc_info => #{},
			   ubt_info => #{},
			   lpc_type => 0 },
	    update_uart(Uart1, State)
    end.

%% we sent show vsn lookup 0x1234abcd
detect_uart_hold(U, Timeout) ->
    uart_ubt_command(U, "show vsn"),
    uart_ubt_detect_hold_(U, Timeout).

uart_ubt_detect_hold_(U, Timeout) ->
    receive
	{uart, U, <<">", _Echo/binary>>} ->
	    ?dbg("FLUSH >~p\n", [_Echo]),
	    uart_ubt_detect_hold_(U, Timeout);
	{uart, U, <<"0x", _Vsn/binary>>} ->
	    ?dbg("VSN = ~p\n", [_Vsn]),
	    true;
	{uart, U, _Data} ->
	    ?dbg("FLUSH ~p\n", [_Data]),
	    uart_ubt_detect_hold_(U, Timeout)
    after
	Timeout -> false
    end.

uart_ubt_info(U) ->
    Serial   = uart_ubt_read_num(U, "serial"),
    Product  = uart_ubt_read_num(U, "product"),
    Datetime = uart_ubt_read_num(U, "datetime"),
    AppAddr  = uart_ubt_read_num(U, "addr"),
    AppVsn   = uart_ubt_read_num(U, "vsn"),
    #{ serial   => (Serial bsr 8),
       product  => Product,
       datetime => Datetime,
       app_addr => AppAddr,
       app_vsn  => AppVsn }.

uart_ubt_read_num(U, Item) ->
    uart_ubt_command(U, ["show"," ",Item]),
    uart_ubt_recv_num(U, 1000).

uart_ubt_recv_num(U, Timeout) ->
    receive
	{uart, U, <<">", _Echo/binary>>} ->
	    ?dbg("FLUSH COMMAND ~p\n", [_Echo]),
	    uart_ubt_recv_num(U, Timeout);
	{uart, U, (<<"0x",Hex/binary>>)} ->
	    Num = string:trim(binary_to_list(Hex)),
	    try list_to_integer(Num,16) of
		Value -> Value
	    catch
		error:_ -> -1
	    end;
	{uart, U, Bin=(<<C,_/binary>>)} when C >= $0, C =< $9 ->
	    Num = string:trim(binary_to_list(Bin)),
	    try list_to_integer(Num, 10) of
		Value -> Value
	    catch
		error:_ -> -1
	    end;
	{uart, U, _Data} ->
	    ?dbg("FLUSH ~p\n", [Data]),
	    uart_ubt_recv_num(U, Timeout)
    after
	Timeout -> -1
    end.

uart_ubt_command(U, Command) ->
    uart_ubt_command(U, Command, 250).

uart_ubt_command(U, Command, Timeout) ->
    uart:send(U, [Command,$\n]),
    %% try flush the echo output
    receive
	{uart, U, <<">", _Echo/binary>>} ->
	    ok
    after Timeout ->
	    ok
    end.

%% Product menu
product_menu() ->
    [ { "powerZone3.2",          ?PRODUCT_MK(?PRODUCT_PDS,16#0302)},
      { "powerZone3.2+DMX",      ?PRODUCT_MK(?PRODUCT_PDS_DMX,16#0302)},
      { "bridgeZone3.2",         ?PRODUCT_MK(?PRODUCT_PDB,16#0302)},
      { "bridgeZone3.2+DMX",     ?PRODUCT_MK(?PRODUCT_PDB_DMX,16#0302)},
      { "bridgeZone3.2+Kelly",   ?PRODUCT_MK(?PRODUCT_PDB_KELLY,16#0302)},
      { "bridgeZone3.2+Marinco", ?PRODUCT_MK(?PRODUCT_PDB_MARINCO,16#0302)},
      { "ioZone3.3-24",          ?PRODUCT_MK(?PRODUCT_PDI_24,16#0303)},
      { "ioZone3.3-12-Analog",   ?PRODUCT_MK(?PRODUCT_PDI_12,16#0303)},
      { "controlZone-80",        ?PRODUCT_MK(?PRODUCT_PDC_80,16#0202)},
      { "controlZone-08",        ?PRODUCT_MK(?PRODUCT_PDC_08,16#0202)},
      { "controlZone-44",        ?PRODUCT_MK(?PRODUCT_PDC_44,16#0202)},
      { "controlZone-71",        ?PRODUCT_MK(?PRODUCT_PDC_71,16#0202)},
      %% not current 
      {"powerZone2.2",           ?PRODUCT_MK(?PRODUCT_PDS,16#0202)},
      {"powerZone2.2+DMX",       ?PRODUCT_MK(?PRODUCT_PDS_DMX,16#0202)}
    ].

product(I) ->
    try lists:nth(I, product_menu()) of
	{_Text, Code} -> Code
    catch
	error:_ -> false
    end.

product_by_value(Value) ->
    keyindex(Value, 2, product_menu()).

keyindex(Value, KeyPos, TupleList) ->
    keyindex_(Value, KeyPos, TupleList, 1).

keyindex_(Value, KeyPos, [T|_], I) when element(KeyPos,T) =:= Value ->
    I;
keyindex_(Value, KeyPos, [_|Ts], I) ->
    keyindex_(Value, KeyPos, Ts, I+1);
keyindex_(_Value, _KeyPos, [], _I) ->
    0.

make_uart_list([]) ->
    [];
make_uart_list([{uart,I,Opts}|Us]) when is_integer(I), I>0 ->
    Device = proplists:get_value(device,Opts),
    Baud   = proplists:get_value(baud,Opts,38400),
    Control = proplists:get_value(control,Opts,false),
    ControlSwap = proplists:get_value(control_swap,Opts,false),
    ControlInv = proplists:get_value(control_inv,Opts,false),
    Uart = #{ pos => I, uart => undefined,
	      device => Device, baud => Baud,
	      control => Control, control_swap => ControlSwap,
	      control_inv => ControlInv, status => idle,
	      lpc_type => 0, lpc_info => #{}, hold => false,
	      ubt_info => #{} },
    refresh_uart_row(Uart),
    [Uart | make_uart_list(Us)].

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

refresh_uart_ubt_info(Info) when is_map(Info) ->
    Serial   = maps:get(serial,Info,undefined),
    Product  = maps:get(product,Info,undefined),
    Creation = maps:get(creation,Info,undefined),
    AppAddr  = maps:get(app_addr,Info,undefined),
    AppVsn   = maps:get(app_vsn,Info,undefined),
    epxy:set("uart.ubt.app_vsn", [{text,format_value(app_vsn,AppVsn)}]),
    epxy:set("uart.ubt.serial", [{text,format_value(serial,Serial)}]),
    set_product_menu("uart.ubt.product", Product),
    epxy:set("uart.ubt.creation", [{text,format_date(Creation)}]),
    epxy:set("uart.ubt.addr", [{text,format_value(app_addr,AppAddr)}]),
    ok.

refresh_lpc_info(Info) when is_map(Info) ->
    Vsn          = maps:get(version, Info, undefined),
    Product      = maps:get(product, Info, ""),
    FlashSize    = maps:get(flashSize, Info, undefined),
    RamSize      = maps:get(ramSize, Info, undefined),
    FlashSectors = maps:get(flashSectors, Info, undefined),
    MaxCopySize  = maps:get(maxCopySize, Info, undefined),
    Variant      = maps:get(variant, Info, undefined),
    
    epxy:set("uart.vsn",[{text,format_value(vsn,Vsn)}]),
    epxy:set("uart.product",[{text,Product}]),
    epxy:set("uart.flashSize",[{text,format_value(flashSize,FlashSize)}]),
    epxy:set("uart.ramSize",[{text,format_value(ramSize,RamSize)}]),
    epxy:set("uart.flashSectors",[{text,format_value(flashSectors,FlashSectors)}]),
    epxy:set("uart.maxCopySize",[{text,format_value(maxCopySize,MaxCopySize)}]),
    epxy:set("uart.variant",[{text,format_value(variant,Variant)}]),
    ok.


%%  upgrade iff state=boot and firmware is available and correct
%%  factory/save/hold iff state=up
%%  go state=boot
%%  setup state=up
%%
refresh_node_state(State) ->
    case find_node_by_pos(State) of
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
	undefined -> State;
	down -> State;
	free -> State;
	flash ->
	    disable_buttons(SID,["go","upgrade"]),
	    enable_buttons(SID,["reset"]),
	    State;
	boot ->
	    Serial = maps:get(serial,Node,0),
	    epxy:set(SID++".serial", [{text,format_value(serial,Serial)}]),
	    Product = maps:get(product, Node, 0),
	    case SID of
		"ubt" ->
		    set_product_menu("ubt.product", Product);
		_ ->
		    epxy:set(SID++".product", [{text,format_product(Product)}])
	    end,
	    Creation = maps:get(creation, Node, 0),
	    epxy:set(SID++".creation", [{text,format_date(Creation)}]),
	    AppAddr = maps:get(app_addr,Node,0),
	    epxy:set(SID++".addr", [{text,format_value(app_addr,AppAddr)}]),
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

refresh_uart_state(State) ->
    case find_uart_by_pos(State) of
	false -> State;
	Uart -> refresh_uart_state(Uart,State)
    end.

refresh_uart_state(Uart, State) ->
    refresh_uart_state(selected_id(State), Uart, State).

refresh_uart_state(SID, Uart, State) ->
    case Uart of
	#{ uart := U, lpc_info := LpcInfo, ubt_info := UbtInfo } 
	  when is_port(U) ->
	    enable_buttons(SID, ["lpc_sync", "lpc_reset"]),
	    case maps:get(hold, Uart, false) of
		true ->
		    disable_buttons(SID,  ["lpc_go", "lpc_flash"]),
		    disable_buttons("uart.ubt", ["hold"]),
		    enable_buttons("uart.ubt", ["go", "reset"]);
		false ->
		    enable_buttons(SID,  ["lpc_go"]),
		    case lists:keyfind(ihex,1,State#state.firmware) of
			{ihex,_Firmware} ->
			    enable_buttons(SID,  ["lpc_flash"]);
			_ ->
			    disable_buttons(SID,  ["lpc_flash"])
		    end,
		    enable_buttons("uart.ubt", ["hold"]),
		    disable_buttons("uart.ubt", ["go", "reset"])
	    end,
	    refresh_lpc_info(LpcInfo),
	    refresh_uart_ubt_info(UbtInfo);
	#{ lpc_info := LpcInfo, ubt_info := UbtInfo } ->
	    enable_buttons(SID,  ["lpc_sync"]),
	    disable_buttons(SID, ["lpc_go", "lpc_flash", "lpc_reset"]),
	    disable_buttons("uart.ubt", ["hold", "go", "reset"]),
	    refresh_lpc_info(LpcInfo),
	    refresh_uart_ubt_info(UbtInfo)
    end,
    State.

disable_buttons(ID, Bs) ->
    [begin 
	 ?dbg("disable button ~s\n", [ID++"."++B]),
	 epxy:set(ID++"."++B, [{font_color,white},{disabled,true}]) 
     end || B <- Bs].

enable_buttons(ID, Bs) ->
    [begin
	 ?dbg("enable button ~s\n", [ID++"."++B]),
	 epxy:set(ID++"."++B, [{font_color,black},{disabled,false}])
     end || B <- Bs].

match_uapp_firmware(Node, State) ->
    Prod = (maps:get(product,Node,0) bsr 16) band 16#ff,
    Product = case Prod of
		  ?PRODUCT_PDS -> powerZone;
		  ?PRODUCT_PDI -> ioZone;
		  ?PRODUCT_PDB -> bridgeZone;
		  ?PRODUCT_PDC -> controlZone;
		  _ -> undefined
	      end,
    Vsn = maps:get(vsn, Node, undefined),
    AppVsn = maps:get(app_vsn, Node, undefined),
    AppVersion = gordon_uapp:decode_app_vsn(AppVsn),
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
    case proplists:get_value(version,Match) of
	undefined -> 
	    match_uapp_vsn(AppVersion,Banks);
	Version ->
	    if AppVersion =:= undefined;
	       Version > AppVersion -> {true,{Version,Bs}};
	       true -> match_uapp_vsn(AppVersion, Banks)
	    end
    end;
match_uapp_vsn(_AppVersion, []) ->
    false.
    
action_sdo(State, RequiredStatus, Index, Si, Value) ->
    case find_node_by_pos(State) of
	false ->
	    State;
	Node ->
	    CurrentStatus = maps:get(status,Node,undefined),
	    Serial = maps:get(serial,Node,0),
	    ?dbg("CurrentStatus = ~w, RequiredState=~w, Serial=~6.16.0B\n",
		 [CurrentStatus,RequiredStatus,Serial]),
	    if CurrentStatus =:= RequiredStatus ->
		    CobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
		    co_sdo_cli:send_sdo_set(CobId, Index, Si, Value),
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

%% Filter callback
event_filter(filter,Event,_Widget,_Window,_XY,decimal) ->
    Valid = [$\r,$\e,$\t,$\b,$0,$1,$2,$3,$4,$5,$5,$6,$7,$8,$9],
    case Event of
	{key_press,Sym,_Mode,_Code} ->
	    lists:member(Sym, Valid);
	{key_release,Sym,_Mode,_Code} ->
	    lists:member(Sym, Valid);
	_ ->
	    true
    end;
event_filter(filter,Event,_Widget,_Window,_XY,hexadeicmal) ->
    Valid = [$\r,$\e,$\t,$\b,$0,$1,$2,$3,$4,$5,$5,$6,$7,$8,$9,
	     $a,$b,$c,$d,$e,$f,$A,$B,$C,$D,$E,$F],
    case Event of
	{key_press,Sym,_Mode,_Code} ->
	    lists:member(Sym, Valid);
	{key_release,Sym,_Mode,_Code} ->
	    lists:member(Sym, Valid);
	_ ->
	    true
    end;
event_filter(_Type,_Event,_Widget,_Window,_XY,_Arg) ->
    true.

deselect_row(undefined,_Pos, State) -> State;
deselect_row(_Tab,undefined, State) -> State;
deselect_row(Tab,Pos,State) ->
    case selected_id(Tab,Pos,State) of
	undefined ->
	    ok;
	SID ->
	    RowID = Tab++".r"++integer_to_list(Pos),
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
-define(CELL_NODE_SERIAL, 5).
-define(CELL_NODE_ID,     3).
-define(CELL_NODE_PROD,   9).
-define(CELL_NODE_VSN,    3).
-define(CELL_NODE_STATUS, 4).

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

    CellSum = ?CELL_NODE_SERIAL+?CELL_NODE_ID+
	?CELL_NODE_PROD+?CELL_NODE_VSN+
	?CELL_NODE_STATUS,
    row(ID,X,Y,CellSum*TxW+(1+1+1+1+1),H,true),

    X0 = 0,
    W0 = ?CELL_NODE_SERIAL*TxW,
    text_cell(ID++".serial", X0, Y, W0, H,
	      [{text,"Serial"},{halign,right}|Opts]),
    X1 = X0 + W0 + 1,
    W1 = ?CELL_NODE_ID*TxW,
    text_cell(ID++".id", X1, Y, W1, H, 
	      [{text,"ID"},{halign,right}|Opts]),
    X2 = X1 + W1 + 1,
    W2 = ?CELL_NODE_PROD*TxW,
    text_cell(ID++".product", X2, Y, W2, H,
	      [{text,"Product"},{halign,center}|Opts]),
    X3 = X2 + W2 + 1,
    W3 = ?CELL_NODE_VSN*TxW,
    text_cell(ID++".vsn", X3, Y, W3, H,
	      [{text,"Vsn"},{halign,center}|Opts]),
    X4 = X3 + W3 + 1,
    W4 = ?CELL_NODE_STATUS*TxW,
    text_cell(ID++".status", X4, Y, W4, H,
	      [{text,"Status"},{halign,center}|Opts]),
    X4 + W4 + 1.

%% install table row return next X location
table_row(Parent,I,TxW,TxH,_W) ->
    ID = Parent++[$.,$r|integer_to_list(I)], %% <id>.r<i>
    X = 0, Y = I*TxH, H = TxH,

    %% parent to table cells
    CellSum = ?CELL_NODE_SERIAL+?CELL_NODE_ID+
	?CELL_NODE_PROD+?CELL_NODE_VSN+
	?CELL_NODE_STATUS,
    row(ID,X,Y,CellSum*TxW+(1+1+1+1+1),H,false),
    epxy:add_callback(ID,select,?MODULE),
    
    %% and now the cells
    X0 = 0,
    W0 = ?CELL_NODE_SERIAL*TxW,
    text_cell(ID++".serial", X0, 0, W0, H,
	      [{text,""},{halign,right}]),
    X1 = X0 + W0 + 1,
    W1 = ?CELL_NODE_ID*TxW,
    text_cell(ID++".id", X1, 0, W1, H,
	      [{text,""},{halign,right}]),
    X2 = X1 + W1 + 1,
    W2 = ?CELL_NODE_PROD*TxW,
    text_cell(ID++".product", X2, 0, W2, H,
	      [{text,""},{halign,center}]),
    X3 = X2 + W2 + 1,
    W3 = ?CELL_NODE_VSN*TxW,
    text_cell(ID++".vsn", X3, 0, W3, H,
	      [{text,""},{halign,center}]),
    X4 = X3 + W3 + 1,
    W4 = ?CELL_NODE_STATUS*TxW,
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
-define(CELL_UART_DEV,    12).
-define(CELL_UART_BAUD,    5).
-define(CELL_UART_CONTROL, 1).
-define(CELL_UART_SWAP,    1).
-define(CELL_UART_INVERT,  1).
-define(CELL_UART_STATUS,  4).


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

    CellSum = ?CELL_UART_DEV+?CELL_UART_BAUD+?CELL_UART_CONTROL+
	?CELL_UART_SWAP+?CELL_UART_INVERT+?CELL_UART_STATUS,
    row(ID,X,Y,CellSum*TxW+(1+1+1+1+1),H,true),

    X0 = 0,
    W0 = ?CELL_UART_DEV*TxW,
    text_cell(ID++".device", X0, Y, W0, H,
	      [{text,"Device"},{halign,center}|Opts]),
    X1 = X0 + W0 + 1,
    W1 = ?CELL_UART_BAUD*TxW,
    text_cell(ID++".baud", X1, Y, W1, H, 
	      [{text,"Baud"},{halign,center}|Opts]),
    X2 = X1 + W1 + 1,
    W2 = ?CELL_UART_CONTROL*TxW,
    text_cell(ID++".control", X2, Y, W2, H,
	      [{text,"C"},{halign,center}|Opts]),
    X3 = X2 + W2 + 1,
    W3 = ?CELL_UART_SWAP*TxW,
    text_cell(ID++".swap", X3, Y, W3, H,
	      [{text,"W"},{halign,center}|Opts]),
    X4 = X3 + W3 + 1,
    W4 = ?CELL_UART_INVERT*TxW,
    text_cell(ID++".invert", X4, Y, W4, H,
	      [{text,"I"},{halign,center}|Opts]),

    X5 = X4 + W4 + 1,
    W5 = ?CELL_UART_STATUS*TxW,
    text_cell(ID++".status", X5, Y, W5, H,
	      [{text,"Status"},{halign,center}|Opts]),
    X5 + W5 + 1.

%% install table row return next X location
uart_table_row(Parent,I,TxW,TxH,_W) ->
    ID = Parent++[$.,$r|integer_to_list(I)], %% <id>.r<i>
    X = 0, Y = I*TxH, H = TxH,

    %% parent to table cells
    CellSum = ?CELL_UART_DEV+?CELL_UART_BAUD+?CELL_UART_CONTROL+
	?CELL_UART_SWAP+?CELL_UART_INVERT+?CELL_UART_STATUS,
    row(ID,X,Y,CellSum*TxW+(1+1+1+1+1),H,false),
    epxy:add_callback(ID,select,?MODULE),
    
    %% and now the cells, smaller font for device (to make it fit)
    DeviceFont = [{name,"Arial"},{slant,roman},{weight,bold},{size,12}],
    X0 = 0,
    W0 = ?CELL_UART_DEV*TxW,
    text_cell(ID++".device", X0, 0, W0, H,
	      [{text,""},{halign,center},{font,DeviceFont}]),
    X1 = X0 + W0 + 1,
    W1 = ?CELL_UART_BAUD*TxW,
    text_cell(ID++".baud", X1, 0, W1, H,
	      [{text,""},{halign,center}]),
    X2 = X1 + W1 + 1,
    W2 = ?CELL_UART_CONTROL*TxW,
    text_cell(ID++".control", X2, 0, W2, H,
	      [{text,""},{halign,center}]),
    X3 = X2 + W2 + 1,
    W3 = ?CELL_UART_SWAP*TxW,
    text_cell(ID++".swap", X3, 0, W3, H,
	      [{text,""},{halign,center}]),
    X4 = X3 + W3 + 1,
    W4 = ?CELL_UART_INVERT*TxW,
    text_cell(ID++".invert", X4, 0, W4, H,
	      [{text,""},{halign,center}]),
    X5 = X4 + W4 + 1,
    W5 = ?CELL_UART_STATUS*TxW,
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
    [{font,Font}] = epxy:get("dummy", [font]),
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
	      {disabled, true},
	      {color,black},{x,-1},{y,0},{width,W+2},{height,H+1}]).

%% bridgeZone layout
bridgeZone(X,Y,_W,_H) ->
    XGap = ?TOP_XGAP,
    YGap = ?GROUP_FONT_SIZE,
    Y0 = YGap,
    X1 = XGap,
    ID = "pdb",

    {X11,Y1,_W0,H0} = tagged_text("pdb.app_vsn", "Version", X1, Y0, 0),
    {_,_,_,_} = tagged_text("pdb.uapp_vsn", "Upgrade", X11+XGap, Y0, 0),

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
    XGap = ?TOP_XGAP,
    YGap = ?GROUP_FONT_SIZE,
    Y0 = YGap,
    X1 = XGap,
    X2 = X1+64,
    ID = "pdi",

    {X11,Yn,_W0,H0} = tagged_text("pdi.app_vsn", "Version", X1, Y0, 0),
    {_,_,_,_} = tagged_text("pdi.uapp_vsn", "Upgrade", X11+XGap, Y0, 0),
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
    XGap = ?TOP_XGAP,
    YGap = ?GROUP_FONT_SIZE,
    Y0 = YGap,
    X1 = XGap,
    ID = "pds",

    {X11,Y1,_W0,H0} = tagged_text("pds.app_vsn", "Version", X1, Y0, 0),
    {_,_,_,_} = tagged_text("pds.uapp_vsn", "Upgrade", X11+XGap, Y0, 0),

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

controlZone(X,Y,_W,_H) ->
    XGap = ?TOP_XGAP,
    YGap = ?GROUP_FONT_SIZE,
    Y0 = YGap,
    X1 = XGap,
    %% X2 = X1+64,
    ID = "pdc",

    {X11,Y1,_W0,H0} = tagged_text("pdc.app_vsn", "Version", X1, Y0, 0),
    {_,_,_,_} = tagged_text("pdc.uapp_vsn", "Upgrade", X11+XGap, Y0, 0),
    %% Din x 4 (row Y3,column=X1)
    {_,Y2,W1,H1} = din_group("pdc.din", 1, 8, X1, Y1+YGap),

    {_,Y21,W11,H11} = ein_group("pdc.ein", 1, 8, X1+W1+XGap, Y1+YGap),

    {W2,H2} = add_buttons(ID, X1, max(Y2,Y21)+YGap),

    Wt = XGap+max(W1+XGap+W11,W2+XGap)+XGap,
    Ht = YGap+H0+YGap+max(H1,H2)+YGap+H2+YGap,

    group_rectangle(ID,"controlZone",X,Y,Wt,Ht,all),

    ok.

%%
%% uBoot mode dialog
%% Buttons 
%%    Go  Upgrade Reset
%%
uBoot(X,Y,_W,_H) ->
    XOffs = 4,
    XGap = ?TOP_XGAP,
    YGap = ?GROUP_FONT_SIZE,
    Y0 = YGap,
    X1 = XGap,
    ID = "ubt",

    TW = 64,
    {X11,Y1,W0,H0} = tagged_text("ubt.app_vsn", "Version", X1, Y0, TW),
    {_,_,W00,_} = tagged_text("ubt.uapp_vsn", "Upgrade", X11+XGap, Y0, 0),

    {_,Y2,W1,H1} = edit_text("ubt.serial", "Serial", X1, Y1+YGap, TW),
    epxy:set("ubt.serial", [{max_length,6},
			    {filter,{?MODULE,event_filter,[decimal]}}]),
    epxy:add_callback("ubt.serial",decimal_item,?MODULE),

    {_,Y3,W2,H2} = product_menu("ubt.product", "Product", X1, Y2+YGap, TW),

    {_,Y4,W3,H3} = edit_text("ubt.creation", "Creation", X1, Y3+YGap, TW),
    epxy:set("ubt.creation", [{max_length,6},
				   {filter,{?MODULE,event_filter,[decimal]}}]),
    epxy:add_callback("ubt.creation",decimal_item,?MODULE),

    {_,Y5,W4,H4} = edit_text("ubt.addr", "Address", X1, Y4+YGap, TW),
    epxy:set("ubt.addr", [{max_length,8},
			  {filter,{?MODULE,event_filter,[hexadecimal]}}]),
    epxy:add_callback("ubt.addr",hexadecimal_item,?MODULE),

    {W5,H5} = add_uboot_buttons(ID, X1, Y5+YGap),

    Wt = XGap+lists:max([W0+W00,W1,W2,W3,W4,W5])+XGap+XOffs*2,
    Ht = YGap+lists:sum([H0,H1,H2,H3,H4,H5])+6*YGap,

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
%% Also uart.ubt
%% hold uboot by sending a number (0.5s interval) of \n 
%% after reset. (wait for ">")
%% then send serial commands to set
%% serial, product, creation, address
%% may also send go and reset serial commands "go\n" and "reset\n"
%% refresh by issue "show\n" and parse result
%% "  serial: 0x31105501\n
%%   product: 0x00010202\n
%%  datetime: 1538308276\n
%%      addr: 0x00010000\n
%%       vsn: 0x000283d8\n"
%%
uartBoot(X,Y,_W,_H) ->
    XOffs = 4,
    XGap = ?TOP_XGAP,
    YGap = ?GROUP_FONT_SIZE,
    TW = 96,
    X1 = XGap,

    YY0 = YGap,
    XX0 = XGap,
    {_,YY1,WW0,HH0} = tagged_text("uart.ubt.app_vsn","Version",X1,YY0,TW),

    {_,YY2,WW1,HH1} = edit_text("uart.ubt.serial","Serial", X1, YY1+YGap, TW),
    epxy:set("uart.ubt.serial", [{max_length,6},
				 {filter,{?MODULE,event_filter,[decimal]}}]),
    epxy:add_callback("uart.ubt.serial",decimal_item,?MODULE),

    {_,YY3,WW2,HH2} = product_menu("uart.ubt.product","Product",X1,YY2+YGap,TW),

    {_,YY4,WW3,HH3} = edit_text("uart.ubt.creation","Creation",X1,YY3+YGap,TW),
    epxy:set("uart.ubt.creation", [{max_length,6},
				   {filter,{?MODULE,event_filter,[decimal]}}]),
    epxy:add_callback("uart.ubt.creation",decimal_item,?MODULE),

    {_,YY5,WW4,HH4} = edit_text("uart.ubt.addr","Address",X1,YY4+YGap,TW),
    epxy:set("uart.ubt.addr", [{max_length,8},
			       {filter,{?MODULE,event_filter,[hexadecimal]}}]),
    epxy:add_callback("uart.ubt.addr",hexadecimal_item,?MODULE),

    {WW5,HH5} = add_urt_buttons("uart.ubt", XX0, YY5+YGap),

    WWt = XGap+lists:max([WW0,WW1,WW2,WW3,WW4,WW5])+XGap+XOffs*2,
    HHt = YGap+lists:sum([HH0,HH1,HH2,HH3,HH4,HH5])+5*YGap,

    Y0 = YGap+?TOP_YGAP,
    H8 = HHt,
    group_rectangle("uart.ubt","uBoot",X1,Y0,WWt,H8,false),

    Y11 = Y0+H8+YGap,
    {_,Y1,W0,H0} = tagged_text("uart.vsn", "Version", X1, Y11, TW),
    {_,Y2,W1,H1} = tagged_text("uart.product", "Product", X1, Y1+YGap, TW),
    {_,Y3,W2,H2} = tagged_text("uart.flashSize", "FlashSize", X1, Y2+YGap, TW),
    {_,Y4,W3,H3} = tagged_text("uart.ramSize", "RamSize", X1, Y3+YGap, TW),
    {_,Y5,W4,H4} = tagged_text("uart.flashSectors", "Sectors", X1, Y4+YGap, TW),
    {_,Y6,W5,H5} = tagged_text("uart.maxCopySize", "MaxCopySize",X1,Y5+YGap,TW),
    {_,Y7,W6,H6} = tagged_text("uart.variant", "Variant", X1, Y6+YGap,TW),
    {W7,H7} = add_lpc_buttons("uart", X1, Y7+YGap),

    Wt = XGap+lists:max([W0,W1,W2,W3,W4,W5,W6,W7])+XGap,
    Ht = YGap+lists:sum([H0,H1,H2,H3,H4,H5,H6,H7])+8*YGap,

    W9 = max(Wt,WWt),
    H9 = Ht+HHt+?TOP_YGAP,
    group_rectangle("uart","lpcBoot",X,Y,W9,H9,all),

    ok.

%% add hold and go buttons
add_lpc_buttons(ID, X, Y) ->
    W = ?BUTTON_WIDTH,
    H = ?BUTTON_HEIGHT,
    YGap = ?BUTTON_YGAP,
    XGap = ?BUTTON_XGAP,
    X0 = X,
    X1 = X0 + W + XGap,
    X2 = X1 + W + XGap,
    X3 = X2 + W + XGap,
    Y0 = Y,
    add_text_button(ID++".lpc_go",      "Go",      X0, Y0, W, H),
    add_text_button(ID++".lpc_flash",   "Flash",   X1, Y0, W, H),
    add_text_button(ID++".lpc_reset",   "Reset",   X2, Y0, W, H),
    add_text_button(ID++".lpc_sync",    "Sync",    X3, Y0, W, H),
    {4*W+4*XGap,1*H+1*YGap}.

%% add hold and go buttons
add_uboot_buttons(ID, X, Y) ->
    W = ?BUTTON_WIDTH,
    H = ?BUTTON_HEIGHT,
    YGap = ?BUTTON_YGAP,
    XGap = ?BUTTON_XGAP,
    X0 = X,
    X1 = X0 + W + XGap,
    X2 = X1 + W + XGap,
    Y0 = Y,
    add_text_button(ID++".go",      "Go",      X0, Y0, W, H),
    add_text_button(ID++".upgrade", "Upgrade", X1, Y0, W, H),
    add_text_button(ID++".reset",   "Reset",   X2, Y0, W, H),
    {3*W+1*XGap,1*H+1*YGap}.

%% add uboot / uart buttons
add_urt_buttons(ID, X, Y) ->
    W = ?BUTTON_WIDTH,
    H = ?BUTTON_HEIGHT,
    YGap = ?BUTTON_YGAP,
    XGap = ?BUTTON_XGAP,
    X0 = X,
    X1 = X0 + W + XGap,
    X2 = X1 + W + XGap,
    Y0 = Y,
    add_text_button(ID++".hold",    "Hold",    X0, Y0, W, H),
    add_text_button(ID++".go",      "Go",      X1, Y0, W, H),
    add_text_button(ID++".reset",   "Reset",   X2, Y0, W, H),
    {3*W+1*XGap,1*H+1*YGap}.

%% add hold and go buttons
add_buttons(ID, X, Y) ->
    W = ?BUTTON_WIDTH,
    H = ?BUTTON_HEIGHT,
    YGap = ?BUTTON_YGAP,
    XGap = ?BUTTON_XGAP,
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
    YGap   = ?GROUP_YGAP,
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
    YGap   = ?GROUP_YGAP,
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
    YGap   = ?GROUP_YGAP,
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

ein_group(ID, Chan0, Chan1, X0, Y0) ->
    Step = if Chan0 < Chan1 -> 1; true -> -1 end,
    XLeft  = 12, XRight = 12,
    YTop   = 12, YBot   = 12,
    YGap   = ?GROUP_YGAP,
    {Y2,W2} = lists:foldl(
		fun(Chan, {Yi,Wi}) ->
			Num = (Chan - min(Chan1,Chan0))+1,
			{_,Y1,W1,_H1} = ein(ID,Chan,Num,XLeft,Yi),
			{Y1+YGap, max(Wi,W1)}
		end, {YTop,0}, lists:seq(Chan0,Chan1,Step)),
    Y3 = Y0+(Y2-YGap)+YBot,
    H = Y3-Y0,
    W = XLeft+W2+XRight,
    group_rectangle(ID,"Encoder",X0,Y0,W,H,false),
    {X0,Y3,W,H}.

aload_group(ID, Chan0, Chan1, X0, Y0) ->
    Step = if Chan0 < Chan1 -> 1; true -> -1 end,
    XLeft  = 12, XRight = 12,
    YTop   = 12, YBot   = 12,
    YGap   = ?GROUP_YGAP,
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
    YGap   = ?GROUP_YGAP,
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
    YGap   = ?GROUP_YGAP,
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
    onoff_switch_with_label(ID,Num,X,Y).

din(ID0,Chan,Num,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 24, H = ?DIN_FONT_SIZE+2,
    LW = 12,
    FontSpecL = [{name,"Arial"},{slant,roman},{size,?LABEL_FONT_SIZE}],
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
    W = 40, H = ?AIN_FONT_SIZE+4,  %% extra to match pout
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

%% encoder input
ein(ID0,Chan,Num,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    W = 40, H = ?AIN_FONT_SIZE+4,  %% extra to match pout
    LW = 12,
    FontSpecL = [{name,"Arial"},{weight,medium},{size,?AIN_FONT_SIZE}],
    FontSpec = [{name,"Arial"},{weight,bold},{size,?AIN_FONT_SIZE}],
    epxy:new(ID,[{type,value},
		 {halign,center},{valign,center},
		 {x,X+LW},{y,Y},{width,W},{height,H},
		 {children_first,false},
		 {font,FontSpec},
		 {fill,solid},{color,white},
		 {format,"~w"},
		 {value,0}
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
    W = 40, H = ?ALOAD_FONT_SIZE+4,
    LW = 12,
    FontSpecL = [{name,"Arial"},{weight,medium},{size,?LABEL_FONT_SIZE}],
    FontSpec = [{name,"Arial"},{weight,bold},{size,?ALOAD_FONT_SIZE}],
    epxy:new(ID,
	     [{type,value},
	      {halign,center},{valign,center},
	      {x,X+LW},{y,Y},{width,W},{height,H},
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
    onoff_slider_with_label(ID,Num,X,Y,lightBlue).

pout(ID0,Chan,Num,X,Y) ->
    ID = ID0++[$.,$e|integer_to_list(Chan)],
    onoff_slider_with_label(ID,Num,X,Y,lightGreen).

onoff_switch_with_label(ID,Num,X,Y) ->
    FontSpecL = [{name,"Arial"},{slant,roman},{size,?LABEL_FONT_SIZE}],
    FontSpec  = [{name,"Arial"},{weight,bold},{size,?ONOFF_FONT_SIZE}],
    W = ?ONOFF_WIDTH, 
    H = ?ONOFF_HEIGHT,
    LW = 12,
    epxy:new(ID,
	     [{type,switch},
	      {halign,center},
	      {x,X+LW},{y,Y},{width,W},{height,H},
	      {shadow_x,2},{shadow_y,2},{children_first,false},
	      {round_h,?ONOFF_ROUND_WH},{round_w,?ONOFF_ROUND_WH},
	      {font,FontSpec},
	      {fill,solid},{color,lightgray},
	      {text,"OFF"}
	     ]),
    epxy:add_callback(ID,switch,?MODULE),
    epxy:new(ID++".label",
	     [{type,text},
	      {font,FontSpecL},
	      {font_color,black},
	      {x,-LW},{y,0},
	      {width,10},{height,H},
	      {halign,left},
	      {text,integer_to_list(Num)}]),
    {X,Y+H,W+LW,H}.

onoff_slider_with_label(ID,Num,X,Y,Color) ->
    W = ?SLIDER_WIDTH+?ONOFF_WIDTH+16, 
    H = ?ONOFF_HEIGHT,
    LW = 12,
    FontSpecL = [{name,"Arial"},{weight,medium},{size,?LABEL_FONT_SIZE}],
    FontSpec = [{name,"Arial"},{weight,bold},{size,?ONOFF_FONT_SIZE}],
    epxy:new(ID,[{type,slider},
		 {x,X+LW+?ONOFF_WIDTH+16},{y,Y+2},
		 {width,?SLIDER_WIDTH},{height,?SLIDER_HEIGHT},
		 {fill,solid},{color,Color},
		 {min,0},{max,65535},
		 {orientation, horizontal},
		 {border, 1},
		 {topimage, knob_medium()}
		]),
    epxy:add_callback(ID,analog,?MODULE),
    ID1 = ID++".onoff",
    epxy:new(ID1,[{type,switch},
		  {halign,center},
		  {x,-(?ONOFF_WIDTH+16)},{y,-4},
		  {width,?ONOFF_WIDTH},{height,?ONOFF_HEIGHT},
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
			   {width,LW},{height,?ONOFF_HEIGHT},
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

%% Tagged text
product_menu(ID,TagText,X,Y,TagWidth) ->
    MenuFontSpec = [{name,"Arial"},{size,10}],
    {ok,MenuFont} = epx_font:match(MenuFontSpec),
    FontSpec = [{name,"Arial"},{slant,roman},{size,?GROUP_FONT_SIZE}],
    {ok,Font} = epx_font:match(FontSpec),
    {Wt,Ht} = epx_font:dimension(Font,TagText),
    XOffs = max(Wt+4,TagWidth),
    H = Ht+2,
    W = 8*10,
    epxy:new(ID,
	     [{type,menu},
	      {items,[Text||{Text,_ProductCode}<-product_menu()]},
	      {font, MenuFont},
	      {border,2},
	      {border_color,black},
	      {color, gray}, {fill, solid},
	      {x,X+XOffs}, {y,Y}]),
    epxy:add_callback(ID,menu,?MODULE),
    epxy:new(ID++".tag",
	     [{type,text},
	      {font,Font},
	      {font_color, black},
	      {text,TagText},
	      {color,white},{fill,solid},
	      {valign,center},
	      {halign,left},
	      {x,-XOffs},{y,0},
	      {height,Ht}
	     ]),
    {X+XOffs+W+2,Y+H,XOffs+W+2,H}.

tagged_text(ID,TagText,X,Y,TagWidth) ->
    tagged_text(ID,TagText,X,Y,TagWidth,true,false).

edit_text(ID,TagText,X,Y,TagWidth) ->
    tagged_text(ID,TagText,X,Y,TagWidth,false,true).

tagged_text(ID,TagText,X,Y,TagWidth,Disabled,Edit) ->
    FontSpec = [{name,"Arial"},{slant,roman},{size,?GROUP_FONT_SIZE}],
    {ok,Font} = epx_font:match(FontSpec),
    {Wt,Ht} = epx_font:dimension(Font,TagText),
    XOffs = max(Wt+4,TagWidth),
    H = Ht+2,
    W = 8*10,
    epxy:new(ID,
	     [{type,text},
	      {font,Font},
	      {edit,Edit},
	      {disabled,Disabled},
	      {font_color, black},
	      {text,""},
	      {color,white},{fill,solid},
	      {valign,center},
	      {halign,left},
	      {x,X+XOffs},{y,Y},
	      {height,Ht}, {width,W}
	     ]),
    BorderColor = if Edit -> blue; true -> lightgray end,
    epxy:new(ID++".border",
	     [{type,rectangle},
	      {disabled, true},
	      {border,1},
	      {border_color,BorderColor},
	      {x,-1},{y,-1},
	      {width,W+2},{height,H+2}]),
    epxy:new(ID++".tag",
	     [{type,text},
	      {font,Font},
	      {font_color, black},
	      {text,TagText},
	      {color,white},{fill,solid},
	      {valign,center},
	      {halign,left},
	      {x,-XOffs},{y,0},
	      {height,Ht}
	     ]),
    {X+XOffs+W+2,Y+H,XOffs+W+2,H}.

send_digital2(CobId, Si, Value) ->
    send_pdo2_tx(CobId,?MSG_DIGITAL,Si,Value).

send_analog2(CobId, Si, Value) ->
    send_pdo2_tx(CobId,?MSG_ANALOG,Si,Value).

send_alarm_cnfrm(CobId, Si) ->
    Value = ((CobId band 16#ffffff) bsl 8) bor Si,
    send_pdo1_tx(0, ?MSG_ALARM_CNFRM, 0, Value).

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
	    ?dbg("mpdo1 index=~w si=~w, Value=~w\n",[Index,Si,Value]),
	    node_message(CobID, Index, Si, Value, State);
	_ ->
	    ?dbg("unhandled pdo1_tx CobID=~w data=~w\n", [CobID,Data]),
	    {noreply, State}
    end.

%% request to node
sdo_rx(CobID,Bin,State) ->
    case Bin of
	?ma_ccs_initiate_download_request(N,E,S,_Index,_SubInd,Data) when 
	      E =:= 1->
	    _Value = sdo_value(S,N,Data),
	    ?dbg("sdo_rx: CobID=~s, SET index=16#~s, si=~w, value=~w\n", 
		 [integer_to_list(CobID,16),
		  integer_to_list(_Index,16),_SubInd,_Value]);

	?ma_ccs_initiate_upload_request(_Index,_SubInd) ->
	    ?dbg("sdo_rx: CobID=~s, GET index=16#~s, si=~w\n", 
		 [integer_to_list(CobID,16),
		  integer_to_list(_Index,16),_SubInd]);

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
	    ?dbg("sdo_tx: CobId=~s, SET RESP index=16#~s, si=~w\n",
		 [integer_to_list(CobId,16),
		  integer_to_list(Index,16),SubInd]),
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
	    ?dbg("sdo_tx: CobId=~s, GET RESP index=16#~s, si=~w, value=~w\n", 
		 [integer_to_list(CobId,16),
		  integer_to_list(Index,16),SubInd,Value]),
	    State1 = set_value_by_cobid(CobId,Index,SubInd,Value,State),
	    {noreply,State1};

	?ma_abort_transfer(Index,SubInd,Code) ->
	    ?dbg("sdo_tx: CobId=~s, ABORT index=~s, si=~w, code=~w\n",
		 [integer_to_list(CobId,16),
		  integer_to_list(Index,16),
		  SubInd,Code]),
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
		CobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
		WDT = 0,
		co_sdo_cli:send_sdo_set(CobId, ?INDEX_UBOOT_HOLD, 0, WDT),
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
	      CobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
	      request_node_info(CobId, false)
      end),
    State2 = refresh_node_state(State1),
    {noreply,State2}.

node_running(_CobId, Serial, State) ->
    %% ?dbg("Node ~6.16.0B running\n", [Serial]),
    case take_node_by_serial(Serial, State#state.nodes) of
	false ->
	    spawn(
	      fun() ->
		      CobId = ?XNODE_ID(Serial) bor ?COBID_ENTRY_EXTENDED,
		      request_node_info(CobId,
					CobId =:= State#state.selected_eff)
	      end),
	    {noreply, State};
	{value,N,Ns} ->
	    Now = erlang:system_time(micro_seconds),
	    {noreply, State#state { nodes=[N#{activity=>Now}|Ns] }}
    end.

request_node_info(CobId, Refresh) ->
    ?dbg("request_node_info CobId = ~8.16.0B\n", [CobId]),
    co_sdo_cli:send_sdo_get(CobId, ?INDEX_ID, 0),
    co_sdo_cli:send_sdo_get(CobId, ?IX_IDENTITY_OBJECT,
			    ?SI_IDENTITY_PRODUCT),
    co_sdo_cli:send_sdo_get(CobId, ?IX_IDENTITY_OBJECT,
			    ?SI_IDENTITY_REVISION),
    co_sdo_cli:send_sdo_get(CobId, ?INDEX_BOOT_APP_VSN, 0),
    co_sdo_cli:send_sdo_get(CobId, ?INDEX_BOOT_APP_ADDR, 0),
    co_sdo_cli:send_sdo_get(CobId, ?INDEX_BOOT_VSN, 0),
    if Refresh ->
	    send_pdo1_tx(0, ?MSG_REFRESH, 0, 0);
       true ->
	    ok
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
	?MSG_ALARM_CNFRM_ACK ->
	    SID = selected_id(State),
	    ?dbg("MSG_ALARM_CNFRM_ACK: [~s], si=~w, value=~w\n",
		 [SID,Si,Value]),
	    case SID of
		"pds" ->
		    send_digital2(State#state.selected_eff,Si,0);
		_ ->
		    ok
	    end;
	?MSG_ALARM ->
	    SID = selected_id(State),
	    ?dbg("MSG_ALARM: [~s], si=~w, value=~w\n",
		 [SID,Si,Value]),
	    case SID of
		"pds" ->
		    %% _Loc = (Value bsr 16) band 16#ff,
		    %% _OutLoc = (Value bsr 8) band 16#ff,
		    Cause = case Value band 16#ff of
				?ALARM_CAUSE_OK    -> "";
				?ALARM_CAUSE_FUSE  -> "Fuse";
				?ALARM_CAUSE_SHORT -> "Short";
				?ALARM_CAUSE_LOW   -> "Low";
				?ALARM_CAUSE_HIGH  -> "Load";
				%% Global cause Si should = 0
				?ALARM_CAUSE_OVERLOAD -> "Overload";
				?ALARM_CAUSE_HOT -> "Hot";
				?ALARM_CAUSE_LOW_BAT -> "Bat";
				?ALARM_CAUSE_LEVEL   -> "Vin";
				_ -> ""
			    end,
		    ?dbg("Alarm output=~w, cause = ~s\n", [Si,Cause]),
		    case Si of
			1 -> switch_state("pds.pout.e1.onoff",Cause);
			2 -> switch_state("pds.pout.e2.onoff",Cause);
			3 -> switch_state("pds.pout.e3.onoff",Cause);
			4 -> switch_state("pds.pout.e4.onoff",Cause);
			5 -> switch_state("pds.pout.e5.onoff",Cause);
			6 -> switch_state("pds.pout.e6.onoff",Cause);
			7 -> switch_state("pds.pout.e7.onoff",Cause);
			8 -> switch_state("pds.pout.e8.onoff",Cause);
			0 -> ok; %% fixme display global larm levels 
			_ -> ok
		    end;
		_ ->
		    ok
	    end;
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
		"pdc" ->
		    case Si of
			1 -> set_value("pdc.din.e1", Value);
			2 -> set_value("pdc.din.e2", Value);
			3 -> set_value("pdc.din.e3", Value);
			4 -> set_value("pdc.din.e4", Value);
			5 -> set_value("pdc.din.e5", Value);
			6 -> set_value("pdc.din.e6", Value);
			7 -> set_value("pdc.din.e7", Value);
			8 -> set_value("pdc.din.e8", Value);
			_ -> ok
		    end;
		_ ->
		    ok
	    end;

	?MSG_ENCODER ->
	    SID = selected_id(State),
	    ?dbg("MSG_ENCODER: [~s], si=~w, value=~w\n", 
		 [SID,Si,Value]),
	    case SID of
		"pdc" ->
		    case Si of
			1 -> set_value("pdc.ein.e1", Value);
			2 -> set_value("pdc.ein.e2", Value);
			3 -> set_value("pdc.ein.e3", Value);
			4 -> set_value("pdc.ein.e4", Value);
			5 -> set_value("pdc.ein.e5", Value);
			6 -> set_value("pdc.ein.e6", Value);
			7 -> set_value("pdc.ein.e7", Value);
			8 -> set_value("pdc.ein.e8", Value);
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

		"pds" -> %% fixme make pds send load update = 0.0 when turnoff
		    case Si of
			1 -> switch_state("pds.pout.e1.onoff",Value),
			     set_value("pds.aload.e33", 0.0);
			2 -> switch_state("pds.pout.e2.onoff",Value),
			     set_value("pds.aload.e34", 0.0);
			3 -> switch_state("pds.pout.e3.onoff",Value),
			     set_value("pds.aload.e35", 0.0);
			4 -> switch_state("pds.pout.e4.onoff",Value),
			     set_value("pds.aload.e36", 0.0);
			5 -> switch_state("pds.pout.e5.onoff",Value),
			     set_value("pds.aload.e37", 0.0);
			6 -> switch_state("pds.pout.e6.onoff",Value),
			     set_value("pds.aload.e38", 0.0);
			7 -> switch_state("pds.pout.e7.onoff",Value),
			     set_value("pds.aload.e39", 0.0);
			8 -> switch_state("pds.pout.e8.onoff",Value),
			     set_value("pds.aload.e40", 0.0);
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


node_activity(Now, State) ->
    node_activity_(State#state.nodes, Now, [], State).

node_activity_([N=#{pos:=Pos,activity:=Then,status:=Status}|Ns],
	       Now,Acc,State) ->
    if
	Status =:= flash ->
	    node_activity_(Ns,Now,[N|Acc],State);
	Now >= Then + ?REMOVE_TIMEOUT, Status =/= free ->
	    refresh_node_row(serial,Pos,""),
	    refresh_node_row(id,Pos,""),
	    refresh_node_row(product,Pos,""),
	    refresh_node_row(vsn,Pos,""),
	    refresh_node_row(status,Pos,free),
	    Acc1 = [N#{status=>free,serial=>0,id=>0,
		       product=>0,vsn=>0}|Acc],
	    if Pos =:= State#state.selected_pos ->
		    State1 = deselect_row(State#state.selected_tab,
					  State#state.selected_pos,State),
		    node_activity_(Ns, Now, Acc1, State1);
	       true ->
		    node_activity_(Ns, Now, Acc1, State)
	    end;

	Now >= Then + ?ACTIVITY_TIMEOUT, Status =/= free, Status =/= down ->
	    refresh_node_row(status,Pos,down),
	    Acc1 = [N#{status=>down}|Acc],
	    if Pos =:= State#state.selected_pos ->
		    State1 = deselect_row(State#state.selected_tab,
					  State#state.selected_pos,State),
		    node_activity_(Ns, Now, Acc1, State1);
	       true ->
		    node_activity_(Ns, Now, Acc1, State)
	    end;
	true ->
	    node_activity_(Ns,Now,[N|Acc],State)
    end;
node_activity_([],_Now,Acc,State) ->
    State#state { nodes=Acc }.

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
	_Uart -> "uart"
    end;
selected_id("nodes",Pos,State) ->
    case find_by_pos(Pos,State#state.nodes) of
	false -> undefined;
	Node ->
	    case maps:get(status,Node,undefined) of
		undefined -> undefined;
		free -> undefined;
		down -> undefined;
		up ->
		    case (maps:get(product,Node,0) bsr 16) band 16#ff of
			?PRODUCT_PDS -> "pds";
			?PRODUCT_PDI -> "pdi";
			?PRODUCT_PDB -> "pdb";
			?PRODUCT_PDC -> "pdc";
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
switch_state(ID,1) ->
    epxy:set(ID,[{color,green},{text,"ON"}]);
switch_state(ID,"") ->
    epxy:set(ID,[{color,lightgray},{text,"OFF"}]);
switch_state(ID,Alarm) ->
    epxy:set(ID,[{color,red},{text,Alarm}]).

get_switch_state(ID) ->
    [{text,Text}] = epxy:get(ID, [text]),
    case Text of
	"OFF" -> off;
	"ON" -> on;
	_ -> alarm
    end.

set_value(ID, Value) ->
    epxy:set(ID,[{value,Value}]).

set_status_by_serial(Serial, Status, Ns) ->
    Now = erlang:system_time(micro_seconds),
    case take_node_by_serial(Serial, Ns) of
	{value,N=#{ pos := Pos },Ns1} ->
	    refresh_node_row(status,Pos,Status),
	    [ N#{ status => Status, activity => Now } | Ns1];
	false ->
	    {N0=#{pos:=Pos},Ns1} = allocate_node(Ns),
	    refresh_node_row(status,Pos,Status),
	    refresh_node_row(serial,Pos,Serial),
	    N = N0#{ serial => Serial, status => Status, activity => Now },
	    [ N | Ns1 ]
    end.

set_value_by_cobid(CobId,Index,SubInd,Value,State) ->
    case Index of
	?INDEX_ID ->
	    set_by_cobid(CobId,id,Value,State);
	?IX_IDENTITY_OBJECT when SubInd =:= ?SI_IDENTITY_PRODUCT ->
	    Major = (Value bsr 8) band 16#ff,
	    Minor = Value band 16#ff,
	    State1 = set_by_cobid(CobId,vsn,{Major,Minor},State),
	    set_by_cobid(CobId,product,Value,State1);
	?IX_IDENTITY_OBJECT when SubInd =:= ?SI_IDENTITY_REVISION ->
	    set_by_cobid(CobId,creation,Value,State);
	?INDEX_BOOT_APP_VSN ->
	    set_by_cobid(CobId,app_vsn,Value,State);
	?INDEX_BOOT_APP_ADDR ->
	    set_by_cobid(CobId,app_addr,Value,State);
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
    Now = erlang:system_time(micro_seconds),
    case ?is_cobid_extended(CobID) of
	true ->
	    Serial = ?XNODE_ID(CobID),
	    Ns = State#state.nodes,
	    case take_node_by_serial(Serial, Ns) of
		false ->
		    {N0=#{pos:=Pos},Ns1} = allocate_node(Ns),
		    refresh_node_row(serial,Pos,Serial),
		    refresh_node_row(Key,Pos,Value),
		    N = N0#{serial=>Serial,activity=>Now,Key=>Value },
		    State#state { nodes=[N|Ns1]};
		{value,N0=#{ pos:=Pos},Ns1} ->
		    refresh_node_row(Key,Pos,Value),
		    N = N0#{ activity=>Now, Key => Value },
		    State#state { nodes=[N|Ns1]}
	    end;
	false ->
	    ID = ?NODE_ID(CobID),
	    Ns = State#state.nodes,
	    case take_node_by_id(ID, Ns) of
		false ->
		    {N0=#{pos:=Pos},Ns1} = allocate_node(Ns),
		    refresh_node_row(id,Pos,ID),
		    refresh_node_row(Key,Pos,Value),
		    N = N0#{ id=>ID, activity=>Now, Key=>Value },
		    State#state { nodes=[N|Ns1]};
		{value,N0=#{ pos:=Pos},Ns1} ->
		    refresh_node_row(Key,Pos,Value),
		    N = N0#{ activity=>Now, Key => Value },
		    State#state { nodes=[N|Ns1]}
	    end
    end.

allocate_node(Ns) ->
    case free_nodes(Ns) of
	{[F|Fs],Ns1} ->
	    {F,Fs++Ns1};
	{[],_} ->
	    L = length(Ns),
	    if L > ?NUM_TABLE_NODES ->
		    false;
	       true ->
		    {#{pos=>L+1},Ns}
	    end
    end.

free_nodes(Ns) ->
    free_nodes_(Ns,[],[]).

free_nodes_([N=#{status:=Status}|Ns],Free,Taken) ->
    if Status =:= free ->
	    free_nodes_(Ns, [N|Free], Taken);
       true ->
	    free_nodes_(Ns, Free, [N|Taken])
    end;
free_nodes_([],Free,Taken) ->
    Free1=lists:sort(fun(A,B) -> maps:get(pos,A) < maps:get(pos,B) end, Free),
    {Free1,Taken}.

set_product_menu(Item, Product) ->
    I = product_by_value(Product),
    epxy:set(Item, [{value,I}]).

%% set text value in nodes table
%% nodes.r<pos>.<key>
refresh_node_row(Key,Pos,Value) ->
    case is_node_item(Key) of
	true ->
	    ID = "nodes.r"++integer_to_list(Pos)++"."++atom_to_list(Key),
	    ?dbg("set_text: id=~s, value=~p\n", [ID,Value]),
	    epxy:set(ID,[{text,format_value(Key,Value)}]);
	false ->
	    ok
    end.

refresh_uart_row(#{ pos:=I, device:=Device, baud:=Baud,
		    control:=Control, control_swap:=ControlSwap,
		    control_inv:=ControlInv, status:=Status}) ->
    {Manuf0,Prod0,Serial0} = get_name_info(Device),
    Manuf = trunc_text(Manuf0,12),
    _Prod = trunc_text(Prod0,12),
    Serial = trunc_text(Serial0,8),
    refresh_uart_row(device,I,Manuf++" "++Serial),
    refresh_uart_row(baud,I,Baud),
    refresh_uart_row(control,I,uint1(Control)),
    refresh_uart_row(swap,I,uint1(ControlSwap)),
    refresh_uart_row(invert,I,uint1(ControlInv)),
    refresh_uart_row(status,I,Status).

refresh_uart_row(Key,Pos,Value) ->
    case is_uart_item(Key) of
	true ->
	    ID = "uarts.r"++integer_to_list(Pos)++"."++atom_to_list(Key),
	    ?dbg("refresh_uart_row: id=~s, value=~p\n", [ID,Value]),
	    epxy:set(ID,[{text,format_value(Key,Value)}]);
	false ->
	    ok
    end.

uint1(true) -> 1;
uint1(false) -> 0.

%% given
date10_to_utc_seconds(Date10) ->
    Month = max(1,min((Date10 div 100) rem 100, 12)),
    Year  = ((Date10 div 10000) rem 100)+2000,
    MaxDay = calendar:last_day_of_the_month(Year, Month),
    Day = max(1,min(Date10 rem 100, MaxDay)),
    Gs = calendar:datetime_to_gregorian_seconds({{Year,Month,Day},{0,0,0}}),
    Gs - 62167219200.

format_hex32(Value) when is_integer(Value) ->
    tl(integer_to_list(16#100000000+Value, 16)).

format_date(undefined) -> 
    "";
format_date(Value) when is_integer(Value) ->
    Date = calendar:gregorian_seconds_to_datetime(Value+62167219200),
    format_date(Date);
format_date({{Year,Mon,Day},{_H,_M,_S}}) ->
    lists:flatten(io_lib:format("~2..0w~2..0w~2..0w", 
				[max(0,Year-2000),Mon,Day])).

format_product(undefined) -> 
    "";
format_product(Code) ->
    case Code bsr 16 of
	?PRODUCT_PDS_DMX -> "powerZone+DMX";
	?PRODUCT_PDB_DMX -> "bridgeZone+DMX";
	?PRODUCT_PDB_KELLY -> "bridgeZone+Kelly";
	?PRODUCT_PDB_MARINCO -> "bridgeZone+Marinco";
	?PRODUCT_PDC_80 -> "controlZone-80";
	?PRODUCT_PDC_08 -> "controlZone-08";
	?PRODUCT_PDC_44 -> "controlZone-44";
	?PRODUCT_PDC_71 -> "controlZone-71";
	?PRODUCT_PDI_24 -> "ioZone-24";
	?PRODUCT_PDI_12 -> "ioZone-12-Analog";
	Code16 -> %% unknown variant
	    case Code16 band 16#ff of
		?PRODUCT_PDS -> "powerZone";
		?PRODUCT_PDC -> "controlZone";
		?PRODUCT_PDI -> "ioZone";
		?PRODUCT_PDB -> "bridgeZone";
		_ -> "Unknown"
	    end
    end.

format_value(Key, undefined) when is_atom(Key) -> 
    "";
format_value(serial,Value) when is_integer(Value) ->
    tl(integer_to_list(16#1000000+Value, 16));
format_value(device,Value) when is_list(Value) ->
    Value;
format_value(vsn,{Major,Minor}) ->
    integer_to_list(Major)++"."++integer_to_list(Minor);
format_value(app_vsn,Date={{_Year,_Mon,_Day},{_H,_M,_S}}) ->
    format_date(Date);
format_value(app_addr, Value) when is_integer(Value) -> 
    format_hex32(Value);
format_value(product, Product) when is_integer(Product) ->
    case Product bsr 16 of
	?PRODUCT_PDS_DMX -> "powerDMX";
	?PRODUCT_PDB_DMX -> "bridgeDMX";
	?PRODUCT_PDB_KELLY -> "bridgeKelly";
	?PRODUCT_PDB_MARINCO -> "bridgeMarinco";
	Type ->
	    case Type band 16#ff of
		?PRODUCT_PDS -> "powerZone";
		?PRODUCT_PDC -> "controlZone";
		?PRODUCT_PDI -> "ioZone";
		?PRODUCT_PDB -> "bridgeZone";
		_ -> "Unknown"
	    end
    end;
format_value(app_vsn,Value) when is_integer(Value) ->
    case Value of
	?CN_APP_ZERO  -> "zero";
	?CN_APP_EMPTY -> "empty";
	?CN_APP_NONE  -> "none";
	_ -> format_date(Value)
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

update_node(N=#{ pos := Pos }, State) ->
    case take_by_pos(Pos, State#state.nodes) of
	false -> State;  %% not present ignore
	{value,_Old,Ns} ->
	    State#state { nodes = [N|Ns]}
    end.

update_uart(U=#{ pos := Pos }, State) ->
    case take_by_pos(Pos, State#state.uarts) of
	false -> State;  %% not present ignore
	{value,_Old,Us} ->
	    State#state { uarts = [U|Us]}
    end.

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

find_node_by_pos(State) ->
    find_node_by_pos(State#state.selected_tab, State#state.selected_pos,
		     State#state.nodes).

find_node_by_pos("nodes",Pos, Ns) ->  find_by_key(pos, Pos, Ns);
find_node_by_pos(_Tab,_Pos,_Ns) -> false.

find_uart_by_pos(State) ->
    find_uart_by_pos(State#state.selected_tab, State#state.selected_pos,
		     State#state.uarts).

find_uart_by_pos("uarts",Pos, Ns) ->  find_by_key(pos, Pos, Ns);
find_uart_by_pos(_Tab,_Pos,_Ns) -> false.

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

take_by_pos(Pos, List) ->
    take_by_key(pos, Pos, List).

take_by_key(Key, Value, List) ->
    take_by_key(Key, Value, List, []).

take_by_key(Key, Value, [M|List], Ms) ->
    case M of
	#{ Key := Value } ->
	    {value,M,List++Ms};
	_ ->
	    take_by_key(Key, Value, List, [M|Ms])
    end;
take_by_key(_Key, _Value, [], _Ms) ->
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
		      ?dbg("Flash ok\n", []),
		      SELF ! {node_flashed,ID,Nid,boot,ok};
		  Error ->
		      ?dbg("Flash failed ~p\n", [Error]),
		      SELF ! {node_flashed,ID,Nid,boot,Error}
	      end
      end),
    State1.

flash_firmware(U,Pos,Firmware,DevType) ->
    BlockList = elpcisp:block_list(Firmware, DevType),
    flash_block_list(U,Pos,DevType, BlockList).

flash_block_list(U,Pos,DevType,[{Start,StartBlock,EndBlock,Data}|Bs]) ->
    {ok,_} = elpcisp:prepare_sector(U, StartBlock, EndBlock),
    {ok,_} = elpcisp:erase_sector(U, StartBlock, EndBlock),
    End = Start + byte_size(Data),
    ID = "uarts.r"++integer_to_list(Pos),
    elpcisp:flash_block(U, DevType, Start, Data,
			fun(A) ->
				V = (A-Start)/(End-Start),
				epxy:set(ID,[{value,V}])
			end),
    flash_block_list(U,Pos,DevType,Bs);
flash_block_list(_U,_Pos,_DevType, []) ->
    ok.

-define(T, 0,0,0,0).         %% transparent
-define(W, 255,255,255,255). %% white
-define(B, 255,0,0,0).       %% black


%% 22/22
knob_medium() -> 
    Pixels=
<<?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,?W,?W,?B,
  ?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B,?B
>>,
    make_pixels(Pixels,22,22,argb).

make_pixels(PixelData,W,H,Format) ->
    Pixmap = epx:pixmap_create(W,H,Format),
    epx:pixmap_put_pixels(Pixmap,0,0,W,H,Format,PixelData,[]),
    Pixmap.
