%%
%% Start for gordon
%%
%%   on raspberry pi with Touch Screen start with -s gordon start_pi
%%   debug -s gordon start true
%%
-module(gordon).

-export([start/0, start/1]).
-export([start_rpi/0]).
-export([status/0]).
-export([firmware_info/0]).

-define(SERVER, gordon_srv).

start() -> start([false]).

start([TTYLogger]) ->
    (catch error_logger:tty(TTYLogger)),
    application:start(lager),
    %% load patched applications
    application:load(epx),
    application:load(gordon),
    Width  = application:get_env(gordon, screen_width, 800),
    Height = application:get_env(gordon, screen_height, 480),
    application:load(can),
    application:load(canopen),
    %% set environment before start
    application:set_env(can, wakeup, true),
    application:set_env(canopen, serial, no_master),
    %% now start
    application:ensure_all_started(canopen),
    can_udp:start(),
    application:start(elpcisp),
    application:ensure_all_started(epx),
    epxy:start_link([{width,Width}, {height,Height}]),
    application:start(gordon).

%% start rpi with touch screen
start_rpi() ->
    (catch error_logger:tty(false)),
    application:start(lager),
    %% load patched applications
    application:load(epx),
    application:load(gordon),
    Width  = application:get_env(gordon, screen_width, 800),
    Height = application:get_env(gordon, screen_height, 480),
    application:load(can),
    application:load(canopen),
    %% set environment before start
    application:set_env(epx, backend, "fb"),
    application:set_env(epx, pixel_format, 'argb/little'),
    application:set_env(epx, input_mouse_device, "/dev/input/event0"),
    application:set_env(can, wakeup, true),
    application:set_env(canopen, serial, no_master),
    %% make sure CANUSB is on full speed (+ other FTDI serial devices)
    lists:foreach(
      fun(UsbDev) ->
	      %% ignore output since only FTDI devices will return ok
	      os:cmd("setserial "++UsbDev++" low_latency")
      end, filelib:wildcard("/dev/ttyUSB*")),
    %% now start
    application:ensure_all_started(canopen),
    can_udp:start(),
    application:start(elpcisp),
    application:ensure_all_started(epx),
    epxy:start_link([{width,Width}, {height,Height}]),
    application:start(gordon).

%%--------------------------------------------------------------------
%% @doc
%%  Display "up" called from shell!!!
%% @end
%%--------------------------------------------------------------------

status() ->
    case erlang:whereis(?SERVER) of
	undefined -> io:format("faulty\n", []);
	Pid when is_pid(Pid) -> io:format("up\n", [])
    end.

firmware_info() ->
    gen_server:call(?SERVER, firmware_info).
