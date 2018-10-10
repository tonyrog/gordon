%%
%% Start for gordon
%%

-module(gordon).

-export([start/0]).
-export([start_rpi/0]).

start() ->
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
    %% now start
    application:ensure_all_started(canopen),
    can_udp:start(),
    application:start(elpcisp),
    application:ensure_all_started(epx),
    epxy:start_link([{width,Width}, {height,Height}]),
    application:start(gordon).
