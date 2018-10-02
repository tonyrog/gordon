%%
%% Start for gordon
%%

-module(gordon).

-export([start/0]).
-export([start_rpi/0]).

start() ->
    %% (catch error_logger:tty(false)),
    application:start(lager),
    application:load(gordon),
    application:load(can),
    application:set_env(can, wakeup, true),
    application:start(can),
    can_udp:start(),
    application:start(elpcisp),
    Width  = application:get_env(gordon, screen_width, 800),
    Height = application:get_env(gordon, screen_height, 480),
    epxy:start_link([{width,Width}, {height,Height}]),
    application:start(gordon).

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
    application:start(elpcisp),
    %% can_usb:start(0),
    Width  = application:get_env(gordon, screen_width, 800),
    Height = application:get_env(gordon, screen_height, 480),
    epxy:start_link([{width,Width}, {height,Height}]),
    application:start(gordon).
