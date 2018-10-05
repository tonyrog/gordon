%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    epxy virtual keyboard creator
%%% @end
%%% Created : 24 Jun 2018 by Tony Rogvall <tony@rogvall.se>

-module(gordon_keyboard).

-export([alpha/5, numeric/5]).    %% user widget
-compile(export_all).

-record(keyb,
	{
	 name,
	 width,         %% pixmap width
	 height,        %% pixmap height
	 pixmap,        %% keyboard image
	 keymap,        %% map from keysym to key rectangle
	 keyrows        %% map from coordinates to keysym
	}).

-record(layout,
	{
	 font,            %% font used to draw characters on keyboard	 
	 shift = false,   %% shift false,true,once
	 pressed = [],    %% currently pressed keys
	 keyb :: #keyb{}, %% current selected keyboard layout
	 keybs = []    %% [#keyb{}]
	}).

%%
%% epxy user type!
%%

numeric() ->
    [ ["1","2","3","+"],
      ["4","5","6","-"],
      ["7","8","9","BSP"],
      ["0","RET"]
    ].

alpha() ->
    [ ["Q","W","E","R","T","Y","U","I","O","P"],
      ["A","S","D","F","G","H","J","K","L"],
      ["SHFT", "Z","X","C","V","B","N","M","BSP"],
      ["123", "SPACE", "RET"]
    ].

%% Fixme make list of keyboards and switch with "123" "abc" keys
alpha1() ->
    [ ["1","2","3","4","5","6","7","8","9","0"],
      ["-","/",":",";","(",")","#","&","@","\""],
      ["+", ".",",","?","!","'","{","}","BSP"],
      ["abc", "SPACE", "RET"]
    ].

alpha(draw,_Event,W,Win,XY) -> keyboard_draw(W,Win,XY);
alpha(event,Event,W,Win,XY) -> keyboard_event(Event,W,Win,XY);
alpha(select,Pos,W,Win,XY) -> keyboard_select(Pos,W,Win,XY);
alpha(init,_What,W,_Win,_XY) ->
    keyboard_init(W, [{alpha,alpha()}, {numeric,alpha1()}]).

numeric(draw,_Event,W,Win,XY) -> keyboard_draw(W,Win,XY);
numeric(event,Event,W,Win,XY) -> keyboard_event(Event,W,Win,XY);
numeric(select,Pos,W,Win,XY) ->  keyboard_select(Pos,W,Win,XY);
numeric(init,_What,W,_Win,_XY) -> 
    keyboard_init(W, [{numeric,numeric()}]).

keyboard_init(W, Layout) ->
    U = keyboard_create(Layout),
    K = U#layout.keyb,
    epxy:widget_set(W,[{user_data,U},
		       {width,K#keyb.width},
		       {height,K#keyb.height}]).

-define(BUTTON_FONT_SIZE, 28).
-define(FONT_UP_COLOR,   16#000000).
-define(FONT_DOWN_COLOR, 16#ffffff).
-define(FILL_COLOR,    16#cfcfcf).
-define(BORDER_COLOR,  16#000000).
-define(KEYUP_COLOR,   16#efefef).
-define(KEYDOWN_COLOR, 16#101010).

%% -define(FONT_COLOR,    16#ff0000).
%% -define(FILL_COLOR,    16#ffffff).
%% -define(BORDER_COLOR,  16#000000).
%% -define(KEYUP_COLOR,   16#cfcfcf).
%% -define(KEYDOWN_COLOR, 16#1f1f1f).

keyboard_draw(W,Win,{X,Y}) ->
    [{image,Pixmap}]  = epxy:widget_get(Win, [image]),
    [{user_data,U},{border,B},{border_color,Bc}] = 
	epxy:widget_get(W, [user_data,border,border_color]),
    K = U#layout.keyb,
    epx:pixmap_copy_area(K#keyb.pixmap,Pixmap,0,0,X,Y,
			 K#keyb.width,
			 K#keyb.height,[]),
    if B > 0 ->
	    epx_gc:set_fill_style(none),
	    BColor = if Bc =:= undefined -> black; true -> Bc end,
	    epx_gc:set_fill_color(BColor),
	    epx:draw_rectangle(Pixmap,X,Y,K#keyb.width,K#keyb.height)
    end,
    lists:foreach(
      fun(Key) ->
	      Rect = find_keyrect(Key,K#keyb.keymap),
	      WinRect = offset_rect(X,Y,Rect),
	      String = key_string(Key),
	      draw_key(Pixmap,String,WinRect,U#layout.font,
		       ?KEYDOWN_COLOR,?FONT_DOWN_COLOR)
      end, U#layout.pressed).

%% Pos = select position
%% XY  = widget place position
keyboard_select({Xi,Yi,_Z},W,_Win,{X,Y}) ->
    [{user_data,U}] = epxy:widget_get(W, [user_data]),
    K = U#layout.keyb,
    case in_rect(Xi,Yi,X,Y,K#keyb.width,K#keyb.height) of
	true -> W;
	false -> false
    end.

keyboard_event(Event,W,Win,{X,Y}) ->
    case Event of
	{button_press,_Buttons,{Px,Py,_Pz}} ->
	    [{user_data,U}] = epxy:widget_get(W, [user_data]),
	    K = U#layout.keyb,
	    Rx = Px - X,
	    Ry = Py - Y,
	    case find_keyrow(Rx,Ry,K#keyb.keyrows) of
		false -> false;
		Key ->
		    Pressed1 = [Key|U#layout.pressed],
		    [{id,WinID}] = epxy:widget_get(Win,[id]),
		    case sym(Key,U#layout.shift) of
			shift ->
			    Shift = shift_toggle(U#layout.shift),
			    epxy:widget_set(W, [{user_data,U#layout{shift=Shift,pressed=Pressed1}}]);
			{switch,Name} ->
			    K1 = select_keyb(Name,U#layout.keybs),
			    epxy:widget_set(W, [{user_data,U#layout{keyb=K1}}]);
			
			Sym ->
			    Code = code(Key),
			    epxy:inject(WinID,{key_press,Sym,[],Code}),
			    epxy:widget_set(W, [{user_data,U#layout{pressed=Pressed1}}])
		    end
	    end;
	{button_release,_Buttons,{_Px,_Py,_Pz}} ->
	    [{user_data,U}] = epxy:widget_get(W, [user_data]),
	    lists:foreach(
	      fun(Key) ->
		      Sym = sym(Key,U#layout.shift),
		      Code = code(Key),
		      [{id,WinID}] = epxy:widget_get(Win,[id]),
		      epxy:inject(WinID,{key_release,Sym,[],Code})
	      end, U#layout.pressed),
	    Shift = shift_release(U#layout.shift),
	    epxy:widget_set(W, [{user_data,U#layout{shift=Shift,pressed=[]}}]);
	_ ->
	    W
    end.

-define(KEY_YOFFS, 4).  %% top & bottom offset
-define(KEY_XOFFS, 4).  %% left & right offset

-define(KEY_XGAP, 2).   %% gap between keys
-define(KEY_YGAP, 2).   

-define(KEY_XPAD, 4).   %% extra space in key
-define(KEY_YPAD, 4).

keyboard_create(Layout) ->
    FontSpec = [{name,"Arial"},{weight,bold},{size,?BUTTON_FONT_SIZE}],
    {ok,Font} = epx_font:match(FontSpec),
    {TxW,_} = epx_font:dimension(Font, "W"),
    {_,TxH} = epx_font:dimension(Font, "y"),
    Ascent = epx:font_info(Font, ascent),
    KW = TxW + ?KEY_XPAD,
    KH = TxH + ?KEY_YPAD,
    keyboard_create_(Layout,Font,Ascent,KW,KH,[]).

keyboard_create_([{Name,Keyb}|Layout],Font,Ascent,KW,KH,Acc) ->
    W = lists:max([rowlen(R) || R <- Keyb]),
    H = length(Keyb),
    Width  = W*KW+(W-1)*?KEY_XGAP + ?KEY_XOFFS*2,
    Height = H*KH+(H-1)*?KEY_YGAP + ?KEY_YOFFS*2,
    Pixmap = epx:pixmap_create(Width,Height),
    epx:pixmap_fill(Pixmap, ?FILL_COLOR),
    epx_gc:set_font(Font),
    {KeyRows,KeyMap} = map_create_(Keyb,Pixmap,0,Width,Ascent,KW,KH,[],#{}),
    K = #keyb { name=Name, pixmap=Pixmap, width=Width, height=Height,
		keymap=KeyMap, keyrows=KeyRows },
    keyboard_create_(Layout,Font,Ascent,KW,KH,[K|Acc]);
keyboard_create_([],Font,_Ascent,_KW,_KH,Acc) ->
    KeybList = lists:reverse(Acc),
    K = default_keyb(KeybList),
    #layout { font=Font, keyb=K, keybs=KeybList }.

default_keyb([K]) -> K;
default_keyb(KL) ->  select_keyb(default, KL).

select_keyb(Name, KL) ->
    case lists:keyfind(Name, #keyb.name, KL) of
	false -> hd(KL);
	K -> K
    end.

map_create_([Row|Rs],Pixmap,I,Width,Ascent,KW,KH,KeyRows,KeyMap) ->
    N = length(Row),
    RowWidth = rowlen(Row)*KW+N*?KEY_XGAP,
    X0 = (Width - RowWidth) div 2,
    Y0 = I*(KH+?KEY_YGAP) + ?KEY_YOFFS,
    {KeyRow,KeyMap1} = create_keys_(Row,Pixmap,X0,Y0,Ascent,KW,KH,[],KeyMap),
    map_create_(Rs,Pixmap,I+1,Width,Ascent,KW,KH,[KeyRow|KeyRows],KeyMap1);
map_create_([],_Pixmap,_I,_Width,_Ascent,_KW,_KH,KeyRows,KeyMap) ->
    {lists:reverse(KeyRows),KeyMap}.

create_keys_([Key|Ks],Pixmap,X,Y,Ascent,KW,KH,KeyRow,KeyMap) ->
    Js = key_width(Key),
    Width = Js*KW,
    Height = KH,
    Rect = {X,Y,Width,Height},
    String = key_string(Key),
    draw_key_rect(Pixmap,String,Rect,Ascent,?KEYUP_COLOR,?FONT_UP_COLOR),
    KeyMap1 = KeyMap#{ Key => Rect },
    KeyRow1 = [{X,Width,Key}|KeyRow],
    create_keys_(Ks,Pixmap,X+Width+?KEY_XGAP,Y,Ascent,KW,KH,KeyRow1,KeyMap1);
create_keys_([],_Pixmap,_X,Y,_Ascent,_KW,KH,KeyRow,KeyMap) ->
    Height = KH,
    {{Y,Height,lists:reverse(KeyRow)},KeyMap}.

draw_key(Pixmap, String, Rect, Font, KeyColor, FontColor) ->
    Ascent = epx:font_info(Font, ascent),
    epx_gc:set_font(Font),
    draw_key_rect(Pixmap, String, Rect, Ascent, KeyColor, FontColor).

draw_key_rect(Pixmap, String, {X,Y,W,H}, Ascent, KeyColor, FontColor) ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(KeyColor),
    epx:draw_roundrect(Pixmap,X,Y,W,H,4,4),

    epx_gc:set_fill_style(none),
    epx_gc:set_fill_color(?BORDER_COLOR),
    epx:draw_roundrect(Pixmap,X,Y,W,H,4,4),

    case String of
	shift ->
	    draw_symbol(Pixmap,X,Y,W,H,shift);
	backspace ->
	    draw_symbol(Pixmap,X,Y,W,H,backspace);
	_ ->
	    %% center text
	    {StringW,_} = epx_font:dimension(epx_gc:current(), String),
	    Offset = max(0,((W-StringW) div 2)),
	    epx_gc:set_foreground_color(FontColor band 16#ffffff),
	    epx:draw_string(Pixmap,X+Offset,Y+Ascent,String)
    end.

shift_toggle(false) -> true;
shift_toggle(true) -> false;
shift_toggle(once) -> false.

shift_release(once) -> false;
shift_release(true) -> true;
shift_release(false) -> false.

sym("BSP",_)   -> $\b;
sym("DEL",_)   -> delete;
sym("RET",_)   -> $\r;
sym("123",_)   -> {switch,numeric};
sym("abc",_)   -> {switch,alpha};
sym("SPACE",_) -> $\s;
sym("SHFT",_)  -> shift;
sym([C],false) -> string:to_lower(C);
sym([C],_) -> C.

%% keycode is platform stuff, so we inject 0
code(_Key) -> 0.

key_width("BSP")   -> 1;
key_width("DEL")   -> 2;
key_width("RET")   -> 2;
key_width("123")   -> 2;
key_width("abc")   -> 2;
key_width("SPACE") -> 4;
key_width("SHFT")  -> 1;  %% draw as symbol
key_width(_) -> 1.

key_string("BSP")   -> backspace;  %% symbol
key_string("DEL")   -> "DEL";      %% symbol
key_string("RET")   -> "RET";      %% symbol
key_string("123")   -> "123";      %% symbol
key_string("SPACE") -> "";         %% draw as blank
key_string("SHFT")  -> shift;      %% symbol
key_string(Key) -> Key.

rowlen(Keys) ->
    lists:sum([key_width(K) || K <- Keys]).

%% find key rect given key
find_keyrect(Key, KeyMap) ->
    maps:get(Key, KeyMap).

find_keyrow(X,Y,[{Yr,Hr,Row}|_Rows]) when Y >= Yr, Y < Yr+Hr ->
    find_keycol(X, Row);
find_keyrow(X,Y,[{Yr,Hr,_Row}|Rows]) when Y >= Yr+Hr ->
    find_keyrow(X,Y,Rows);
find_keyrow(_X,_Y,_) ->
    false.

find_keycol(X, [{Xr,Wr,Key}|_Row]) when X >= Xr, X < Xr+Wr ->
    Key;
find_keycol(X, [{Xr,Wr,_Key}|Row]) when X >= Xr+Wr ->
    find_keycol(X, Row);
find_keycol(_X, _) ->
    false.

offset_rect(X,Y,{Xr,Yr,Wr,Hr}) ->
    {X+Xr,Y+Yr,Wr,Hr}.

in_rect(X,Y,Xr,Yr,Wr,Hr) ->
    if X >= Xr, X < Xr + Wr, Y >= Yr, Y < Yr + Hr -> true;
       true -> false
    end.

draw_symbol(Pixmap,X,Y,W,H,shift) ->
    XOffs = 4,
    YOffs = 8,
    W0 = W-2*XOffs,
    H0 = H-2*YOffs,
    W2 = W0 div 2,
    W4 = W0 div 4,
    %% W8 = W0 div 8,
    X0 = X + XOffs,
    Y0 = Y + YOffs,
    H2 = H0 div 2,
    X1 = X0 + W2,
    X2 = X0 + (W0-1),
    Y1 = Y0 + H2,
    %% Y2 = Y0 + (H-1),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(black),
    epx:draw_triangle(Pixmap, {X1,Y0}, {X2,Y1}, {X0,Y1}),
    epx:draw_rectangle(Pixmap, X1-W4, Y1, W2, H2);

draw_symbol(Pixmap,X,Y,W,H,backspace) ->
    XOffs = 2,
    Xc = X + (W div 2),
    Yc = Y + (H div 2),
    W0 = W,
    H0 = H,
    W2 = W0 div 2,
    H2 = H0 div 2,
    W4 = W0 div 4,
    H4 = H0 div 4,
    W8 = W0 div 8,
    X0 = Xc - W4 + XOffs,
    Y0 = Yc - H4,
    X1 = X0 - W8 - XOffs,
    Y2 = Y0 + H2,
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(black),
    epx:draw_rectangle(Pixmap, X0, Y0, W2, H2),
    epx:draw_triangle(Pixmap, {X0,Y0}, {X1,Yc}, {X0,Y2});

draw_symbol(Pixmap,X,Y,W,H,{Sw,Sh,Format,PixelData}) ->
    Ox = max(0,((W-Sw) div 2)),    
    Oy = max(0,((H-Sh) div 2)),
    epx:pixmap_put_pixels(Pixmap,X+Ox,Y+Oy,Sw,Sh,Format,PixelData,[blend]).
