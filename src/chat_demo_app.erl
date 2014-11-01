%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(chat_demo_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    One = {'_',
	   [
	    {"/", cowboy_static, {priv_file, chat_demo, "index.html"}},
	    {"/websocket", chat_ws_handler, []},
	    {"/static/[...]", cowboy_static, {priv_dir, chat_demo, "static"}}
	   ]},
    Dispatch = cowboy_router:compile([One]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
				[{env, [{dispatch, Dispatch}]}]),
    chat_demo_sup:start_link().

stop(_State) ->
	ok.
