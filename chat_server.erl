%% Copyright (c) 2011 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(chat_server).
-export([start_link/1,stop/0]).

%% Start misultin http server
start_link(Port) ->
    misultin:start_link([{port, Port},
			 {loop, fun(Req) -> handle_http(Req, Port) end},
			 {ws_loop, fun(Ws) -> handle_websocket(Ws) end},
			 {ws_autoexit, false}
			]).

%% stop misultin
stop() ->
    misultin:stop().

%% callback on request received
handle_http(Req, Port) ->
    io:fwrite("hh: ~p\n", [{self(),Req,Port}]),
    %% output
    case {Req:get(method),Req:resource([lowercase,urldecode])} of
	{'GET',["chat"]} ->			%Our chat program
	    {ok,File} = file:read_file("./chat.html"),
	    [Bef,Aft] = binary:split(File, <<"%%HOST:PORT%%">>),
	    Host = proplists:get_value('Host', Req:get(headers)),
	    Req:ok([Bef,Host,Aft]);
	{'GET',[File]} ->
	    case filelib:is_regular(File) of
		true ->
		    io:fwrite("hh: sending ~p\n", [File]),
		    Req:file(File);
		false ->
		    io:fwrite("hh: no file ~p\n", [File]),
		    Req:respond(404, [], ["no file: ",File])
	    end;
	_ ->
	    io:fwrite("hh: ignoring\n")
    end.

%% callback on received websockets data
handle_websocket(Ws) ->
    chat_backend:new_user(),			%Create user
    ws_loop(Ws).

ws_loop(Ws) ->
    %% io:fwrite("ws: ~p\n", [self()]),    
    receive
	{browser, Data} ->
	    case Data of
		"msg ! " ++ Msg ->
		    io:fwrite("msg: ~p\n", [Msg]),
		    chat_backend:send_message(Msg);
		"nick ! " ++ Nick ->
		    io:fwrite("nick: ~p\n", [Nick]),
		    chat_backend:set_nick(Nick);
		_ ->				%Unrecognised message
		    Ws:send(["status ! received '", Data, "'"])
	    end,
	    ws_loop(Ws);
	closed ->
	    %% Must ensure this process terminates.
	    io:format("The WebSocket was CLOSED!~n"),
	    closed;
	{chat_server,{message,Msg}} ->
	    %% Send message to output.
	    Ws:send(["output ! ",Msg]),
	    ws_loop(Ws);
	_Ignore ->
	    io:fwrite("ws: ~p\n", [{self(),_Ignore}]),
	    ws_loop(Ws)
    after 10000 ->
	    Ws:send("clock ! tick " ++ io_lib:fwrite("~p", [time()])),
	    ws_loop(Ws)
    end.
