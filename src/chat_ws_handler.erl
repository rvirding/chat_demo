%% Copyright (c) 2011-2014 Robert Virding. All rights reserved.
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

-module(chat_ws_handler).

%% Cowboy callbacks.
-export([init/2,websocket_handle/3,websocket_info/3]).

%% Internal state
-record(state, {opts=[]}).

init(Req, Opts) ->
    chat_server:new_user(),			%A new user 
    {cowboy_websocket,Req,#state{opts=Opts}}.

%% Callback on received websockets data.

websocket_handle({text,Data}, Req, St) ->
    io:fwrite("ws text: ~p\n", [Data]),
    %% Handle message types.
    case Data of
	<<"msg ! ",Msg/binary>> ->		%A message
	    chat_server:send_message(Msg),
	    {ok,Req,St};
	<<"nick ! ",Nick/binary>> ->		%Set the nick
	    chat_server:set_nick(Nick),
	    {ok,Req,St};
	_  ->
	    {reply,{text,["status ! received '",Data,"'"]},Req,St}
    end;
websocket_handle(Other, Req, St) ->
    io:format("ws other: ~p\n", [Other]),
    {ok,Req,St}.

%% Callback on message from erlang.

websocket_info({chat_server,{message,Msg}}, Req, St) ->
    io:format("cs text: ~p\n", [Msg]),
    {reply,{text,["output ! ",Msg]},Req,St};
websocket_info(Other, Req, St) ->
    io:format("cs other: ~p\n", [Other]),
    {ok,Req,St}.
