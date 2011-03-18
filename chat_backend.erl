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

-module(chat_backend).

-export([start_link/0,start/0,stop/0,debug/0]).
-export([new_user/0,set_nick/1,send_message/1]).

-import(lists, [reverse/1,foreach/2]).

-record(st, {buf=new_buf(),users=[]}).		%Server state
-record(user, {pid,nick=[]}).			%User info

start_link() ->
    %% Be chatty on death.
    proc_lib:spawn_link(fun () -> init() end).

start() ->
    %% Be silent on death.
    proc_lib:spawn(fun () -> init() end).

stop() ->
    chat_backend ! stop.

debug() ->
    chat_backend ! {self(),debug},
    receive
	{chat_backend,{debug_info,Dbg}} -> Dbg
    end.

%% User API
new_user() -> chat_backend ! {self(),new_user}.
set_nick(Nick) -> chat_backend ! {self(),{set_nick,Nick}}.
send_message(Msg) -> chat_backend ! {self(),{message,Msg}}.

init() ->
    register(chat_backend, self()),
    process_flag(trap_exit, true),
    loop(#st{buf=new_buf(),users=[]}).

loop(St0) ->
    receive
	{Pid,new_user} ->			%Add a new user
	    link(Pid),				%We link to user
	    Us = add_user(Pid, St0#st.users),
	    send_msgs(Pid, St0#st.buf),
	    loop(St0#st{users=Us});
	{Pid,{set_nick,Nick}} ->		%Set the nick
	    Us = set_nick(Nick, Pid, St0#st.users),
	    loop(St0#st{users=Us});
	{Pid,{message,Text}} ->			%Send a message
	    Msg = build_msg(Pid, Text, St0#st.users),
	    broadcast_msg(Msg, St0#st.users),
	    Buf = buffer_msg(Msg, St0#st.buf),
	    loop(St0#st{buf=Buf});
	{Pid,debug} ->				%Return debug info
	    Pid ! {chat_backend,{debug_info,St0}},
	    loop(St0);
	{'EXIT',Pid,_} ->
	    Us = del_user(Pid, St0#st.users),
	    loop(St0#st{users=Us});
	stop -> ok				%We're done
    end.

%% new_buf() -> Buffer.
%% get_msgs(Buffer) -> [Msg].
%% buffer_msg(Message, Buffer) -> Buffer.
%% The message is a queue structure with a count. We save max 20
%% messages in buffer.

new_buf() -> {0,[],[]}.

get_msgs({_,B,F}) -> F ++ reverse(B).

buffer_msg(Msg, {C,B,F}) when C < 20 ->
    {C+1,[Msg|B],F};
buffer_msg(Msg, {20,B,[_|F]}) -> {20,[Msg|B],F};
buffer_msg(Msg, {20,B,[]}) ->
    buffer_msg(Msg, {20,[],reverse(B)}).

%% broadcast_msg(Msg, [Users]) -> ok.
%% send_msgs(Pid, Buf) -> ok.

broadcast_msg(Msg, Us) ->
    foreach(fun (U) -> send_msg(U#user.pid, Msg) end, Us).

send_msgs(Pid, Buf) ->
    foreach(fun (M) -> send_msg(Pid, M) end, get_msgs(Buf)).

send_msg(Pid, Msg) -> Pid ! {chat_server,{message,Msg}}.

build_msg(Pid, Text, Users) ->
    [get_nick(Pid, Users),": ",Text].

%% add_user(Pid, Users) -> Users.
%% del_user(Pid, Users) -> Users.
%% set_nick(Nick, Pid, Users) -> Users.
%% get_nick(Pid, Users) -> Nick.
%% Work with user info.

add_user(Pid, Users) -> [#user{pid=Pid,nick="Anon"}|Users].

del_user(Pid, [#user{pid=Pid}|Us]) -> Us;
del_user(Pid, [U|Us]) -> [U|del_user(Pid, Us)];
del_user(_, []) -> [].

set_nick(Nick, Pid, [#user{pid=Pid}=U|Us]) ->
    [U#user{nick=Nick}|Us];
set_nick(Nick, Pid, [U|Us]) -> [U|set_nick(Nick, Pid, Us)];
set_nick(_, _, []) -> [].			%Be kind

get_nick(Pid, [#user{pid=Pid,nick=Nick}|_]) -> Nick;
get_nick(Pid, [_|Us]) -> get_nick(Pid, Us);
get_nick(_, []) -> "Unknown".			%Be kind
