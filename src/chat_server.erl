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

-module(chat_server).

-behaviour(gen_server).

%% API

-export([start_link/0,start/0,stop/0]).
-export([new_user/0,set_nick/1,send_message/1]).

%% Behaviour callbacks.
-export([init/1,terminate/2,
	 handle_call/3,handle_cast/2,handle_info/2,
	 code_change/3]).

-import(lists, [reverse/1,foreach/2]).

-record(st, {buf=new_buf(),users=[]}).		%Server state
-record(user, {pid,nick=[]}).			%User info

%% Management API

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local,?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% User API

new_user() ->
    gen_server:call(?MODULE, {new_user,self()}).

set_nick(Nick) ->
    gen_server:call(?MODULE, {set_nick,self(),Nick}).

send_message(Msg) ->
    gen_server:cast(?MODULE, {message,self(),Msg}).

%% Callbacks.

init(_) ->
    process_flag(trap_exit, true),
    {ok,#st{buf=new_buf(),users=[]}}.

terminate(_Reason, _St) ->
    ok.

handle_call(stop, _, St) ->			%Stop the server
    {stop,normal,ok,St};
handle_call({new_user,Pid}, _, St) ->		%Add a user
    link(Pid),					%Link to user
    Us = add_user(Pid, St#st.users),
    send_msgs(Pid, St#st.buf),
    {reply,ok,St#st{users=Us}};
handle_call({set_nick,Pid,Nick}, _, St) ->	%Set the nick for a user
    Us = set_nick(Nick, Pid, St#st.users),
    {reply,ok,St#st{users=Us}};
handle_call(_Other, _, St) ->			%Other messages
    {reply,{error,request},St}.

handle_cast({message,Pid,Text}, #st{users=Us}=St) ->
    Msg = build_msg(Pid, Text, Us),
    broadcast_msg(Msg, Us),
    Buf = buffer_msg(Msg, St#st.buf),
    {noreply,St#st{buf=Buf}};
handle_cast(_Other, St) ->			%Ignore unknown messages
    {noreply,St}.

handle_info({'EXIT',Pid,_}, St) ->		%User process has died
    Us = del_user(Pid, St#st.users),
    {noreply,St#st{users=Us}};
handle_info(_Other, St) ->			%Ignore unknown messages
    {noreply,St}.

code_change(_Vsn, St, _Extra) ->
    {ok,St}.

%% new_buf() -> Buffer.
%% get_msgs(Buffer) -> [Msg].
%% buffer_msg(Message, Buffer) -> Buffer.
%%  The message is a queue structure with a count. We save max 20
%%  messages in buffer. The buffer is {Count,Back,Front}. Messages are
%%  pushed onto the back and drop off the front. Like a queue.

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
%%  Work with user info.

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
