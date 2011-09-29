%%  The contents of this file are subject to the Mozilla Public License
%%  Version 1.1 (the "License"); you may not use this file except in
%%  compliance with the License. You may obtain a copy of the License at
%%  http://www.mozilla.org/MPL/
%%
%%  Software distributed under the License is distributed on an "AS IS"
%%  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%  License for the specific language governing rights and limitations
%%  under the License.
%%
%%  The Original Code is "oacd_bots".
%%
%%  The Initial Developer of the Original Code is Micah Warren <micahw at 
%%  lordnull dot com>
%%  Portions created by Micah Warren <micahw at lordnull dot com> are 
%%  Copyright (C) 2011.  
%%  All Rights Reserved.
%%
%%  Contributor(s): Micah Warren <micahw at lordnull dot com>

-module(oacd_bots_freeswitch_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Fsnode) ->
	start_link(Fsnode, []).

start_link(Fsnode, Options) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {Fsnode, Options}, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Fsnode, Options}) ->
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
