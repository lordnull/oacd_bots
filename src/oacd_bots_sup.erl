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
%%  The Initial Developer of the Original Code is Micah Warren.
%%  Portions created by Micah Warren are Copyright (C) 2011.  
%%  All Rights Reserved.
%%
%%  Contributor(s): Micah Warren.

-module(oacd_bots_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, StartArgs), {I, {I, start_link, StartArgs}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Freeswitch) when is_atom(Freeswitch) ->
	start_link(Freeswitch, []).

start_link(Freeswitch, Options) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, {Freeswitch, Options}). 

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({Freeswitch, Options}) ->
	FreeswitchManager = ?CHILD(oacd_bots_freeswitch_manager, worker, Freeswitch),
	Callers = ?CHILD(oacd_bots_caller_manager, worker, []),
	{ok, { {one_for_one, 5, 10}, [FreeswitchManager, Callers]} }.

