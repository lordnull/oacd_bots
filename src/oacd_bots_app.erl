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

-module(oacd_bots_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
		{ok, Freeswitch} = application:get_env(oacd_bots, freeswitch),
    oacd_bots_sup:start_link(Freeswitch, []).

stop(_State) ->
    ok.
