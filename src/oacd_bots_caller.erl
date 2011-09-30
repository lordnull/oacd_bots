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

%% @doc Make a call through the freeswitch dialplan.

-module(oacd_bots_caller).
-author("Micah Warren").
-behaviour(gen_server).

%% API
-export([
	start/1,
	start/2,
	start_link/1,
	start_link/2
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
	nodename :: atom(),
	uuid = "" :: string(),
	originate_job :: 'undefined' | string(),
	answered = false :: boolean(),
	parked = false :: boolean(),
	playback_file = "" :: string()
}).

%%====================================================================
%% API
%%====================================================================

-type(uuid_option() :: {'uuid', string()}).
-type(playback_file() :: {'playback_file', string()}).
-type(caller_id_option() :: {'caller_id', {string(), string()}}).
-type(originate() :: {'originate', string() | 'false'}).
-type(dialplan_option() :: {'dialplan', string()}).
-type(start_option() :: 
	uuid_option() |
	playback_file() |
	caller_id_option() |
	originate() |
	dialplan_option()
).
-type(start_options() :: [start_option()]).

-spec(start/1 :: (Node :: atom()) -> {'ok', pid()}).
start(Node) ->
	start(Node, []).

-spec(start/2 :: (Node :: atom(), Opts :: start_options()) -> {'ok', pid()}).
start(Node, Opts) ->
	gen_server:start(?MODULE, [Node, Opts], []).

-spec(start_link/1 :: (Node :: atom()) -> {'ok', pid()}).
start_link(Node) ->
	start_link(Node, []).

-spec(start_link/2 :: (Node :: atom(), Opts :: start_options()) -> {'ok', pid()}).
start_link(Node, Opts) ->
	gen_server:start_link(?MODULE, [Node, Opts], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Node, Opts]) ->
	InUuid = proplists:get_value(uuid, Opts),
	Originate = proplists:get_value(originate, Opts, false),
	Playback = proplists:get_value(playback_file, Opts, "sounds/sip_bot_message.aiff"),
	case {InUuid, Originate} of
		{undefined, false} ->
			ignore;
		{undefined, _String} ->
			{ok, UUID} = freeswitch:api(Node, create_uuid),
			init([Node, [{uuid, UUID} | Opts]]);
		{UUID, false} ->
			{ok, #state{nodename = Node, uuid = UUID}};
		{UUID, OriginateTarget} ->
			OriginateOpts = create_originate_opts(Opts),
			Dialplan = proplists:get_value(dialplan, Opts, "'&park()'"),
			OriginateString = make_originate(OriginateTarget, OriginateOpts, Dialplan),
			{ok, Bgid} = freeswitch:bgapi(Node, originate, OriginateString),
			become_handler(Node, UUID),
			lager:info("Caller for target ~s started with uuid ~s", [OriginateTarget, UUID]),
			lager:debug("Originate string:  ~s", [OriginateString]),
			{ok, #state{nodename = Node, uuid = UUID,
				originate_job = Bgid, playback_file = Playback}}
	end.

%%--------------------------------------------------------------------
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
	{reply, invalid, State}.

%%--------------------------------------------------------------------
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({call, {event, [UUID | Rest]}}, #state{uuid = UUID} = State) ->
	freeswitch:session_setevent(State#state.nodename, ['CHANNEL_ANSWER',
		'CHANNEL_BRIDGE', 'CHANNEL_UNBRIDGE', 'CHANNEL_HANGUP']),
	{noreply, State};
handle_info({call_event, {event, [UUID | Rest]}}, #state{uuid = UUID} = State) ->
	case_event_name([ UUID | Rest], State);
handle_info({bgok, Reply}, State) ->
	{noreply, State};
handle_info({bgerror, Err}, State) ->
	{noreply, State};
handle_info(channel_destroy, State) ->
	{stop, hangup, State};
handle_info(call_hangup, State) ->
	{stop, normal, State};
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

become_handler(Node, UUID) ->
	become_handler(Node, UUID, 0).

become_handler(Node, UUID, Count) when Count > 10 ->
	erlang:error(badsession);
become_handler(Node, UUID, Count) ->
	case freeswitch:handlecall(Node, UUID) of
		{error, _} ->
			timer:sleep(100),
			become_handler(Node, UUID, Count + 1);
		_ ->
			ok
	end.

create_originate_opts(Opts) ->
	create_originate_opts(Opts, []).

create_originate_opts([], Acc) ->
	lists:reverse(Acc);
create_originate_opts([{uuid, UUID} | Tail], Acc) ->
	create_originate_opts(Tail, [{"origination_uuid", UUID} | Acc]);
create_originate_opts([{caller_id, {Name, Number}} | Tail], Acc) ->
	CallerName = {"origination_caller_id_name", "'" ++ Name ++ "'"},
	CallerNumber = {"origination_caller_id_number", Number},
	NewAcc = [CallerName | [CallerNumber | Acc]],
	create_originate_opts(Tail, NewAcc);
create_originate_opts([{Key, Value} = Head | Tail], Acc) when is_list(Key), is_list(Value) ->
	create_originate_opts(Tail, [Head | Acc]);
create_originate_opts([_Head | Tail], Acc) ->
	create_originate_opts(Tail, Acc).

make_originate(Target, Options, Dialplan) ->
	DialOpts = stringify_dial_opts(Options),
	DialOpts ++ "loopback/" ++ Target ++ "/default " ++ Dialplan ++ " inline".

stringify_dial_opts(Options) ->
	stringify_dial_opts(Options, []).

stringify_dial_opts([], Acc) ->
	MidOpts = lists:reverse(Acc),
	Joined = string:join(MidOpts, ","),
	[${ | Joined] ++ "}";
stringify_dial_opts([{Key, Value} | Tail], Acc) ->
	NewHead = string:join([Key, Value], "="),
	NewAcc = [NewHead | Acc],
	stringify_dial_opts(Tail, NewAcc).

case_event_name([UUID | Rawcall], State) ->
	Ename = proplists:get_value("Event-Name", Rawcall),
	lager:debug("handling event ~s", [Ename]),
	case Ename of
		"CHANNEL_PARK" when State#state.answered == false ->
			% answer the mofo!
			%freeswitch:sendmsg(State#state.nodename, UUID,[{"call-command", "execute"},{"execute-app-name", "answer"}]),
			{noreply, State#state{parked = true}};
		"CHANNEL_ANSWER" when State#state.parked == false ->
			freeswitch:api(State#state.nodename, uuid_park, UUID),
			{noreply, State#state{answered = true}};
		"CHANNEL_PARK" ->
			freeswitch:sendmsg(State#state.nodename, UUID, [
				{"call-command", "execute"},
				{"event-lock", "true"},
				{"execute-app-name", "playback"},
				{"execute-app-arg", State#state.playback_file}
			]),
			%spew_vars(Rawcall),
			{noreply, State};
		_ ->
			% not much else I can think of.
			{noreply, State}
	end.

%spew_vars(Rawcall) ->
%	spawn(fun() -> spew_vars_loop(Rawcall) end).
%
%spew_vars_loop([]) ->
%	ok;
%spew_vars_loop([{Key, Val} | Tail]) ->
%	io:format("	~s:	~s~n", [Key, Val]),
%	spew_vars_loop(Tail).

%12:24:01 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_DATA";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:24:01 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_PARK";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_HANGUP";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_UNPARK";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_EXECUTE_COMPLETE";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_STATE";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_HANGUP_COMPLETE";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_STATE";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_DESTROY";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"
%12:25:00 [DEBUG] <0.61.0>@sip_bot:163 Event:  "CHANNEL_STATE";  UUID:  "31e5569e-0428-4230-a327-fd531e0ebecf"

	
	
	
	
	
	
	
