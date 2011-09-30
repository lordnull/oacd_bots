Overview
========

Using the Power of Erlang, use a single Freeswitch instance to act as many
different callers or external callers.

When developing an application using FreeSWITCH, often there will be cases 
where having one or two softphones simply won't be enough.  This application
can make many calls into FreeSWITCH's loopback interface.  If there is
an dialplan entry, it can also catch outgoing calls and create a 
controllable process for that channel as well.

In essence, instead of having a softphone with multiple lines to switch
between, or multiple softphones, this allows one to control freeswitch
via Erlang.

Building and Running
====================

Designed to aid in testing OpenACD, it does have it a dependency.  Future
versions will likely remove it.  OpenACD does not need to be running to
use this application.

FreeSWITCH must be built with mod_erlang_event enabled.

    ./rebar get-deps compile && ./devboot

That will get the dependencies, compile the application, and start it in
a development state.
