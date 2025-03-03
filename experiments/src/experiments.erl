-module(experiments).

-export([start/0]).

-behaviour(application).
-export([start/2]).
-export([stop/1]).

%%====================================================================
%% start
%%====================================================================

start() ->
    application:start(jsx),
    application:start(?MODULE).

%%====================================================================
%% application
%%====================================================================

start(_Type, _Args) ->
    {ok, Sup} = experiments_sup:start_link(),
    {ok, Sup}.

%%--------------------------------------------------------------------

stop(_State) ->
    ok.

