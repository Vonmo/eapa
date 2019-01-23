%%%-------------------------------------------------------------------
%% @doc erbloom top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eapa_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(Id, Module, Type, Args), #{id => Id,
                                         start => {Module, start_link, [Args]},
                                         restart => permanent,
                                         shutdown => infinity,
                                         type => Type,
                                         modules => [Module]}).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
