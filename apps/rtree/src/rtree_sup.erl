%%----------------------------------------------------------------
%% @author Carlos Roman <caroman@gmail.com>
%% @doc
%%   Supervisor behaviour implementation for rtree server.
%% @copyright 2013 Carlos Roman
%% @end
%%----------------------------------------------------------------
-module(rtree_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Start a child process, an rtree element.
%%
%% @spec start_child(Name, Capacity) -> void()
%% @end
%%--------------------------------------------------------------------
start_child(Name, Capacity) ->
    supervisor:start_child(?SERVER, [Name, Capacity]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [?CHILD(rtree_server, worker)]}}.


%% ===================================================================
%%  Internal functions
%% ===================================================================
