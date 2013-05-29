-module(rtree_server).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Server Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([
    start_link/0,
    stop/0,
    create/1,
    intersects/2
    load/2,
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Server call back functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Server initial state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {request_count = 0}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------------------------------------------------------------------------
%%% @doc Server start_link interface
%%% @spec start_link() -> {atom(ok), pid()}  | {atom(error), Reason::term()}
%%% @end
%%% ----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%%% ----------------------------------------------------------------------------
%%% @doc Server stop interface
%%% @spec stop() -> atom(ok)
%%% @end
%%% ----------------------------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop).

%%% ----------------------------------------------------------------------------
%%% @doc Server intersects interface
%%% @spec intersects(float(), float()) -> 
%%%   {atom(ok), bool()} | {atom(error), Reason::term()}
%%% @end
%%% ----------------------------------------------------------------------------
intersects(X, Y) ->
    gen_server:call(?MODULE, {intersects, X, Y}).

%%% ----------------------------------------------------------------------------
%%% @doc Server intersects interface
%%% @spec intersects(float(), float()) -> 
%%%   {atom(ok), bool()} | {atom(error), Reason::term()}
%%% @end
%%% ----------------------------------------------------------------------------
load(File, Name) ->
    gen_server:call(?MODULE, {load, File, Name}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EXPORTED FUNCTIONS/GEN_SERVER CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% ----------------------------------------------------------------------------
%%% @doc Handle start/start_link server callback function
%%% @spec init(atom(no_args)) -> {ok, state()}
%%% @end
%%% ----------------------------------------------------------------------------
init(no_args) ->
    process_flag(trap_exit, true),
    {ok, #state{request_count = 0}}.

%%% ----------------------------------------------------------------------------
%%% @doc Handle call server callback function
%%% @spec handle_call(Call::term(), From::{pid(), reference()}, state()) ->
%%%  {atom(reply), Reply, state()}
%%% @end
%%% ----------------------------------------------------------------------------
handle_call({intersects, X, Y}, _, State) ->
    case rtree:intersects(X, Y) of
        {ok, Bool} -> {reply, {ok, Bool}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call({load, File, Name}, _, State) ->
    case rtree:load(File, Name) of
        {ok, Bool} -> {reply, {ok, Bool}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end.


%%% ----------------------------------------------------------------------------
%%% @doc Handle cast server callback function
%%% @spec handle_cast(Cast::term(), state()) ->
%%%  {atom(stop), Reason::string(), state()}
%%% @end
%%% ----------------------------------------------------------------------------
handle_cast(stop, State) ->
    io:format("Cast: ~p~n", [State]),
    {stop, normal, State};
handle_cast(Cast, State) ->
    io:format("Cast: ~p~n", [State]),
    {stop, {"Can't not handle cast", Cast}, State}.

%%% ----------------------------------------------------------------------------
%%% @doc Handle info server callback function
%%% @spec handle_info(term(), state()) ->
%%% {atom(stop), Reason::string(), state()}
%%% @end
%%% ----------------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("Info: ~p~n", [State]),
    {stop, {"Can't not handle info", Info}, State}.

%%% ----------------------------------------------------------------------------
%%% @doc Code change server callback function
%%% @spec code_change(OldVsn::term(), state(), Extra::[term()]) ->
%%%  {atom(ok), state()}
%%% @end
%%% ----------------------------------------------------------------------------
code_change(_, State, _) -> 
    io:format("Code Change: ~p~n", [State]),
    {ok, State}.

%%% ----------------------------------------------------------------------------
%%% @doc Terminate server callback function
%%% @spec terminate(Reason::term(), State::state()) -> none()
%%% @end
%%% ----------------------------------------------------------------------------
terminate(shutdown, State) -> 
    io:format("Terminate: ~p~n", [State]);
terminate(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% rtree module
