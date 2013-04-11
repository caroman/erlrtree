-module(rtree_server).
-behaviour(gen_server).

%%%%%
%% Interface
%
-export([
    start_link/0,
    stop/0,
    intersects/2
    ]).

%%%%%
%% Gen Server part
%
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

-record(state, {request_count = 0}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%

%%%%%
%% @spec start_link() -> {atom(ok), pid()}  | {atom(error), Reason::term()}
%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%%%%%
%% @spec stop() -> atom(ok)
%
stop() ->
    gen_server:cast(?MODULE, stop).

%%%%%
%% @spec intersects(string(), string()) -> 
%   {atom(ok), bool()} | {atom(error), Reason::term()}
%
intersects(Latitude, Longitude) ->
    gen_server:call(?MODULE, {intersects, Latitude, Longitude}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
% EXPORTED FUNCTIONS/GE_SERVER CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%

%%%%%
%% @spec init(atom(no_args)) -> {ok, state()}
%
init(no_args) ->
    process_flag(trap_exit, true),
    {ok, #state{request_count = 1}}.

%%%%%
%% @spec handle_call(Call::term(), From::{pid(), reference()}, state()) ->
%%  {atom(reply), Reply, state()}
%
handle_call({intersects, Latitude, Longitude}, _, State) ->
    case rtree:intersects(Latitude, Longitude) of
        {ok, Bool} -> {reply, {ok, Bool}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end.


%%%%%
%% @spec handle_cast(Cast::term(), state()) ->
%%  {atom(stop), Reason::string(), state()}
%
handle_cast(stop, State) ->
    io:format("Cast: ~p~n", [State]),
    {stop, normal, State};
handle_cast(Cast, State) ->
    io:format("Cast: ~p~n", [State]),
    {stop, {"Can't not handle cast", Cast}, State}.

%%%%%
%% @spec handle_info(term(), state()) ->
%%  {atom(stop), Reason::string(), state()}
%
handle_info(Info, State) ->
    io:format("Info: ~p~n", [State]),
    {stop, {"Can't not handle info", Info}, State}.

%%%%%
%% @spec code_change(OldVsn::term(), state(), Extra::[term()]) ->
%%  {atom(ok), state()}
%
code_change(_, State, _) -> 
    io:format("Code Change: ~p~n", [State]),
    {ok, State}.

%%%%%
%% @spec terminate(Reason::term(), State::state()) -> none()
%
terminate(shutdown, State) -> 
    io:format("Terminate: ~p~n", [State]);
terminate(_, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%

% rtree module
