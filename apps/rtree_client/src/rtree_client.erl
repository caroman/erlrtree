% Copyright 2013 Carlos Roman
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%%%----------------------------------------------------------------
%%% @author Carlos Roman <caroman@gmail.com>
%%% @doc
%%%   Escript with rtree client
%%% @copyright 2013 Carlos Roman
%%% @end
%%%----------------------------------------------------------------
-module(rtree_client).
-export([main/1]).
-mode(compile).

-define(ESCRIPT, filename:basename(escript:script_name())).

%% ====================================================================
%% Public API
%% ====================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Entry point
%%
%% @spec main(Args) -> atom(ok)
%% @end
%%------------------------------------------------------------------------------
main(Args) ->
    os:putenv("ESCRIPT", "1"),
    case catch(run(Args)) of
        ok ->
            ok;
        Error ->
            %% Dump this error to console
            io:format("Uncaught error processing args: ~p\n", [Error]),
            halt(1)
    end.

%% ====================================================================
%% Parser
%% ====================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Main usage function
%%
%% @spec usage() -> ok
%% @end
%%------------------------------------------------------------------------------
usage() ->
    OptSpecList = main_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client",
                 "command_args -- [options]").

%%------------------------------------------------------------------------------
%% @doc
%% Command create parser specific usage
%%
%% @spec command_create_usage() -> ok
%% @end
%%------------------------------------------------------------------------------
command_create_usage() ->
    OptSpecList = command_create_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client create --").

%%------------------------------------------------------------------------------
%% @doc
%% Command create load specific usage
%%
%% @spec command_load_usage() -> ok
%% @end
%%------------------------------------------------------------------------------
command_load_usage() ->
    OptSpecList = command_load_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client load --").

%%------------------------------------------------------------------------------
%% @doc
%% Command create build specific usage
%%
%% @spec command_build_usage() -> ok
%% @end
%%------------------------------------------------------------------------------
command_build_usage() ->
    OptSpecList = command_build_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client build --").

%%------------------------------------------------------------------------------
%% @doc
%% Command create intersects specific usage
%%
%% @spec command_intersects_usage() -> ok
%% @end
%%------------------------------------------------------------------------------
command_intersects_usage() ->
    OptSpecList = command_intersects_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client intersects --").

%%------------------------------------------------------------------------------
%% @doc
%% Main option specification list
%%
%% @spec main_option_spec_list() -> ok
%% @end
%%------------------------------------------------------------------------------
main_option_spec_list() ->
    %Jobs = ?DEFAULT_JOBS,
    %JobsHelp = io_lib:format(
    %    "Number of concurrent workers a command may use. Default: ~B",
    %    [Jobs]),
    %VerboseHelp = "Verbosity level (-v, -vv, -vvv, --verbose 3). Default: 0",
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,         $h,         "help",         undefined,
        "Show the program options"},
     %%{verbose,      $v,         "verbose",      integer,
     %%   VerboseHelp},
     %%{version,      $V,         "version",      undefined,
     %%   "Show version information"},
     %%{force,        $f,         "force",        undefined,
     %%   "Force"},
     {node_name,    $n,         "node_name",    {atom, node_name()},
        "Set the client node's <name|sname>. Default rtree_client."},
     {remote_node,  $r,         "remote_node",  {atom, list_to_atom("rtree@127.0.0.1")},
        "Node <name|sname> to connect to. Default rtree@127.0.0.1."},
     {cookie,       $c,         "cookie",       {atom, rtree},
        "Set cookie. Default rtree."},
     {timeout,      $t,         "timeout",      {integer, 10},
        "Timeout for response. Default 10 seconds. If is 0 then none is set."},
     {command,     undefined,   undefined,    atom,
        "Execute command. Options create, load, build, intersects. "}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% Command create specific option specification list
%%
%% @spec command_create_option_spec_list() -> ok
%% @end
%%------------------------------------------------------------------------------
command_create_option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,         $h,         "help",         undefined,
        "Show the program options"},
     {tree_name,    undefined,  undefined,      atom,
        "Tree name for rtree server (gen_server and ets)."}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% Command load specific option specification list
%%
%% @spec command_load_option_spec_list() -> ok
%% @end
%%------------------------------------------------------------------------------
command_load_option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,         $h,         "help",         undefined,
        "Show the program options"},
     {tree_name,    undefined,  undefined,      atom,
        "Tree name for rtree server (gen_server and ets)."},
     {dsn,    undefined,  undefined,      string,
        "Data source name."}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% Command build specific option specification list
%%
%% @spec command_build_option_spec_list() -> ok
%% @end
%%------------------------------------------------------------------------------
command_build_option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,         $h,         "help",         undefined,
        "Show the program options"},
     {tree_name,    undefined,  undefined,      atom,
        "Tree name for rtree server (gen_server and ets)."}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% Command intersects specific option specification list
%%
%% @spec command_intersects_option_spec_list() -> ok
%% @end
%%------------------------------------------------------------------------------
command_intersects_option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,         $h,         "help",         undefined,
        "Show the program options"},
     {tree_name,    undefined,  undefined,      atom,
        "Tree name for rtree server (gen_server and ets)."}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% Parse command line arguments
%%
%% @spec parse_args(RawArgs) -> {Options, Args}
%% @end
%%------------------------------------------------------------------------------
parse_args(RawArgs) ->
    %% Parse getopt options
    OptSpecList = main_option_spec_list(),
    case getopt:parse_and_check(OptSpecList, RawArgs) of
        {ok, {Options, Args}} ->
            case lists:any(
                fun(Elem) -> case Elem of help -> true; _ -> false end end,
                Options) of
                true ->
                    usage(),
                    halt(1);
                false -> false
            end,
            %% SubArgs contains Args, if appending is done then values
            %% will be replicated
            case  proplists:get_value(command, Options) of
                create ->
                    {SubOptions, SubArgs} = command_parse_args(Args,
                        fun command_create_option_spec_list/0,
                        fun command_create_usage/0),
                    MergedOptions = lists:append(Options, SubOptions),
                    {MergedOptions, SubArgs};
                load ->
                    {SubOptions, SubArgs} = command_parse_args(Args,
                        fun command_load_option_spec_list/0,
                        fun command_load_usage/0),
                    MergedOptions = lists:append(Options, SubOptions),
                    {MergedOptions, SubArgs};

                build ->
                    {SubOptions, SubArgs} = command_parse_args(Args,
                        fun command_build_option_spec_list/0,
                        fun command_build_usage/0),
                    MergedOptions = lists:append(Options, SubOptions),
                    {MergedOptions, SubArgs};
                intersects ->
                    {SubOptions, SubArgs} = command_parse_args(Args,
                        fun command_intersects_option_spec_list/0,
                        fun command_intersects_usage/0),
                    MergedOptions = lists:append(Options, SubOptions),
                    {MergedOptions, SubArgs};
                undefined ->
                    io:format("ERROR: missing argument <command>.~n"),
                    usage(),
                    halt(1);
                Other ->
                    io:format("ERROR: wrong argument <command>: ~p~n", [Other]),
                    usage(),
                    halt(1)
            end;
        {error, {Reason, Data}} ->
            io:format("ERROR: ~s ~p~n~n", [Reason, Data]),
            usage(),
            halt(1)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Parse command line arguments according to input functions
%%
%% @spec command_parse_args(ParserArgs, OptionSpecListFun, UsageFun) ->
%%  {Options, Args}
%% @end
%%------------------------------------------------------------------------------
command_parse_args(ParserArgs, OptionSpecListFun, UsageFun) ->
    OptSpecList = OptionSpecListFun(),
    case getopt:parse_and_check(OptSpecList, ParserArgs) of
        {ok, {Options, Args}} ->
            case lists:any(
                fun(Elem) -> case Elem of help -> true; _ -> false end end,
                Options) of
                true ->
                    UsageFun(),
                    halt(1);
                false -> {Options, Args}
            end;
        {error, {Reason, Data}} ->
            io:format("ERROR: ~s ~p~n~n", [Reason, Data]),
            UsageFun(),
            halt(1)
    end.

%% ====================================================================
%% Execution
%% ====================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Read raw arguments parse them and execute command
%%
%% @spec run(RawArgs) -> ok
%% @end
%%------------------------------------------------------------------------------
run(RawArgs) ->
    {Options, Args} = parse_args(RawArgs),
    run_command(proplists:get_value(command, Options), Options, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Run specific command 
%%
%% @spec run_command(create, Options, Args) -> ok
%% @end
%%------------------------------------------------------------------------------
run_command(create, Options, _Args) ->
    io:format("Run create: ~p~n", [Options]),
    RemoteNode = connect(Options),
    io:format("Remote node: ~p~n", [RemoteNode]),
    TreeName = proplists:get_value(tree_name, Options),
    Res = rpc:call(RemoteNode, rtree_server, create, [TreeName]),
    io:format("~p~n",[Res]);
%%------------------------------------------------------------------------------
%% @doc
%% Run specific command 
%%
%% @spec run_command(load, Options, Args) -> ok
%% @end
%%------------------------------------------------------------------------------
run_command(load, Options, Args) ->
    io:format("Run load: ~p~p~n", [Options, Args]),
    RemoteNode = connect(Options),
    io:format("Remote node: ~p~n", [RemoteNode]),
    TreeName = proplists:get_value(tree_name, Options),
    Dsn = proplists:get_value(dsn, Options),
    rtree:create_ets(TreeName),
    rtree:load_to_ets(Dsn, TreeName),
    lists:foreach(
        fun(R) -> io:format("~p~n", [R]) end,
            ets:match_object(TreeName, '$1')),
    {ok, Records} = rtree:load_to_list(Dsn),
    {ok, Tree} = rtree:tree_from_records(Records),
    %Res = rpc:call(RemoteNode, rtree_server, load, [TreeName]),
    %io:format("~p~n",[Res]);
    %io:format("Ets loaded ~p~n",[Table]);
    Res = rtree:intersects(Tree,  1.0, -1.0),
    io:format("Res ~p~n",[Res]);
%%------------------------------------------------------------------------------
%% @doc
%% Run specific command 
%%
%% @spec run_command(build, Options, Args) -> ok
%% @end
%%------------------------------------------------------------------------------
run_command(build, Options, Args) ->
    io:format("Run build: ~p~p~n", [Options, Args]);
%%------------------------------------------------------------------------------
%% @doc
%% Run specific command 
%%
%% @spec run_command(intersects, Options, Args) -> ok
%% @end
%%------------------------------------------------------------------------------
run_command(intersects, Options, Args) ->
    io:format("Run intersects: ~p~p~n", [Options, Args]).

%% ====================================================================
%% Helper Functions
%% ====================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Get the node name from the escript name plus the hostname
%%
%% @spec node_name() -> NodeName::atom
%% @end
%%------------------------------------------------------------------------------
node_name() ->
    Localhost = net_adm:localhost(),
    list_to_atom(?ESCRIPT ++ "@" ++ Localhost).

%%------------------------------------------------------------------------------
%% @doc
%% Setup and test connection to erlang cluster
%%
%% @spec connect(Options) -> RemoteNode::atom
%% @end
%%------------------------------------------------------------------------------
connect(Options) ->
    io:format("Connectting with options: ~p~n", [Options]),
    NodeName = proplists:get_value(node_name, Options),
    Cookie = proplists:get_value(cookie, Options),
    RemoteNode = proplists:get_value(remote_node, Options),
    net_kernel:start([NodeName, longnames]),
    erlang:set_cookie(NodeName, Cookie),
    case net_adm:ping(RemoteNode) of
        pong ->
            io:format("~s: ~s ~p~n", [?ESCRIPT, NodeName, pong]),
            RemoteNode;
        Else ->
            io:format("~s: ~s ~p~n", [?ESCRIPT, NodeName, Else]),
            halt(1)
    end.


