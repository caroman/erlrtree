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
-compile([{parse_transform, lager_transform}]).


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
    lager:start(),
    lager:set_loglevel(lager_console_backend, debug),
    case catch(run(Args)) of
        ok ->
            ok;
        Error ->
            %% Dump this error to console
            lager:error("Uncaught error processing args: ~p", [Error]),
            delayed_halt(1)
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
%% Command load parser specific usage
%%
%% @spec command_load_usage() -> ok
%% @end
%%------------------------------------------------------------------------------
command_load_usage() ->
    OptSpecList = command_load_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client load --").

%%------------------------------------------------------------------------------
%% @doc
%% Command build parser specific usage
%%
%% @spec command_build_usage() -> ok
%% @end
%%------------------------------------------------------------------------------
command_build_usage() ->
    OptSpecList = command_build_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client build --").

%%------------------------------------------------------------------------------
%% @doc
%% Command intersects parser specific usage
%%
%% @spec command_intersects_usage() -> ok
%% @end
%%------------------------------------------------------------------------------
command_intersects_usage() ->
    OptSpecList = command_intersects_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client intersects --").

%%------------------------------------------------------------------------------
%% @doc
%% Command doall parser specific usage
%%
%% @spec command_doall_usage() -> ok
%% @end
%%------------------------------------------------------------------------------
command_doall_usage() ->
    OptSpecList = command_doall_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client doall --").


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
     {remote_node,  $r,         "remote_node",  {atom,
        list_to_atom("rtree_server@127.0.0.1")},
        "Node <name|sname> to connect to. Default rtree_server@127.0.0.1."},
     {cookie,       $c,         "cookie",       {atom, rtree_server},
        "Set cookie. Default rtree_server."},
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
        "Tree name for rtree server (gen_server and ets)."},
     {input_file,    undefined,  undefined,      string,
        "Input file with longitude,latitude values to intersect (.csv.gz)."},
     {output_file,    undefined,  undefined,      string,
        "Output file with longitude,latitude values to intersect (.csv.gz)."}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% Command doall specific option specification list
%%
%% @spec command_doall_option_spec_list() -> ok
%% @end
%%------------------------------------------------------------------------------
command_doall_option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,         $h,         "help",         undefined,
        "Show the program options"},
     {dsn,    undefined,  undefined,      string,
        "Data source name."},
     {input_file,    undefined,  undefined,      string,
        "Input file with longitude,latitude values to intersect (.csv.gz)."},
     {output_file,    undefined,  undefined,      string,
        "Output file with longitude,latitude values to intersect (.csv.gz)."}
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
                    delayed_halt(1);
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
                doall->
                    {SubOptions, SubArgs} = command_parse_args(Args,
                        fun command_doall_option_spec_list/0,
                        fun command_doall_usage/0),
                    MergedOptions = lists:append(Options, SubOptions),
                    {MergedOptions, SubArgs};
                undefined ->
                    lager:error("Missing argument <command>."),
                    usage(),
                    delayed_halt(1);
                Other ->
                    lager:warning("Wrong argument <command>: ~p~n", [Other]),
                    usage(),
                    delayed_halt(1)
            end;
        {error, {Reason, Data}} ->
            lager:error("~s ~p~n~n", [Reason, Data]),
            usage(),
            delayed_halt(1)
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
                    delayed_halt(1);
                false -> {Options, Args}
            end;
        {error, {Reason, Data}} ->
            lager:error("~s ~p~n~n", [Reason, Data]),
            UsageFun(),
            delayed_halt(1)
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
    lager:debug("Run create: ~p~n", [Options]),
    RemoteNode = connect(Options),
    lager:debug("Remote node: ~p~n", [RemoteNode]),
    TreeName = proplists:get_value(tree_name, Options),
    lager:debug("rtree_server:create(~p)~n", [TreeName]),
    Res = rpc:call(RemoteNode, rtree_server, create, [TreeName]),
    lager:info("Response: ~p~n",[Res]),
    delayed_halt(0);
%%------------------------------------------------------------------------------
%% @doc
%% Run specific command 
%%
%% @spec run_command(load, Options, Args) -> ok
%% @end
%%------------------------------------------------------------------------------
run_command(load, Options, Args) ->
    lager:debug("Run load: ~p~p~n", [Options, Args]),
    RemoteNode = connect(Options),
    lager:debug("Remote node: ~p~n", [RemoteNode]),
    TreeName = proplists:get_value(tree_name, Options),
    Dsn = proplists:get_value(dsn, Options),
    Res = rpc:call(RemoteNode, rtree_server, load, [TreeName, Dsn]),
    %{ok, Records} = rtree:load_to_list(Dsn),
    %{ok, Tree} = rtree:tree_from_records(Records),
    %lager:debug("~p~n",[Res]);
    lager:info("Response ~p~n",[Res]),
    delayed_halt(0);
%%------------------------------------------------------------------------------
%% @doc
%% Run specific command 
%%
%% @spec run_command(build, Options, Args) -> ok
%% @end
%%------------------------------------------------------------------------------
run_command(build, Options, Args) ->
    lager:debug("Run build: ~p~p~n", [Options, Args]),
    RemoteNode = connect(Options),
    lager:debug("Remote node: ~p~n", [RemoteNode]),
    TreeName = proplists:get_value(tree_name, Options),
    Res = rpc:call(RemoteNode, rtree_server, build, [TreeName]),
    %{ok, Records} = rtree:load_to_list(Dsn),
    %{ok, Tree} = rtree:tree_from_records(Records),
    %lager:debug("~p~n",[Res]);
    lager:info("Response ~p~n",[Res]),
    delayed_halt(0);
%%------------------------------------------------------------------------------
%% @doc
%% Run specific command 
%%
%% @spec run_command(intersects, Options, Args) -> ok
%% @end
%%------------------------------------------------------------------------------
run_command(intersects, Options, Args) ->
    lager:debug("Run intersects: Options:~p Args:~p~n", [Options, Args]),
    TreeName = proplists:get_value(tree_name, Options),
    InputPath = filename:absname(proplists:get_value(input_file, Options)),
    OutputPath = filename:absname(proplists:get_value(output_file, Options)),
    RemoteNode = connect(Options),
    Res = rpc:call(RemoteNode, rtree_server, pintersects_file,
        [TreeName, InputPath, OutputPath, self()]),
    lager:debug("Response: ~p", [Res]),
    io:format("Response: ~p~n", [Res]),
    receive
        {ok, InputFile} ->
            lager:info("Done processing input file: ~p~n", [InputFile]);
        Other ->
            lager:info("Done: ~p~n", [Other])
    end,
    delayed_halt(0);
 
%%------------------------------------------------------------------------------
%% @doc
%% Run specific command 
%%
%% @spec run_command(load, Options, Args) -> ok
%% @end
%%------------------------------------------------------------------------------
run_command(doall, Options, Args) ->
    lager:debug("Running doall with: ~p~p~n", [Options, Args]),
    Dsn = proplists:get_value(dsn, Options),
    ok = application:start(rtree_server),
    rtree_server:create(local_tree),
    rtree_server:load(local_tree, Dsn),
    rtree_server:build(local_tree),
    InputPath = filename:absname(proplists:get_value(input_file, Options)),
    OutputPath = filename:absname(proplists:get_value(output_file, Options)),
    Res = rtree_server:intersects_file(local_tree, InputPath, OutputPath),
    %lager:debug("Response: ~p", [Res]),
    io:format("Response: ~p~n", [Res]),
    delayed_halt(0).
 
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
    lager:debug("Connecting with options: ~p~n", [Options]),
    NodeName = proplists:get_value(node_name, Options),
    Cookie = proplists:get_value(cookie, Options),
    RemoteNode = proplists:get_value(remote_node, Options),
    net_kernel:start([NodeName, longnames]),
    erlang:set_cookie(NodeName, Cookie),
    case net_adm:ping(RemoteNode) of
        pong ->
            lager:debug("~s: ~s ~p~n", [?ESCRIPT, NodeName, pong]),
            %net_kernel:connect_node(RemoteNode),
            RemoteNode;
        Else ->
            lager:debug("~s: ~s ~p~n", [?ESCRIPT, NodeName, Else]),
            delayed_halt(1)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% From rebar_utils.erl
%% Work around buffer flushing issue in erlang:halt if OTP older than R15B01.
%% TODO: remove workaround once we require R15B01 or newer R15B01 introduced
%% erlang:halt/2
%%
%% @spec delayed_halt(integer()) -> no_return()
%% @end
%%------------------------------------------------------------------------------
delayed_halt(Code) ->
    case erlang:is_builtin(erlang, halt, 2) of
        true ->
            timer:sleep(100),
            halt(Code);
        false ->
            case os:type() of
                {win32, nt} ->
                    timer:sleep(100),
                    halt(Code);
                _ ->
                    halt(Code),
                    %% workaround to delay exit until all output is written
                    receive after infinity -> ok end
            end
    end.
