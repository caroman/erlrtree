-module(rtree_client).
-export([main/1]).
-mode(compile).

-define(DEFAULT_JOBS, 3).

%% ====================================================================
%% Public API
%% ====================================================================

%% escript Entry point
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



usage() ->
    OptSpecList = main_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client",
                 "-- [options] args",
                 [{"options", "connection options"},
                  {"command", "Command to run (create, load, build, intersects)"}]).

command_create_usage() ->
    OptSpecList = command_create_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client create --").

command_load_usage() ->
    OptSpecList = command_create_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client load --").

command_build_usage() ->
    OptSpecList = command_create_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client build --").

command_intersects_usage() ->
    OptSpecList = command_create_option_spec_list(),
    getopt:usage(OptSpecList, "rtree_client intersects --").

%%
%% options accepted via getopt
%%
main_option_spec_list() ->
    Jobs = ?DEFAULT_JOBS,
    JobsHelp = io_lib:format(
        "Number of concurrent workers a command may use. Default: ~B",
        [Jobs]),
    VerboseHelp = "Verbosity level (-v, -vv, -vvv, --verbose 3). Default: 0",
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
     {node_name,    $n,         "node_name",    {string, "rtree_client"},
        "Set the client node's <name|sname>. Default rtree_client."},
     {remote_node,  $r,         "remote_node",  {string, "rtree@127.0.0.1"},
        "Node <name|sname> to connect to. Default rtree@127.0.0.1."},
     {cookie,       $c,         "cookie",       {string, "rtree"},
        "Set cookie. Default rtree."},
     {timeout,      $t,         "timeout",      {integer, 10},
        "Timeout for response. Default 10 seconds. If is 0 then none is set."},
     {command,     undefined,   undefined,    atom,
        "Execute command. Options create, load, build, intersects. "}
    ].

command_create_option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,         $h,         "help",         undefined,
        "Show the program options"},
     {tree_name,    undefined,  undefined,      atom,
        "Tree name for rtree server (gen_server and ets)."}
    ].

command_load_option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,         $h,         "help",         undefined,
        "Show the program options"},
     {tree_name,    undefined,  undefined,      atom,
        "Tree name for rtree server (gen_server and ets)."}
    ].

command_build_option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,         $h,         "help",         undefined,
        "Show the program options"},
     {tree_name,    undefined,  undefined,      atom,
        "Tree name for rtree server (gen_server and ets)."}
    ].

command_intersects_option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,         $h,         "help",         undefined,
        "Show the program options"},
     {tree_name,    undefined,  undefined,      atom,
        "Tree name for rtree server (gen_server and ets)."}
    ].

%%
%% Parse command line arguments using getopt
%%
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
%% Internal functions
%% ====================================================================

%run([StringX, StringY]) ->
%    try
%        X = list_to_float(StringX),
%        Y = list_to_float(StringY),
%        Wkt = astext(X, Y),
%        io:format("Input:~f ~f = ~w\n", [X, Y, Wkt])
%    catch
%        _:_ ->
%            usage(),
%            halt(1)
%    end;
run(RawArgs) ->
    {Options, Args} = parse_args(RawArgs),
    run_command(proplists:get_value(command, Options), Options, Args).

run_command(create, Options, Args) ->
    io:format("Run create: ~p~p~n", [Options, Args]);
run_command(load, Options, Args) ->
    io:format("Run load: ~p~p~n", [Options, Args]);
run_command(build, Options, Args) ->
    io:format("Run build: ~p~p~n", [Options, Args]);
run_command(intersects, Options, Args) ->
    io:format("Run intersects: ~p~p~n", [Options, Args]).

%%astext(X, Y) ->
%%    Pt = {'Point',[X, Y]},
%%    Pt1 = erlgeom:to_geom(Pt),
%%    erlgeom:from_geom(Pt1).
