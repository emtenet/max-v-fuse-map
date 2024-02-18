-module(quartus_compile).

-export([in_dir/2]).

-export_type([source/0]).
-export_type([result/0]).

-include_lib("kernel/include/file.hrl").

-type source() :: #{
    title => term(),
    family := binary(),
    device := binary(),
    settings := binary(),
    verilog := binary()
}.

-type result() :: {ok, #{pof := binary(), rcf := binary()}} | {error, term()}.

%%====================================================================
%% compile
%%====================================================================

-spec in_dir(file:filename_all(), source()) -> result().

in_dir(InDir, Compile = #{family := Family, device := Device, settings := Settings, verilog := Verilog})
        when is_binary(Family) andalso
             is_binary(Device) andalso
             is_binary(Settings) andalso
             is_binary(Verilog) ->
    case Compile of
        #{title := Title} when is_atom(Title) orelse is_binary(Title) ->
            io:format("[~s] ~s ~s ~s~n", [InDir, Family, Device, Title]);

        #{title := Title} ->
            io:format("[~s] ~s ~s ~p~n", [InDir, Family, Device, Title]);

        _ ->
            io:format("[~s] ~s ~s~n", [InDir, Family, Device])
    end,
    Dir = filename:join("run", InDir),
    case file:make_dir(Dir) of
        ok ->
            clear(Dir, Family, Device, Settings, Verilog);

        {error, eexist} ->
            clear(Dir, Family, Device, Settings, Verilog);

        {error, Reason} ->
            {error, {make_dir, Reason}}
    end;
in_dir(_, _) ->
    {error, badarg}.

%%--------------------------------------------------------------------

clear(Dir, Family, Device, Settings, Verilog) ->
    case clear_dir(Dir) of
        ok ->
            write_qpf(Dir, Family, Device, Settings, Verilog);

        {error, Reason} ->
            {error, {clear_dir, Reason}}
    end.

%%--------------------------------------------------------------------

write_qpf(Dir, Family, Device, Settings, Verilog) ->
    File = filename:join(Dir, "experiment.qpf"),
    Data = <<
        "QUARTUS_VERSION = \"13.1\"\n"
        "DATE = \"11:38:48  January 28, 2023\"\n"
        "PROJECT_REVISION = \"experiment\"\n"
    >>,
    case file:write_file(File, Data) of
        ok ->
            write_qsf(Dir, Family, Device, Settings, Verilog);

        {error, Reason} ->
            {error, {qpf_file, Reason}}
    end.

%%--------------------------------------------------------------------

write_qsf(Dir, Family, Device, Settings, Verilog) ->
    File = filename:join(Dir, "experiment.qsf"),
    Data = <<
        "set_global_assignment -name FAMILY \"", Family/binary, "\"\n"
        "set_global_assignment -name DEVICE ", Device/binary, "\n"
        "set_global_assignment -name PROJECT_OUTPUT_DIRECTORY output_files\n"
        "set_global_assignment -name VERILOG_FILE experiment.v\n"
        "set_global_assignment -name TOP_LEVEL_ENTITY experiment\n",
        Settings/binary
    >>,
    case file:write_file(File, Data) of
        ok ->
            write_vhd(Dir, Family, Device, Verilog);

        {error, Reason} ->
            {error, {qpf_file, Reason}}
    end.

%%--------------------------------------------------------------------

write_vhd(Dir, Family, Device, Verilog) ->
    File = filename:join(Dir, "experiment.v"),
    case file:write_file(File, Verilog) of
        ok ->
            exec_map(Dir, Family, Device);

        {error, Reason} ->
            {error, {qpf_file, Reason}}
    end.

%%--------------------------------------------------------------------

exec_map(Dir, Family, Device) ->
    Bin = "quartus_map",
    Args = [
        "experiment",
        "--source=experiment.v",
        "--family=" ++ binary_to_list(Family)
    ],
    case exec(Dir, Bin, Args) of
        ok ->
            exec_fit(Dir, Device);

        {error, {exit, Exit, Out}} ->
            {error, {quartus_map, Exit, Out}}
    end.

%%--------------------------------------------------------------------

exec_fit(Dir, Device) ->
    Bin = "quartus_fit",
    Args = [
        "experiment",
        "--part=" ++ binary_to_list(Device)
    ],
    case exec(Dir, Bin, Args) of
        ok ->
            exec_asm(Dir);

        {error, {exit, Exit, Out}} ->
            {error, {quartus_fit, Exit, Out}}
    end.

%%--------------------------------------------------------------------

exec_asm(Dir) ->
    Bin ="quartus_asm",
    Args = [
        "experiment"
    ],
    case exec(Dir, Bin, Args) of
        ok ->
            exec_cdb(Dir);

        {error, {exit, Exit, Out}} ->
            {error, {quartus_asm, Exit, Out}}
    end.

%%--------------------------------------------------------------------

exec_cdb(Dir) ->
    Bin ="quartus_cdb",
    Args = [
        "--back_annotate=routing",
        "experiment"
    ],
    case exec(Dir, Bin, Args) of
        ok ->
            read_pof(Dir);

        {error, {exit, Exit, Out}} ->
            {error, {quartus_asm, Exit, Out}}
    end.

%%--------------------------------------------------------------------

read_pof(Dir) ->
    File = filename:join([Dir, "output_files", "experiment.pof"]),
    case file:read_file(File) of
        {ok, POF} ->
            read_rcf(Dir, POF);

        {error, Reason} ->
            {error, {pof_file, Reason}}
    end.

%%--------------------------------------------------------------------

read_rcf(Dir, POF) ->
    File = filename:join([Dir, "experiment.rcf"]),
    case file:read_file(File) of
        {ok, RCF} ->
            {ok, #{pof => POF, rcf => RCF}};

        {error, Reason} ->
            {error, {rcf_file, Reason}}
    end.

%%====================================================================
%% helpers
%%====================================================================

clear_dir(Dir) ->
    case file:list_dir_all(Dir) of
        {ok, Names} ->
            clear_dir(Dir, Names);

        Error ->
            Error
    end.

%%--------------------------------------------------------------------

clear_dir(_, []) ->
    ok;
clear_dir(Dir, [Name | Names]) ->
    case clear_file(filename:join(Dir, Name)) of
        ok ->
            clear_dir(Dir, Names);

        Error ->
            Error
    end.

%%--------------------------------------------------------------------

clear_file(File) ->
    case file:read_link_info(File) of
        {ok, #file_info{type = directory}} ->
            case clear_dir(File) of
                ok ->
                    file:del_dir(File);

                Error ->
                    Error
            end;

        {ok, _} ->
            file:delete(File);

        Error ->
            Error
    end.

%%--------------------------------------------------------------------

exec(Dir, Arg0, Args) ->
    Path = "C:\\Applications\\Altera\\13.1\\quartus\\bin64",
    Exec = {spawn_executable, filename:join(Path, Arg0)},
    Opts = [
        {args, Args},
        {cd, Dir},
        stream,
        exit_status,
        use_stdio,
        binary,
        eof
    ],
    Port = erlang:open_port(Exec, Opts),
    Result = exec(Port, []),
    erlang:port_close(Port),
    case Result of
        {0, _} ->
            ok;

        {Exit, Out} ->
            %io:format("[[[[~n~s~n]]]]~nEXIT: ~p~n", [Out, Exit]),
            {error, {exit, Exit, Out}}
    end.

%%--------------------------------------------------------------------

exec(Port, Out) ->
    receive
        {Port, eof} ->
            receive
                {Port, {exit_status, Exit}} ->
                    {Exit, lists:reverse(Out)}
            end;

        {Port, {exit_status, Exit}} ->
            receive
                {Port, eof} ->
                    {Exit, lists:reverse(Out)}
            end;

        {Port, {data, Data}} ->
            exec(Port, [Data | Out])
    end.
