-module(fuse_bitmap).

-export([print_database/1]).
-export([print_map/1]).
-export([print_minimal/1]).

-type density() :: density:density().

%%====================================================================
%% print
%%====================================================================

-spec print_database(density()) -> ok.

print_database(Density) ->
    FuseCount = density:fuse_count(Density),
    Database = fuse_database:read(Density),
    lines(0, FuseCount, Database).

%%--------------------------------------------------------------------

-spec print_map(density()) -> ok.

print_map(Density) ->
    FuseCount = density:fuse_count(Density),
    lines(0, FuseCount, Density).

%%--------------------------------------------------------------------

-spec print_minimal(density()) -> ok.

print_minimal(Density) ->
    FuseCount = density:fuse_count(Density),
    Fuses = density:minimal_fuses(Density),
    lines(0, FuseCount, {Density, Fuses}).

%%--------------------------------------------------------------------

lines(Start, Stop, _) when Start >= Stop ->
    ok;
lines(Start, Stop, Database) ->
    Line = line(Start, Start + 64, Database, <<>>),
    io:format("~6b:~s~n", [Start, Line]),
    lines(Start + 64, Stop, Database).

%%--------------------------------------------------------------------

line(Stop, Stop, _, Line) ->
    Line;
line(Fuse, Stop, Database, Line) when Fuse rem 8 =:= 0 ->
    C = fuse(Fuse, Database),
    line(Fuse + 1, Stop, Database, <<Line/binary, " ", C>>);
line(Fuse, Stop, Database, Line) ->
    C = fuse(Fuse, Database),
    line(Fuse + 1, Stop, Database, <<Line/binary, C>>).

%%--------------------------------------------------------------------

fuse(Fuse, {Density, Fuses}) when is_list(Fuses) ->
    case pof_file:is_stripe(Fuse, Density) of
        true ->
            $|;

        false ->
            case lists:member(Fuse, Fuses) of
                true ->
                    %case fuse_map:to_name(Fuse, Density) of
                    %    {ok, _} -> $M;
                    %    {error, _} -> $?
                    %end;
                    $M;

                false -> $.
            end
    end;
fuse(Fuse, Density) when is_atom(Density) ->
    case pof_file:is_stripe(Fuse, Density) of
        true ->
            $|;

        false ->
            case fuse_map:to_name(Fuse, Density) of
                {ok, Name} ->
                    fuse(Name);

                {error, _} ->
                    $.
            end
    end;
fuse(Fuse, Database) ->
    case fuse_database:name(Fuse, Database) of
        Name when is_integer(Name) ->
            $.;

        Name ->
            fuse(Name)
    end.

%%--------------------------------------------------------------------

fuse({_IOC, bus_hold}) -> $B;
fuse({_IOC, schmitt_trigger}) -> $S;
fuse({_IOC, enable}) -> $E;
fuse({_IOC, enable_invert}) -> $E;
fuse({_IOC, enable3, _}) -> $E;
fuse({_IOC, enable4, _}) -> $E;
fuse({_IOC, output}) -> $O;
fuse({_IOC, output_invert}) -> $O;
fuse({_IOC, output3, _}) -> $O;
fuse({_IOC, output4, _}) -> $O;
fuse({_IOC, weak_pull_up}) -> $W;
fuse({_LAB, clk1, global0}) -> $k;
fuse({_LAB, clk1, global1}) -> $k;
fuse({_LAB, clk1, global2}) -> $k;
fuse({_LAB, clk1, global3}) -> $k;
fuse({_LAB, clk1, invert}) -> $k;
fuse({_LAB, clk2, global0}) -> $k;
fuse({_LAB, clk2, global1}) -> $k;
fuse({_LAB, clk2, global2}) -> $k;
fuse({_LAB, clk2, global3}) -> $k;
fuse({_LAB, clk2, invert}) -> $k;
fuse({_LAB, clr1, global0}) -> $r;
fuse({_LAB, clr1, global1}) -> $r;
fuse({_LAB, clr1, global2}) -> $r;
fuse({_LAB, clr1, global3}) -> $r;
fuse({_LAB, clr1, invert}) -> $r;
fuse({_LAB, {interconnect, _}, _}) -> $L;
fuse({_LAB, {interconnect, _}, _, _}) -> $L;
fuse({_LC, clk}) -> $k;
fuse({_LC, clr}) -> $r;
fuse({_LC, local_line}) -> $l;
fuse({_LC, lut, _}) -> $#;
fuse({_LC, lut_out, left}) -> $<;
fuse({_LC, lut_out, right}) -> $>;
fuse({{c4, _, _}, {mux, _}, _}) -> $C;
fuse({{c4, _, _}, {mux, _}, _, _}) -> $C;
fuse({{r4, _, _}, {mux, _}, _}) -> $R;
fuse({{r4, _, _}, {mux, _}, _, _}) -> $R;
fuse({user_code, _}) -> $U;
fuse(_) -> $~.


