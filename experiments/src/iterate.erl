-module(iterate).

-export([labs/5]).

-export([pins_start/1]).
-export([pins_select/2]).

%%====================================================================
%% labs
%%====================================================================

labs(Device, PinCount, Pins0, Sources, Experiments) ->
    LABs = device:labs(Device),
    Pins = pins_start(Pins0),
    labs_loop(LABs, PinCount, Pins, Sources, Experiments).

%%--------------------------------------------------------------------

labs_loop([], _, _, _, _) ->
    ok;
labs_loop(LABs0, PinCount, Pins0, Sources, Experiments) ->
    {LABs, Pins, Sets} = labs_sources(4, LABs0, PinCount, Pins0, Sources, []),
    Ss = lists:flatten([S || {_, _, S} <- Sets]),
    {ok, Es} = experiment:compile_to_fuses_and_rcf(Ss),
    labs_experiments(Sets, Es, Experiments),
    labs_loop(LABs, PinCount, Pins, Sources, Experiments).

%%--------------------------------------------------------------------

labs_sources(0, LABs, _, Pins, _, Sets) ->
    {LABs, Pins, lists:reverse(Sets)};
labs_sources(_, LABs = [],  _, Pins, _, Sets) ->
    {LABs, Pins, lists:reverse(Sets)};
labs_sources(N, [LAB | LABs], PinCount, Pins0, Sources, Sets) ->
    {Ps, Pins} = pins_select(PinCount, Pins0),
    Ss = Sources(LAB, Ps),
    Set = {LAB, Ps, Ss},
    labs_sources(N - 1, LABs, PinCount, Pins, Sources, [Set | Sets]).

%%--------------------------------------------------------------------

labs_experiments([], [], _) ->
    ok;
labs_experiments([{LAB, Ps, Ss} | Sets], Es0, Experiments) ->
    {Es, Es1} = lists:split(length(Ss), Es0),
    Experiments(LAB, Ps, Es),
    labs_experiments(Sets, Es1, Experiments).

%%====================================================================
%% pins
%%====================================================================

pins_start(Pins = {_, _}) ->
    Pins;
pins_start(Pins) when is_list(Pins) ->
    {Pins, Pins}.

%%--------------------------------------------------------------------

pins_select(1, {[], Tail = [A | Head]}) ->
    {{A}, {Head, Tail}};
pins_select(1, {[A | Head], Tail}) ->
    {{A}, {Head, Tail}};
pins_select(2, {[], Tail = [A, B | Head]}) ->
    {{A, B}, {Head, Tail}};
pins_select(2, {[A], Tail = [B | Head]}) ->
    {{A, B}, {Head, Tail}};
pins_select(2, {[A, B | Head], Tail}) ->
    {{A, B}, {Head, Tail}};
pins_select(3, {[], Tail = [A, B, C | Head]}) ->
    {{A, B, C}, {Head, Tail}};
pins_select(3, {[A], Tail = [B, C | Head]}) ->
    {{A, B, C}, {Head, Tail}};
pins_select(3, {[A, B], Tail = [C | Head]}) ->
    {{A, B, C}, {Head, Tail}};
pins_select(3, {[A, B, C | Head], Tail}) ->
    {{A, B, C}, {Head, Tail}};
pins_select(4, {[], Tail = [A, B, C, D | Head]}) ->
    {{A, B, C, D}, {Head, Tail}};
pins_select(4, {[A], Tail = [B, C, D | Head]}) ->
    {{A, B, C, D}, {Head, Tail}};
pins_select(4, {[A, B], Tail = [C, D | Head]}) ->
    {{A, B, C, D}, {Head, Tail}};
pins_select(4, {[A, B, C], Tail = [D | Head]}) ->
    {{A, B, C, D}, {Head, Tail}};
pins_select(4, {[A, B, C, D | Head], Tail}) ->
    {{A, B, C, D}, {Head, Tail}};
pins_select(5, {[], Tail = [A, B, C, D, E | Head]}) ->
    {{A, B, C, D, E}, {Head, Tail}};
pins_select(5, {[A], Tail = [B, C, D, E | Head]}) ->
    {{A, B, C, D, E}, {Head, Tail}};
pins_select(5, {[A, B], Tail = [C, D, E | Head]}) ->
    {{A, B, C, D, E}, {Head, Tail}};
pins_select(5, {[A, B, C], Tail = [D, E | Head]}) ->
    {{A, B, C, D, E}, {Head, Tail}};
pins_select(5, {[A, B, C, D], Tail = [E | Head]}) ->
    {{A, B, C, D, E}, {Head, Tail}};
pins_select(5, {[A, B, C, D, E | Head], Tail}) ->
    {{A, B, C, D, E}, {Head, Tail}};
pins_select(6, {[], Tail = [A, B, C, D, E, F | Head]}) ->
    {{A, B, C, D, E, F}, {Head, Tail}};
pins_select(6, {[A], Tail = [B, C, D, E, F | Head]}) ->
    {{A, B, C, D, E, F}, {Head, Tail}};
pins_select(6, {[A, B], Tail = [C, D, E, F | Head]}) ->
    {{A, B, C, D, E, F}, {Head, Tail}};
pins_select(6, {[A, B, C], Tail = [D, E, F | Head]}) ->
    {{A, B, C, D, E, F}, {Head, Tail}};
pins_select(6, {[A, B, C, D], Tail = [E, F | Head]}) ->
    {{A, B, C, D, E, F}, {Head, Tail}};
pins_select(6, {[A, B, C, D, E], Tail = [F | Head]}) ->
    {{A, B, C, D, E, F}, {Head, Tail}};
pins_select(6, {[A, B, C, D, E, F | Head], Tail}) ->
    {{A, B, C, D, E, F}, {Head, Tail}}.

