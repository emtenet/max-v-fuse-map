-module(iterate).

-export([iobs/5]).
-export([iobs/6]).

-export([labs/5]).
-export([labs/6]).

-export([pins_start/1]).
-export([pins_select/2]).
-export([pins_select_except/3]).

-type compile() :: experiment:compile().
-type device() :: device:device().
-type iob() :: iob:iob().
-type lab() :: lab:lab().
-type pin() :: pin:pin().
-type pins() :: [pin()].
-type pins_cycle() :: {pins(), pins()}.
-type pin_count() :: pos_integer().
-type pin_select() :: pin_count() | {pin_count(), except, pins()}.
-type result() :: experiment:result().

%%====================================================================
%% iobs
%%====================================================================

-spec iobs(device(), pins(), PinCount, Sources, Experiments) -> ok
    when
        PinCount :: pin_count() | fun((iob(), lab()) -> pin_select()),
        Sources :: fun((iob(), lab(), tuple()) -> [compile()]),
        Experiments :: fun((iob(), lab(), tuple(), [result()]) -> any()).

iobs(Device, Pins0, PinCount, Sources, Experiments) ->
    iobs(Device, Pins0, PinCount, Sources, Experiments, {batch, 1}).

%%--------------------------------------------------------------------

-spec iobs(device(), pins(), PinCount, Sources, Experiments, Batch) -> ok
    when
        PinCount :: pin_count() | fun((iob(), lab()) -> pin_select()),
        Sources :: fun((iob(), lab(), tuple()) -> [compile()]),
        Experiments :: fun((iob(), lab(), tuple(), [result()]) -> any()),
        Batch :: {batch, pos_integer()}.

iobs(Device, Pins0, PinCount, Sources, Experiments, {batch, Batch})
        when is_integer(Batch) ->
    IOBs = device:iobs(Device),
    Pins = pins_start(Pins0),
    iobs_loop(Batch, IOBs, Pins, PinCount, Sources, Experiments).

%%--------------------------------------------------------------------

iobs_loop(_, [], _, _, _, _) ->
    ok;
iobs_loop(Batch, IOBs0, Pins0, PinCount, Sources, Experiments) ->
    {IOBs, Pins, Sets} =
        iobs_sources(Batch, IOBs0, Pins0, PinCount, Sources, []),
    Ss = lists:flatten([S || {_, _, _, S} <- Sets]),
    {ok, Es} = experiment:compile_to_fuses_and_rcf(Ss),
    iobs_experiments(Sets, Es, Experiments),
    iobs_loop(Batch, IOBs, Pins, PinCount, Sources, Experiments).

%%--------------------------------------------------------------------

iobs_sources(0, IOBs, Pins, _, _, Sets) ->
    {IOBs, Pins, lists:reverse(Sets)};
iobs_sources(_, IOBs = [], Pins, _, _, Sets) ->
    {IOBs, Pins, lists:reverse(Sets)};
iobs_sources(N, [{IOB, LAB} | IOBs], Pins0, PinCount, Sources, Sets) ->
    {Ps, Pins} = iobs_pins(PinCount, IOB, LAB, Pins0),
    Ss = Sources(IOB, LAB, Ps),
    Set = {IOB, LAB, Ps, Ss},
    iobs_sources(N - 1, IOBs, Pins, PinCount, Sources, [Set | Sets]).

%%--------------------------------------------------------------------

iobs_pins(Count, _, _, Pins) when is_integer(Count) ->
    pins_select(Count, Pins);
iobs_pins(Fun, IOB, LAB, Pins) when is_function(Fun, 2) ->
    case Fun(IOB, LAB) of
        Count when is_integer(Count) ->
            pins_select(Count, Pins);

        {Count, except, Except} ->
            pins_select_except(Count, Pins, Except)
    end.

%%--------------------------------------------------------------------

iobs_experiments([], [], _) ->
    ok;
iobs_experiments([{IOB, LAB, Ps, Ss} | Sets], Es0, Experiments) ->
    {Es, Es1} = lists:split(length(Ss), Es0),
    Experiments(IOB, LAB, Ps, Es),
    iobs_experiments(Sets, Es1, Experiments).

%%====================================================================
%% labs
%%====================================================================

-spec labs(device(), pins(), pin_count(), Sources, Experiments) -> ok
    when
        Sources :: fun((lab(), tuple()) -> [compile()]),
        Experiments :: fun((lab(), tuple(), [result()]) -> any()).

labs(Device, Pins0, PinCount, Sources, Experiments)
        when is_integer(PinCount) ->
    labs(Device, Pins0, PinCount, Sources, Experiments, {batch, 1}).

%%--------------------------------------------------------------------

-spec labs(device(), pins(), pin_count(), Sources, Experiments, Batch) -> ok
    when
        Sources :: fun((lab(), tuple()) -> [compile()]),
        Experiments :: fun((lab(), tuple(), [result()]) -> any()),
        Batch :: {batch, pos_integer()}.

labs(Device, Pins0, PinCount, Sources, Experiments, {batch, Batch})
        when is_integer(PinCount) andalso
             is_integer(Batch) ->
    LABs = device:labs(Device),
    Pins = pins_start(Pins0),
    labs_loop(Batch, LABs, Pins, PinCount, Sources, Experiments).

%%--------------------------------------------------------------------

labs_loop(_, [], _, _, _, _) ->
    ok;
labs_loop(Batch, LABs0, Pins0, PinCount, Sources, Experiments) ->
    {LABs, Pins, Sets} =
        labs_sources(Batch, LABs0, Pins0, PinCount, Sources, []),
    Ss = lists:flatten([S || {_, _, S} <- Sets]),
    {ok, Es} = experiment:compile_to_fuses_and_rcf(Ss),
    labs_experiments(Sets, Es, Experiments),
    labs_loop(Batch, LABs, Pins, PinCount, Sources, Experiments).

%%--------------------------------------------------------------------

labs_sources(0, LABs, Pins, _, _, Sets) ->
    {LABs, Pins, lists:reverse(Sets)};
labs_sources(_, LABs = [], Pins, _, _, Sets) ->
    {LABs, Pins, lists:reverse(Sets)};
labs_sources(N, [LAB | LABs], Pins0, PinCount, Sources, Sets) ->
    {Ps, Pins} = pins_select(PinCount, Pins0),
    Ss = Sources(LAB, Ps),
    Set = {LAB, Ps, Ss},
    labs_sources(N - 1, LABs, Pins, PinCount, Sources, [Set | Sets]).

%%--------------------------------------------------------------------

labs_experiments([], [], _) ->
    ok;
labs_experiments([{LAB, Ps, Ss} | Sets], Es0, Experiments) ->
    {Es, Es1} = lists:split(length(Ss), Es0),
    Experiments(LAB, Ps, Es),
    labs_experiments(Sets, Es1, Experiments).

%%====================================================================
%% pins_start
%%====================================================================

-spec pins_start(pins()) -> pins_cycle().

pins_start(Pins) when is_list(Pins) ->
    {Pins, Pins}.

%%====================================================================
%% pins_select
%%====================================================================

-spec pins_select(pin_count(), pins_cycle()) -> {tuple(), pins_cycle()}.

pins_select(0, Cycle) ->
    {{}, Cycle};
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
    {{A, B, C, D, E, F}, {Head, Tail}};
pins_select(N, Pins) when N > 6 ->
    pins_select(N, [], Pins).

%%--------------------------------------------------------------------

pins_select(0, Ps, Pins) ->
    {list_to_tuple(lists:reverse(Ps)), Pins};
pins_select(N, Ps, {[], Tail = [P | Head]}) ->
    pins_select(N - 1, [P | Ps], {Head, Tail});
pins_select(N, Ps, {[P | Head], Tail}) ->
    pins_select(N - 1, [P | Ps], {Head, Tail}).

%%====================================================================
%% pins_select_except
%%====================================================================

-spec pins_select_except(pin_count(), pins_cycle(), pins())
    -> {tuple(), pins_cycle()}.

pins_select_except(N, Pins, Except) ->
    pins_select_except(N, [], Pins, Except).

%%--------------------------------------------------------------------

pins_select_except(0, Ps, Pins, _) ->
    {list_to_tuple(lists:reverse(Ps)), Pins};
pins_select_except(N, Ps, {[], Pins}, Except) ->
    pins_select_except(N, Ps, {Pins, Pins}, Except);
pins_select_except(N, Ps, {[P | Head], Tail}, Except) ->
    case lists:member(P, Except) of
        true ->
            pins_select_except(N, Ps, {Head, Tail}, Except);

        false ->
            pins_select_except(N - 1, [P | Ps], {Head, Tail}, Except)
    end.

