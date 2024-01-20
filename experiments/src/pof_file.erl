-module(pof_file).

-export([read/1]).
-export([decode/1]).
-export([fuse_count/1]).
-export([fuses/2]).
-export([has_fuse/2]).
-export([is_stripe/2]).

-export_type([pof/0]).
-export_type([pof_binary/0]).
-export_type([flash/0]).

-type density() :: density:density().
-type pof_binary() :: binary().

-type pof() :: #{
    cfm => flash(),
    ufm => flash(),
    any() => any()
}.

-type flash() :: #{
    data := binary(),
    size := non_neg_integer()
}.

%%====================================================================
%% read
%%====================================================================

-spec read(file:name_all()) -> {ok, pof()}.

read(File) ->
    {ok, Data} = file:read_file(File),
    decode(Data).

%%====================================================================
%% decode
%%====================================================================

-spec decode(pof_binary()) -> {ok, pof()}.

decode(<<"POF", 0, 0, 0, 1, 0, Count:32/little-unsigned, Data/binary>>) ->
    decode_parts(Count, Data, #{}).

%%--------------------------------------------------------------------

decode_parts(0, <<>>, Parts) ->
    {ok, Parts};
decode_parts(N, <<Id:16/little, Size:32/little, Data/binary>>, Parts) ->
    <<Part:Size/binary, Rest/binary>> = Data,
    decode_parts(N - 1, Rest, decode_part(Id, Part, Parts)).

%%--------------------------------------------------------------------

decode_part(1, Data, Parts) ->
    decode_string(compiler, Data, Parts);
decode_part(2, Data, Parts) ->
    decode_string(device, Data, Parts);
decode_part(3, Data, Parts) ->
    decode_string(name, Data, Parts);
decode_part(17, Data, Parts) ->
    decode_flash(cfm, Data, Parts);
decode_part(24, Data, Parts) ->
    decode_flash(ufm, Data, Parts);
decode_part(5, Data, Parts) ->
    case Data of
        <<0, 0>> ->
            Parts#{security => off};

        <<1, 0>> ->
            Parts#{security => on}
    end;
decode_part(8, _Data, Parts) ->
    % checksum ?
    Parts.

%%--------------------------------------------------------------------

decode_string(Name, Data, Parts) ->
    Size = byte_size(Data) - 1,
    <<String:Size/binary, 0>> = Data,
    Parts#{Name => String}.

%%--------------------------------------------------------------------

decode_flash(Name, <<0,0,0,0,0,0, Size:32/little, 1,0, Data/binary>>, Parts) ->
    Size = 8 * byte_size(Data),
    Parts#{Name => #{
        data => Data,
        size => Size
    }}.

%%====================================================================
%% fuse_count
%%====================================================================

-spec fuse_count(pof()) -> non_neg_integer().

fuse_count(#{cfm := #{size := Size}}) ->
    Size.

%%====================================================================
%% fuses
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

fuses_test() ->
    % See: doc/fuse-ordering.md
    % See: doc/fuse-vs-bit.md
    CFM = <<16#fe, 16#ff, 16#ff, 16#ff, 16#fd, 16#3f, 16#9f, 16#e7>>,
    POF = #{cfm => #{data => CFM}},
    Fuses = [33, 46, 47, 53, 54, 59, 60],
    ?assertEqual(Fuses, fuses(max_v_2210z, POF)),
    [
        ?assertEqual(lists:member(Fuse, Fuses), has_fuse(Fuse, POF))
        ||
        % start from 1, since bit 0 is a strip bit
        % that _always_ exists, yet not returned by fuses/2
        Fuse <- lists:seq(1, bit_size(CFM) - 1)
    ],
    ok.

-endif.

%%--------------------------------------------------------------------

-spec fuses(density(), pof()) -> [fuse:fuse()].

fuses(max_v_240z, #{cfm := #{data := Bytes}}) ->
    fuses_small_bytes(0, Bytes, []);
fuses(max_v_570z, #{cfm := #{data := Bytes}}) ->
    fuses_small_bytes(0, Bytes, []);
fuses(max_v_1270z, #{cfm := #{data := Bytes}}) ->
    fuses_large_bytes(0, Bytes, []);
fuses(max_v_2210z, #{cfm := #{data := Bytes}}) ->
    fuses_large_bytes(0, Bytes, []).

%%--------------------------------------------------------------------

fuses_small_bytes(_, <<>>, Fuses) ->
    lists:reverse(Fuses);
fuses_small_bytes(Fuse, <<255, Bytes/binary>>, Fuses) ->
    fuses_small_bytes(Fuse + 8, Bytes, Fuses);
fuses_small_bytes(Fuse, <<Byte, Bytes/binary>>, Fuses) ->
    fuses_small_bytes(Fuse + 8, Bytes, fuses_small_byte(8, Fuse, Byte, Fuses)).

%%--------------------------------------------------------------------

fuses_small_byte(0, _, _, Fuses) ->
    Fuses;
fuses_small_byte(N, Fuse, Byte, Fuses)
        when Fuse rem 64 =:= 0 orelse Fuse rem 64 =:= 33 ->
    % stripe but is _always_ 0
    0 = Byte band 1,
    fuses_small_byte(N - 1, Fuse + 1, Byte bsr 1, Fuses);
fuses_small_byte(N, Fuse, Byte, Fuses) when Byte band 1 =:= 0 ->
    fuses_small_byte(N - 1, Fuse + 1, Byte bsr 1, [Fuse | Fuses]);
fuses_small_byte(N, Fuse, Byte, Fuses) ->
    fuses_small_byte(N - 1, Fuse + 1, Byte bsr 1, Fuses).

%%--------------------------------------------------------------------

fuses_large_bytes(_, <<>>, Fuses) ->
    lists:reverse(Fuses);
fuses_large_bytes(Fuse, <<255, Bytes/binary>>, Fuses) ->
    fuses_large_bytes(Fuse + 8, Bytes, Fuses);
fuses_large_bytes(Fuse, <<Byte, Bytes/binary>>, Fuses) ->
    fuses_large_bytes(Fuse + 8, Bytes, fuses_large_byte(8, Fuse, Byte, Fuses)).

%%--------------------------------------------------------------------

fuses_large_byte(0, _, _, Fuses) ->
    Fuses;
fuses_large_byte(N, Fuse, Byte, Fuses)
        when Fuse rem 128 =:= 0 orelse Fuse rem 128 =:= 65 ->
    % stripe but is _always_ 0
    0 = Byte band 1,
    fuses_large_byte(N - 1, Fuse + 1, Byte bsr 1, Fuses);
fuses_large_byte(N, Fuse, Byte, Fuses) when Byte band 1 =:= 0 ->
    fuses_large_byte(N - 1, Fuse + 1, Byte bsr 1, [Fuse | Fuses]);
fuses_large_byte(N, Fuse, Byte, Fuses) ->
    fuses_large_byte(N - 1, Fuse + 1, Byte bsr 1, Fuses).

%%====================================================================
%% has_fuse
%%====================================================================

-spec has_fuse(fuse:fuse(), pof()) -> boolean().

has_fuse(Fuse, #{cfm := #{data := Bytes}}) ->
    Skip = Fuse div 8,
    Bit = 1 bsl (Fuse rem 8),
    case Bytes of
        <<_:Skip/binary, Byte, _/binary>> when Byte band Bit =:= 0 ->
            true;

        _ ->
            false
    end.

%%====================================================================
%% is_stripe
%%====================================================================

-spec is_stripe(fuse:fuse(), density()) -> boolean().

is_stripe(Fuse, max_v_240z) ->
    is_stripe_small(Fuse);
is_stripe(Fuse, max_v_570z) ->
    is_stripe_small(Fuse);
is_stripe(Fuse, max_v_1270z) ->
    is_stripe_large(Fuse);
is_stripe(Fuse, max_v_2210z) ->
    is_stripe_large(Fuse).

%%--------------------------------------------------------------------

is_stripe_small(Fuse) when Fuse rem 64 =:= 0 ->
    true;
is_stripe_small(Fuse) when Fuse rem 64 =:= 33 ->
    true;
is_stripe_small(_) ->
    false.

%%--------------------------------------------------------------------

is_stripe_large(Fuse) when Fuse rem 128 =:= 0 ->
    true;
is_stripe_large(Fuse) when Fuse rem 128 =:= 65 ->
    true;
is_stripe_large(_) ->
    false.

