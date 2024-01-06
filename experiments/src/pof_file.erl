-module(pof_file).

-export([read/1]).
-export([decode/1]).
-export([fuse_count/1]).
-export([fuses/1]).
-export([unknown_fuses/2]).
-export([has_fuse/2]).

-export_type([pof/0]).
-export_type([flash/0]).

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

-spec decode(binary()) -> {ok, pof()}.

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
    decode_skip(<<0,0>>, Data, Parts);
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

%%--------------------------------------------------------------------

decode_skip(Expect, Data, Parts) ->
    Expect = Data,
    Parts.

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
    Fuses = [0, 33, 46, 47, 53, 54, 59, 60],
    ?assertEqual(Fuses, fuses(POF)),
    [
        ?assertEqual(lists:member(Fuse, Fuses), has_fuse(Fuse, POF))
        ||
        Fuse <- lists:seq(0, bit_size(CFM) - 1)
    ],
    Unknowns = [
        {36, 0},
        {38, 1},
        {44, 0},
        {46, 1},
        {52, 0},
        {54, 1},
        {60, 0},
        {62, 1}
    ],
    UnknownFuses = [38, 60, 62],
    ?assertEqual(UnknownFuses, unknown_fuses(POF, Unknowns)),
    ok.

-endif.

%%--------------------------------------------------------------------

-spec fuses(pof()) -> [fuse:fuse()].

fuses(#{cfm := #{data := Bytes}}) ->
    fuses_bytes(0, Bytes, []).

%%--------------------------------------------------------------------

fuses_bytes(_, <<>>, Fuses) ->
    lists:reverse(Fuses);
fuses_bytes(Fuse, <<255, Bytes/binary>>, Fuses) ->
    fuses_bytes(Fuse + 8, Bytes, Fuses);
fuses_bytes(Fuse, <<Byte, Bytes/binary>>, Fuses) ->
    fuses_bytes(Fuse + 8, Bytes, fuses_byte(8, Fuse, Byte, Fuses)).

%%--------------------------------------------------------------------

fuses_byte(0, _, _, Fuses) ->
    Fuses;
fuses_byte(N, Fuse, Byte, Fuses) when Byte band 1 =:= 0 ->
    fuses_byte(N - 1, Fuse + 1, Byte bsr 1, [Fuse | Fuses]);
fuses_byte(N, Fuse, Byte, Fuses) ->
    fuses_byte(N - 1, Fuse + 1, Byte bsr 1, Fuses).

%%====================================================================
%% unknown_fuses
%%====================================================================

-spec unknown_fuses(pof(), [{fuse:fuse(), 0 | 1}]) -> [fuse:fuse()].

unknown_fuses(#{cfm := #{data := Bytes}}, Unknown) ->
    unknown_bytes(0, Bytes, Unknown, []).

%%--------------------------------------------------------------------

unknown_bytes(_, <<>>, _, Fuses) ->
    lists:reverse(Fuses);
unknown_bytes(_, _, [], Fuses) ->
    lists:reverse(Fuses);
unknown_bytes(Fuse, <<Byte, Bytes/binary>>, Unknown, Fuses) ->
    unknown_byte(8, Fuse, Byte, Bytes, Unknown, Fuses).

%%--------------------------------------------------------------------

unknown_byte(0, Fuse, _, Bytes, Unknown, Fuses) ->
    unknown_bytes(Fuse, Bytes, Unknown, Fuses);
unknown_byte(N, Fuse, Byte, Bytes, [{Fuse, Bit} | Unknown], Fuses) ->
    case Byte band 1 of
        Bit ->
            unknown_byte(N - 1, Fuse + 1, Byte bsr 1, Bytes, Unknown, [Fuse | Fuses]);

        _ ->
            unknown_byte(N - 1, Fuse + 1, Byte bsr 1, Bytes, Unknown, Fuses)
    end;
unknown_byte(N, Fuse, Byte, Bytes, Unknown, Fuses) ->
    unknown_byte(N - 1, Fuse + 1, Byte bsr 1, Bytes, Unknown, Fuses).

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

