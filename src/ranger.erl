-module(ranger).

%% API
-export([
    get_ranges/3,
    find/2
]).

-type max_value() :: non_neg_integer().
-type capacity() :: non_neg_integer().
-type section_number() :: non_neg_integer().
-type alive_sections() :: [section_number()].
-type min_range_value() :: non_neg_integer().
-type max_range_value() :: non_neg_integer().
-type range_map() :: #{{min_range_value(), max_range_value()} => section_number()}.

-spec get_ranges(max_value(), capacity(), alive_sections()) -> range_map().
get_ranges(MaxValue, Capacity, AliveList) ->
    AliveSize = erlang:length(AliveList),
    AliveListSorted = lists:sort(AliveList),
    FullList = lists:seq(0, Capacity - 1),
    DeadList = lists:filter(fun(E) -> not lists:member(E, AliveList) end, FullList),
    BaseRangeMap = distribute({0, MaxValue}, Capacity, FullList),
    redistribute(BaseRangeMap, AliveSize, AliveListSorted, DeadList).

-spec find(non_neg_integer(), range_map()) -> {ok, section_number()} | error.
find(Value, RangeMap) ->
    Iterator = maps:iterator(RangeMap),
    do_find(maps:next(Iterator), Value).

%% Internal functions

do_find(none, _) -> none;
do_find({Range, Num, Iterator}, Value) ->
    case in_range(Value, Range) of
        true -> {ok, Num};
        false -> do_find(maps:next(Iterator), Value)
    end.

in_range(Value, {Min, Max}) when Value >= Min andalso Value =< Max -> true;
in_range(_, _) -> false.

distribute(Range, Size, ListSorted) ->
    distribute(Range, Size, ListSorted, #{}).

distribute({Min, Max}, Size, ListSorted, Acc) ->
    Delta = not_zero(((Max - Min) div Size) - 1),
    SizeFromZero = Size - 1,
    {_, Result} = lists:foldl(fun
        (Num, {StartPos, Map}) when Num =:= SizeFromZero ->
            {Max, Map#{{StartPos, Max} => lists:nth(Num + 1, ListSorted)}}; %% because lists indexed from 1
        (Num, {StartPos, Map}) ->
            MaxVal = StartPos + Delta,
            {MaxVal + 1, Map#{{StartPos, MaxVal} => lists:nth(Num + 1, ListSorted)}}
    end, {Min, Acc}, lists:seq(0, SizeFromZero)),
    Result.

redistribute(BaseRangeMap, AliveSize, AliveListSorted, DeadList) ->
    maps:fold(fun(Range, RangeNum, Acc) ->
        case lists:member(RangeNum, DeadList) of
            true ->
                distribute(Range, AliveSize, AliveListSorted, Acc);
            false ->
                Acc#{Range => RangeNum}
        end
    end, #{}, BaseRangeMap).

not_zero(0) -> 1;
not_zero(Value) -> Value.

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

without_dead_test() ->
    ?assertEqual(
        #{{0,3} => 0,{4,7} => 1,{8,11} => 2,{12,16} => 3},
        get_ranges(16, 4, [0,1,2,3])
    ),
    ?assertEqual(
        #{{0,3} => 0,{4,7} => 1,{8,11} => 2,{12,17} => 3},
        get_ranges(17, 4, [0,1,2,3])
    ),
    ?assertEqual(
        #{{0,4} => 0,{5,9} => 1,{10,14} => 2,{15,21} => 3},
        get_ranges(21, 4, [0,1,2,3])
    ).

with_dead_test() ->
    ?assertEqual(
        #{
            {0,3} => 0,
            {4,5} => 0,
            {6,7} => 2,
            {8,7} => 3,
            {8,11} => 2,
            {12,16} => 3
        },
        get_ranges(16, 4, [0,2,3])
    ),
    ?assertEqual(
        #{
            {0,3} => 0,
            {4,5} => 0,
            {6,7} => 2,
            {8,11} => 2,
            {12,13} => 0,
            {14,16} => 2
        },
        get_ranges(16, 4, [0,2])
    ).

find_test() ->
    RangeMap = get_ranges(16, 4, [0,2]),
    ?assertEqual({ok, 0}, find(5, RangeMap)),
    ?assertEqual({ok, 2}, find(10, RangeMap)),
    ?assertEqual(none, find(100, RangeMap)).

-endif.