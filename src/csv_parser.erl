-module(csv_parser).
-export([read_record/1,
         read_records/2,
         file_parser/1,
         binary_parser/1]).

-ifdef(BENCHMARK).
-define(USE_COMMONS, 1).
-export([run_read_record/2,
         run_read_records/3]).
-include_lib("emark/include/emark.hrl").
-endif.

-ifdef(TEST).
-define(USE_COMMONS, 1).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.


-record(csvp, {
        more_fun,
        non_escape_delim_cp =
            binary:compile_pattern([<<$,>>, <<$\n>>, <<$\r>>]),
        quote_delim_cp =
            binary:compile_pattern(<<$">>)
        }).
-define(NO_MORE(S), (S#csvp.more_fun =:= undefined)).
-define(MORE(S), (S#csvp.more_fun =/= undefined)).


file_parser(Fd) ->
    #csvp{more_fun=file_reader_hof(Fd)}.


binary_parser(Bin) ->
    hide_binary(Bin, #csvp{}).


file_reader_hof(Fd) ->
    fun() ->
            case file:read(Fd, 1000) of
                {ok, Data} -> {Data, file_reader_hof(Fd)};
                eof -> {<<>>, undefined}
            end
        end.


more(S=#csvp{more_fun = M}) when is_function(M) ->
    {B, M1} = M(),
    {B, S#csvp{more_fun = M1}}.


read_record(S=#csvp{}) ->
    {B, S1} = more(S),
    {Fields, B2, S2} = read_record2(B, S1),
    {Fields, hide_binary(B2, S2)}.


read_records(S=#csvp{}, N) when is_integer(N), N > 0, ?MORE(S) ->
    {B, S1} = more(S),
    read_records2(B, S1, N, []);
%% Already EOF?
read_records(S=#csvp{more_fun=undefined}, _N) ->
    {[], S}.


read_records2(B, S, 0, Rs) ->
    {lists:reverse(Rs), hide_binary(B, S)};

read_records2(B, S, N, Rs) ->
    case read_record2(B, S) of
        {[], _, _} ->
            %% EOF
            {lists:reverse(Rs), undefined};
        {Fields, B2, S2} ->
            read_records2(B2, S2, N-1, [Fields|Rs])
    end.


hide_binary(<<>>, S=#csvp{}) ->
    S;
hide_binary(B2, S=#csvp{more_fun=M}) ->
    S#csvp{more_fun=fun() -> {B2, M} end}.


read_record2(<<>>, S) when ?MORE(S) ->
    {B, S1} = more(S),
    read_record2(B, S1);
read_record2(<<>>, S) ->
    {[], <<>>, S};
read_record2(B, S) ->
    {Field,  B1, S1} = read_field(B, S),
    {Fields, B2, S2} = read_record_delimeter(B1, S1),
    {[Field|Fields], B2, S2}.


read_record_delimeter(<<",", B/binary>>, S) ->
    case read_record2(B, S) of
        {[], B2, S2} ->
            {[<<>>], B2, S2};
        Result ->
            Result
    end;
read_record_delimeter(<<"\r">>, S) when ?MORE(S) ->
    %% see the "More in rhe break (test 2)." test.
    {B, S1} = more(S),
    read_record_delimeter(<<"\r", B/binary>>, S1);
read_record_delimeter(<<"\r\n", B/binary>>, S) ->
    {[], B, S};
read_record_delimeter(<<"\n", B/binary>>, S) ->
    {[], B, S};
read_record_delimeter(<<"\r", B/binary>>, S) ->
    {[], B, S};
read_record_delimeter(<<" ", B/binary>>, S) ->
    read_record_delimeter(B, S);
read_record_delimeter(<<>>, S) when ?MORE(S) ->
    %% get more
    {B, S1} = more(S),
    case B of
        <<>> ->
            {[], <<>>, S1}; %% eof
        _ ->
            read_record_delimeter(B, S1)
    end;
read_record_delimeter(<<>>, S) ->
    {[], <<>>, S}.


read_field(<<$", B/binary>>, S) ->
    read_escaped_field(B, S);
read_field(<<>>, S) when ?MORE(S) ->
    {B, S1} = more(S),
    read_field(B, S1);
read_field(B, S) ->
    read_non_escaped_field(B, S).


read_non_escaped_field(B, S=#csvp{non_escape_delim_cp = Pat}) ->
    case binary:match(B, Pat) of
        {Start, _Len} ->
            %% Field-delimeter was found
            %% Start is a length in bytes.
            <<Field:Start/binary, B2/binary>> = B,
            {Field, B2, S};
        nomatch when ?NO_MORE(S) ->
            {B, <<>>, S};
        nomatch ->
            {B1, S1} = more(S),
            {Field, B2, S2} = read_non_escaped_field(B1, S1),
            {<<B/binary, Field/binary>>, B2, S2}
    end.


read_escaped_field(B, S=#csvp{quote_delim_cp = Pat}) ->
    case binary:split(B, Pat) of
        [A, B1] ->
            case B1 of
                <<$", B2/binary>> -> %% A ++ 2QUOTE ++ B1
                    {Field, B3, S1} = read_escaped_field(B2, S),
                    {<<A/binary, $", Field/binary>>, B3, S1};
                <<>> when ?MORE(S) ->
                    %% The binary was splited beetween 2 dquotes.
                    %% see the "More in rhe break." test.
                    {B2, S1} = more(S),
                    B3 = <<B/binary, B2/binary>>,
                    read_escaped_field(B3, S1);
                _ ->
                    {A, B1, S}
            end;
        [_] when ?MORE(S) ->
            %% see "More in rhe break (test 3).".
            %% get more
            {B1, S1} = more(S),
            {Field, B2, S2} = read_escaped_field(B1, S1),
            {<<B/binary, Field/binary>>, B2, S2}
    end.


-ifdef(TEST).

read_record_test_() ->
    [?_assertEqual(read_record2(<<"a,b,c">>, #csvp{}),
                   {[<<$a>>, <<$b>>, <<$c>>], <<>>, #csvp{}})
    ,?_assertEqual(read_record2(<<"a,,c">>, #csvp{}),
                   {[<<$a>>, <<>>, <<$c>>], <<>>, #csvp{}})
    ,?_assertEqual(read_record2(<<",">>, #csvp{}),
                   {[<<>>, <<>>], <<>>, #csvp{}})
    ,?_assertEqual(read_records(binary_parser(<<"a,\nc,d">>), 2),
                   {[[<<$a>>, <<>>], [<<$c>>, <<$d>>]], #csvp{}})
    ,?_assertEqual(read_record2(<<>>, #csvp{}),
                   {[], <<>>, #csvp{}})
    ,{"More in rhe break."
     ,?_assertEqual(read_record2(<<$\", $a, $\">>,
                                 hide_binary(<<$\", $\a, $\">>, #csvp{})),
                   {[<<$a, $\", $a>>], <<>>, #csvp{}})}
    ,{"More in the break (test 2)."
     ,?_assertEqual(read_records(hide_binary(<<"a\r">>,
                                             binary_parser(<<"\nb">>)),
                                 2),
                    {[[<<$a>>], [<<$b>>]], #csvp{}})}
    ,{"More in rhe break (test 3)."
     ,?_assertEqual(read_record2(<<$\", $a>>,
                                 binary_parser(<<$\a, $\">>)),
                   {[<<"aa">>], <<>>, #csvp{}})}
    ,?_assertEqual(read_records(binary_parser(<<"\na">>), 3),
                   {[[<<>>], [<<$a>>]], undefined})
    ,?_assertEqual(read_records(binary_parser(<<"\na\n">>), 3),
                   {[[<<>>], [<<$a>>]], undefined})
%% [[<<192>>,<<38,125,241,31>>],[<<50,170,82,49>>,<<>>],[<<>>,<<43,177>>],[<<202,126,255>>,<<77,233,198>>]]
    ].


parse(V, R) ->
    {Rs, _P1} = read_records(binary_parser(list_to_binary(V)), 100),
    [list_to_tuple([binary_to_list(Field) || Field <- Rec])
            || Rec <- Rs] ++ R.


%% This test is from https://github.com/afiniate/erfc_parsers
parse_test_() ->
    [ { "empty binary"
      , ?_assertEqual([], parse("", []))}
    , { "Unix LF"
      , ?_assertEqual([{"1A","1B","1C"},{"2A","2B","2C"}],
                      parse("1A,1B,1C\n2A,2B,2C", []))}
    , { "Unix LF with extra spaces after quoted element stripped"
      , ?_assertEqual([{"1A","1B","1C"},{"2A","2B","2C"}],
                      parse("\"1A\"   ,\"1B\" ,\"1C\"\n\"2A\" ,\"2B\",\"2C\"", []))}
    , { "Unix LF with extra spaces preserved in unquoted element"
      , ?_assertEqual([{" 1A ","1B","1C"},{"2A","2B","2C"}],
                      parse(" 1A ,1B,1C\n2A,2B,2C", []))}
    , { "Pre Mac OSX 10 CR"
      , ?_assertEqual([{"1A","1B","1C"},{"2A","2B","2C"}],
                      parse("1A,1B,1C\r2A,2B,2C", []))}
    , { "Windows CRLF"
      , ?_assertEqual([{"1A","1B","1C"},{"2A","2B","2C"}],
                      parse("1A,1B,1C\r\n2A,2B,2C", []))}

    , { "Quoted element"
      , ?_assertEqual([{"1A","1B"}], parse("1A,1B", []))}
    , { "Nested quoted element"
      , ?_assertEqual([{"1A","\"1B\""}],
                      parse("\"1A\",\"\"\"1B\"\"\"", []))}
    , { "Quoted element with embedded LF"
      , ?_assertEqual([{"1A","1\nB"}],
                       parse("\"1A\",\"1\nB\"", []))}
%   , { "Quoted element with embedded quotes (1)"
%     , ?_assertThrow({ecsv_exception,bad_record,0,7},
%                      parse("\"1A\",","\"\"B\"", []))}
    , { "Quoted element with embedded quotes (2)"
      , ?_assertEqual([{"1A","blah\"B"}],
                       parse("\"1A\",\"blah\"\"B\"", []))} %"
%   , { "Missing 2nd quote"
%     , ?_assertThrow({ecsv_exception,unclosed_quote,0,8},
%                      parse("\"1A\",\"2B", []))}
%   , { "Bad record size"
%     , ?_assertThrow({ecsv_exception,bad_record_size, _, _},
%                     parse("1A,1B,1C\n2A,2B\n", []))}
    ].

%% -------------------------------------------------------------------
%% Property Testing
%% -------------------------------------------------------------------


run_property_testing_test_() ->
    {timeout, 5000, fun run_property_testing_case/0}.


run_property_testing_case() ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    Res = proper:module(?MODULE, [{constraint_tries, 500}]),
    erlang:group_leader(EunitLeader, self()),
    ?assertEqual([], Res).


prop_read_records() ->
    ?FORALL(DecodedRecords, records(),
        equals(DecodedRecords,
               encode_than_decode_records(DecodedRecords))).

encode_than_decode_records(DecodedRecords) ->
    EncodedRecords = encode_records(DecodedRecords),
    RowsCount = length(DecodedRecords),
    Parser = binary_parser(EncodedRecords),
    {Recs, _P1} = read_records(Parser, RowsCount),
    Recs.


prop_more_fun() ->
    ?FORALL(DecodedRecords, records(),
        equals(DecodedRecords,
               encode_than_tokenize_then_decode_records(DecodedRecords))).


encode_than_tokenize_then_decode_records(DecodedRecords) ->
    EncodedRecords = encode_records(DecodedRecords),
    Tokenized = bin_split(EncodedRecords),
    RowsCount = length(DecodedRecords),
    Parser = hide_binaries(Tokenized, #csvp{}),
    {Recs, _P1} = read_records(Parser, RowsCount),
    Recs.


bin_split(B) when is_binary(B) ->
    [<<X>> || <<X>> <= B].


hide_binaries(Bins, S=#csvp{more_fun = M}) ->
    S#csvp{more_fun = hide_binaries_hof(Bins ++ [M])}.


hide_binaries_hof([M]) ->
    M;
hide_binaries_hof([B|Bs]) ->
    fun() -> {B, hide_binaries_hof(Bs)} end.


extract_records({Rs, _P}) ->
    Rs.

%% @doc Generate `[[binary()]]' with fixed count of binaries.
-spec records() -> proper_types:type().
records() ->
    ?SUCHTHAT(Records, 
              records2(),
              Records =:= [] orelse lists:last(Records) =/= [<<>>]).

records2() ->
    ?LET(FieldCount,
         range(1, 100),
         list(record(FieldCount))).


-spec record(non_neg_integer()) -> proper_types:type().
record(FieldCount) ->
    vector(FieldCount, binary()).


encode_records_test_() ->
    [?_assertEqual(encode_records([[<<>>],[<<0>>]]), <<$\n, 0>>)].

encode_records(Rs) ->
    Rs1 = [encode_fields(R) || R <- Rs],
    bjoin(Rs1, <<$\n>>).

encode_fields(Fs) ->
    Fs1 = [encode_field(F) || F <- Fs],
    bjoin(Fs1, <<$,>>).

encode_field(F) ->
    case binary:match(F, [<<$,>>, <<$\n>>, <<$\r>>, <<$">>]) of
        nomatch ->
            F; %% no changes
        _SL ->
            F1 = binary:replace(F, <<$">>, <<$", $">>, [global]),
            escape_field(F1)
    end.

escape_field(F1) ->
    <<$", F1/binary, $">>.
    

-endif.


-ifdef(BENCHMARK).

run_read_record(B, P) ->
    read_record2(B, P).

run_read_records(N, B, P) ->
    read_records(hide_binary(B, P), N).

read_record_benchmark(N) ->
    Xs = gen_records(N, 100, 100),
    P = #csvp{},
    emark:start(?MODULE, run_read_record, 2),
    [?MODULE:run_read_record(X, P) || X <- Xs],
    ok.


read_records_benchmark(N) ->
     Xs = gen_records(N, 20, 15, 10),
     P = #csvp{},
     emark:start(?MODULE, run_read_records, 3),
     [?MODULE:run_read_records(N, X, P) || X <- Xs],
     ok.


gen_records(0, _L1, _L2, L3) ->
    [];
gen_records(N, L1, L2, L3) ->
    [bjoin(gen_records(L1, L2, L3), <<$\r, $\n>>) | gen_records(N-1, L1, L2, L3)].


gen_records(0, _L1, _L2) ->
    [];
gen_records(N, L1, L2) ->
    [gen_record(L1, L2) | gen_records(N-1, L1, L2)].


gen_record(N, L) ->
    bjoin(gen_record1(N, L), <<$,>>).


gen_record1(0, L) -> [];
gen_record1(N, L) when N > 0 ->
    [ simple_string(L) | gen_record1(N-1, L)].



simple_string(L) ->
    Rand = crypto:rand_bytes(L),
    << <<X>> || <<X>> <= Rand, X =/= $", X =/= $\n, X =/= $\r, X =/= $,>>.

-endif.


-ifdef(USE_COMMONS).

bjoin([], _Del) ->
    <<>>;
bjoin([B|Bs], Del) ->
    X = << <<Del/binary, B/binary>> || B <- Bs >>,
    <<B/binary, X/binary>>.

-endif.
