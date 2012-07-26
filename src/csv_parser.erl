-module(csv_parser).
-compile(export_all).

-ifdef(BENCHMARK).
-include_lib("emark/include/emark.hrl").
-endif.


-record(csvp, {
        more_fun,
        non_escape_delim_cp = 
            binary:compile_pattern([<<$,>>, <<$\n>>, <<$\r>>]),
        quote_delim_cp = 
            binary:compile_pattern(<<$">>)
        }).
-define(NO_MORE(S), (S#csvp.more_fun =:= undefined)).




prof() ->
    csv_parser:read_record2(<<"a,b,c,a,s,d,g,u,y,e,q,w,e,r,t,y,u,i">>, #csvp{}).


bench() ->
    {ok, Fd} = file:open("example.csv", [binary]),
    P = file_parser(Fd),
    bench_cycle(P, 100, 100000).


bench_cycle(P, L, N) when N > 0 ->
    %% Ignore records
    {_R, P1} = read_records(P, L),
    bench_cycle(P1, L, N-L);
bench_cycle(_P, _L, _N) ->
    ok.


file_parser(Fd) ->
    #csvp{more_fun=file_reader_hof(Fd)}.


file_reader_hof(Fd) ->
    fun() ->
            case file:read(Fd, 1000) of
                {ok, Data} -> {Data, file_reader_hof(Fd)};
                eof -> {<<>>, undefined}
            end
        end.


read_record(S=#csvp{more_fun=M}) ->
    {B, M1} = M(),
    {Fields, B2, S2} = read_record2(B, S#csvp{more_fun=M1}),
    {Fields, hide_binary(B2, S2)}.


read_records(S=#csvp{more_fun=M}, N) when N > 0, is_function(M) ->
    {B, M1} = M(),
    S1 = S#csvp{more_fun=M1},
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


read_record2(<<>>, S=#csvp{more_fun=undefined}) ->
    {[], <<>>, S};
read_record2(<<>>, S=#csvp{more_fun=M}) ->
    {B, M1} = M(),
    {[], B, S#csvp{more_fun=M1}};
read_record2(B, S) ->
    {Field,  B1, S1} = read_field(B, S),
    {Fields, B2, S2} = read_record_delimeter(B1, S1),
    {[Field|Fields], B2, S2}.


read_record_delimeter(<<$,, B/binary>>, S) ->
    read_record2(B, S);
read_record_delimeter(<<"\r\n", B/binary>>, S) ->
    {[], B, S};
read_record_delimeter(<<"\n", B/binary>>, S) ->
    {[], B, S};
read_record_delimeter(<<"\r", B/binary>>, S) ->
    {[], B, S};
read_record_delimeter(<<" ", B/binary>>, S) ->
    read_record_delimeter(B, S);
read_record_delimeter(<<>>, S = #csvp{more_fun = undefined}) ->
    {[], <<>>, S};
read_record_delimeter(<<>>, S) ->
    %% get more
    {B, S1} = S(),
    case B of
        <<>> -> 
            {[], B, S1}; %% eof
        _ ->
            read_record_delimeter(B, S1)
    end.
            

read_field(<<$", B/binary>>, S) ->
    read_escaped_field(B, S);
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
            M = S#csvp.more_fun,
            {B1, M1} = M(),
            S1 = S#csvp{more_fun=M1},
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
                _ ->
                    {A, B1, S}
            end;
        [] when ?NO_MORE(S) ->
            %% get more
            {B1, S1} = S(),
            {Field, B2, S2} = read_escaped_field(B1, S1),
            {<<B/binary, Field/binary>>, B2, S2}
    end.

-ifdef(TEST).                          
-include_lib("eunit/include/eunit.hrl").  
                                          
read_record_test_() ->                             
    [?_assertEqual(read_record2(<<"a,b,c">>, #csvp{}), 
                   {[<<$a>>, <<$b>>, <<$c>>], <<>>, #csvp{}})
    ,?_assertEqual(read_record2(<<>>, #csvp{}), 
                   {[], <<>>, #csvp{}})
    ].


parse(V, R) ->
    P = #csvp{},
    {Rs, _P1} = read_records(hide_binary(list_to_binary(V), P), 100),
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


bjoin([B|Bs], Del) ->
    X = << <<Del/binary, B/binary>> || B <- Bs >>,
    <<B/binary, X/binary>>.

-endif.
