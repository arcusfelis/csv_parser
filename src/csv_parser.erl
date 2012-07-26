-module(csv_parser).
-compile(export_all).

-ifdef(BENCHMARK).
-include_lib("emark/include/emark.hrl").
-endif.

read_record(B, M) ->
    {Field, B1, M1} = read_field(B, M),
    {Fields, B2, M2} = read_record_delimeter(B1, M1),
    {[Field|Fields], B2, M2}.


read_record_delimeter(<<$,, B/binary>>, M) ->
    read_record(B, M);
read_record_delimeter(<<"\r\n", B/binary>>, M) ->
    {[], B, M};
read_record_delimeter(<<"\n", B/binary>>, M) ->
    {[], B, M};
read_record_delimeter(<<"\r", B/binary>>, M) ->
    {[], B, M};
read_record_delimeter(<<>>, undefined) ->
    {[], <<>>, undefined};
read_record_delimeter(<<>>, M) ->
    %% get more
    {B, M1} = M(),
    case B of
        <<>> -> 
            {[], B, M1}; %% eof
        _ ->
            read_record_delimeter(B, M1)
    end.
            

read_field(<<$", B/binary>>, M) ->
    read_escaped_field(B, M);
read_field(B, M) ->
    read_non_escaped_field(B, M).


read_non_escaped_field(B, M) ->
    case binary:match(B, [<<$,>>, <<$\n>>, <<$\r>>]) of
        {S, _L} ->
            %% Field-delimeter was found
            %% S in a length bytes.
            <<Field:S/binary, B2/binary>> = B,
            {Field, B2, M};
        nomatch when M =:= undefined ->
            {B, <<>>, undefined};
        nomatch ->
            {B1, M1} = M(),
            {Field, B2, M2} = read_non_escaped_field(B1, M1),
            {<<B/binary, Field/binary>>, B2, M2}
    end.


read_escaped_field(B, M) ->
    case binary:split_binary(B, <<$">>) of
        [A, B] ->
            case B of
                <<$", B1>> -> %% A ++ 2QUOTE ++ B1
                    {Field, B2, M1} = read_escaped_field(B1, M),
                    {<<A/binary, $", Field/binary>>, B2, M1};
                B1 ->
                    {A, B1, M}
            end;
        [] when M =/= undefined ->
            %% get more
            {B1, M1} = M(),
            {Field, B2, M2} = read_escaped_field(B1, M1),
            {<<B/binary, Field/binary>>, B2, M2}
    end.

-ifdef(TEST).                          
-include_lib("eunit/include/eunit.hrl").  
                                          
read_record_test_() ->                             
    [?_assertEqual(read_record(<<"a,b,c">>, undefined), 
                   {[<<$a>>, <<$b>>, <<$c>>], <<>>, undefined})
    ].

-endif.


-ifdef(BENCHMARK).

run_read_record(B, M) ->
    read_record(B, M).

read_record_benchmark(N) ->
    Xs = gen_records(N, 100, 100),
    emark:start(?MODULE, run_read_record, 2),
    [?MODULE:run_read_record(X, undefined) || X <- Xs],
    ok.


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
