-module(csv_parser).
-compile(export_all).

-ifdef(BENCHMARK).
-include_lib("emark/include/emark.hrl").
-endif.

read_record(B, M) ->
    {Field, B1, M1} = read_field(B, M),
    {Fields, B2, M2} = read_record_delimeter(B1, M1),
    {[Field|Fields], B2, M2}.


read_record_delimeter(<<$,, B>>, M) ->
    read_record(B, M);
read_record_delimeter(<<"\r\n", B>>, M) ->
    {[], B, M};
read_record_delimeter(<<"\n", B>>, M) ->
    {[], B, M};
read_record_delimeter(<<"\r", B>>, M) ->
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
            BS = 8*S,
            %% Field-delimeter was found
            <<Field:BS/binary, B2/binary>> = B,
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



-ifdef(BENCHMARK).

read_record_benchmark(N) ->
    Xs = gen_record(N, 1000),
    emark:start(?MODULE, read_record, 2),
    [?MODULE:read_record(X, undefined) || X <- Xs],
    ok.


gen_record(0, L) -> [];
gen_record(N, L) when N > 0 ->
    Field = simple_string(L),
    [ <<Field/binary, $\n>> | gen_record(N-1, L)].



simple_string(L) ->
    Rand = crypto:rand_bytes(L),
    << <<X>> || <<X>> <= Rand, X =/= $", X =/= $\n, X =/= $\r, X =/= $,>>.


-endif.
