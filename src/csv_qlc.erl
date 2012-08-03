-module(csv_qlc).
-export([table/2, table/3]).

%% @equiv table(Parser, Converter, []) 
table(Parser, Converter) ->
    table(Parser, Converter, []).


%% Params: [{position_field, Pos}]
-spec table(Parser, Converter, Params) -> Table when
    Table  :: qlc:table(),
    Parser :: csv_parser:parser(),
    Params :: [Param],
    Converter :: fun(),
    Param  :: {position_field, non_neg_integer()}
            | {start_position, integer()}.

table(Parser, Converter, Params) ->
    TraverseFun = 
    case proplists:get_value(position_field, Params) of
        undefined ->
            traverse_fun(Parser, Converter);
        FieldIdx ->
            Start = proplists:get_value(start_position, Params, 1),
            traverse_fun_with_index(Parser, Converter, FieldIdx, Start)
    end,
    qlc:table(TraverseFun, []).


traverse_fun(Parser, Converter) -> 
    fun() ->
        {Records, Parser2} = csv_parser:read_records(Parser, 20),
        LastTail = 
        case Parser2 of
            undefined -> %% EOF
                [];
            _ ->
                traverse_fun(Parser2, Converter)
        end,
        decode_list(lists:reverse(Records), Converter, LastTail)
        end.


%% It is here, because it allows to have one less `lists:reverse'.
decode_list([R|Rs], Converter, Ds) ->
    D = csv_record:decode(Converter, R),
    decode_list(Rs, Converter, [D|Ds]);

decode_list([], _Converter, Ds) ->
    Ds.


traverse_fun_with_index(Parser, Converter, FieldIdx, Start) ->
    fun() ->
        {Records, Parser2} = csv_parser:read_records(Parser, 20),
        LastTail = 
        case Parser2 of
            undefined -> %% EOF
                [];
            _ ->
                traverse_fun_with_index(Parser2, Converter, FieldIdx, Start+20)
        end,
        Ds = decode_list(lists:reverse(Records), Converter, LastTail),
        add_index(Ds, FieldIdx, Start)
        end.


add_index([D|Ds], FieldIdx, FilePos) ->
    D2 = erlang:setelement(FieldIdx, D, FilePos),
    [D2|add_index(Ds, FieldIdx, FilePos+1)];
add_index(Tail, _FieldIdx, _FilePos) ->
    Tail.
    


-ifdef(TEST).                          
-include_lib("eunit/include/eunit.hrl").  
                                          
-record(product, {name, type, price, index}).

ikea_data() ->
    "name,type,price\n"
    "HEMNES,Coffee table,99\n"
    "LACK,Coffee table,39\n"
    "BEDDINGE,Cushion,34\n".

table_with_positions_test() ->
    Parser = csv_parser:binary_parser(list_to_binary(ikea_data())),
    {Header, Parser2} = csv_parser:read_record(Parser),
    NameRec = #product{name = <<"name">>, 
                       type = <<"type">>,
                      price = <<"price">>},
    Converter   = csv_record:create_converter(NameRec, Header),
    QlcTable    = csv_qlc:table(Parser2, Converter, 
                                [{position_field, #product.index}]),
    Cursor      = qlc:cursor(QlcTable),
    ?assertEqual(qlc_next(Cursor), #product{name = <<"HEMNES">>, 
                                            type = <<"Coffee table">>, 
                                            price = <<"99">>,
                                            index = 1}),
    ?assertEqual(qlc_next(Cursor), #product{name = <<"LACK">>, 
                                            type = <<"Coffee table">>, 
                                            price = <<"39">>,
                                            index = 2}),
    ok.

table_without_postions_test() ->
    Parser = csv_parser:binary_parser(list_to_binary(ikea_data())),
    {Header, Parser2} = csv_parser:read_record(Parser),
    NameRec = #product{name = <<"name">>, 
                       type = <<"type">>,
                      price = <<"price">>},
    Converter   = csv_record:create_converter(NameRec, Header),
    QlcTable    = csv_qlc:table(Parser2, Converter),
    Cursor      = qlc:cursor(QlcTable),
    ?assertEqual(qlc_next(Cursor), #product{name = <<"HEMNES">>, 
                                            type = <<"Coffee table">>, 
                                            price = <<"99">>}),
    ?assertEqual(qlc_next(Cursor), #product{name = <<"LACK">>, 
                                            type = <<"Coffee table">>, 
                                            price = <<"39">>}),
    ok.

qlc_next(Cursor) ->
    case qlc:next_answers(Cursor, 1) of
        0   -> undefined;
        [X] -> X
    end.

-endif.
