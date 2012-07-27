-module(csv_qlc).
-export([table/2]).

table(Parser, Converter) ->
    TraverseFun = traverse_fun(Parser, Converter),
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
