-module(csv_record).
-export([create_converter/2,
         decode/2]).

-record(converter, {
        name :: atom(),
        %% Stores positions of required fields in the CSV file in
        %% required order.
        field_positions :: [non_neg_integer()]
}).

%% @doc Converters allows convert data cvs file records to Erlang records.
%%      `Record' is the wanted format: 
%%      `#product{name = <<"Name">>, price = <<"Price">>}'.
%%      `CsvHeaders' is usually a first record in a CSV file:
%%      `[<<"Name">>, <<"Count">>, <<"Price">>]'.
%%
%%      Fields in the `Record' can be skipped or in the different
%%      with the file order.
-spec create_converter(Record, CsvHeaders) -> Converter when
    Record :: RecordName | FieldName,
    RecordName :: atom(),
    FieldName :: binary(),
    Converter :: tuple(),
    CsvHeaders :: [FieldName].

create_converter(Rec, CsvHeader) ->
    [RecordName | RecFieldNames] = tuple_to_list(Rec),
    Ps = find_field_positions(RecFieldNames, CsvHeader),
    #converter{name=RecordName, field_positions=Ps}.


decode(#converter{name = RecordName, field_positions = Ps}, CsvList) ->
    CsvTuple = list_to_tuple(CsvList),
    Fields = [case Pos of 
                   0 -> undefined; 
                _ -> try erlang:element(Pos, CsvTuple) catch _:_ -> throw([Pos, CsvTuple]) end
              end || Pos <- Ps],
    list_to_tuple([RecordName|Fields]).

    
    

-spec find_field_positions(RecFieldNames, CsvHeader) -> Positions when
        CsvHeader :: [Name],
        RecFieldNames :: [Name],
        Name :: binary(),
        Positions :: [Pos],
        Pos :: non_neg_integer().

find_field_positions(RecFieldNames, CsvHeader) ->
    [pos(FieldName, CsvHeader) || FieldName <- RecFieldNames].


pos(H, List) -> 
    calc_pos(H, List, 1).

calc_pos(H, [H|_], N) -> 
    N;
calc_pos(H, [_|T], N) -> 
    calc_pos(H, T, N+1);
calc_pos(_, [], _) -> 
    0. %% undefined


-ifdef(TEST).                          
-include_lib("eunit/include/eunit.hrl").  
                                          
-record(product, {name, price}).
-record(book, {position, title}).

product_converter() ->
    create_converter(#product{name = <<"Name">>, price = <<"Price">>}, 
                     [<<"Name">>, <<"Count">>, <<"Price">>]).

book_converter() ->
    create_converter(#book{title = <<"Title">>}, 
                     [<<"Title">>]).

create_converter_test_() ->                             
    [?_assertEqual(create_converter(#product{name = <<"Name">>, price = <<"Price">>}, 
                                    [<<"Name">>, <<"Count">>, <<"Price">>]), 
                   #converter{name = product, field_positions = [1,3]})
    ].

decode_test_() ->                             
    [?_assertEqual(decode(product_converter(),  
                          [<<"microphone">>, <<"10">>, <<"30">>]), 
                   #product{name = <<"microphone">>, price = <<"30">>})
    ,{"Test a skipped field."
     ,?_assertEqual(decode(book_converter(),  
                          [<<"Code Complete">>]), 
                    #book{title = <<"Code Complete">>, position = undefined})}
    ].

-endif.
