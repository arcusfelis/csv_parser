#!/usr/bin/env escript

-include_lib("stdlib/include/qlc.hrl").

-record(state, {name, capital}).

main([Path]) ->
    load_deps(),
    {ok, Fd} = file:open(Path, [binary]),
    Parser = csv_parser:file_parser(Fd),
    {Header, Parser2} = csv_parser:read_record(Parser),
    NameRec = #state{name = <<"name">>, 
                     capital = <<"capital">>},
    Converter = csv_record:create_converter(NameRec, Header),
    QlcTable = csv_qlc:table(Parser2, Converter),
    QlcQuery = qlc:q([print_state(Doc) || Doc <- QlcTable]),
    qlc:e(QlcQuery),
    file:close(Fd),
    ok.


print_state(#state{name = Name, capital = Capital}) ->
    io:format("Name: ~s\n\tCapital: ~s\n\n", [Name, Capital]),
    ok.


load_deps() ->
    ScriptDir = filename:dirname(escript:script_name()),
    [ code:add_pathz(Dir)
        || Dir <- filelib:wildcard(ScriptDir ++ "/../ebin") ],
    ok.
