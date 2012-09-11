-module(streamsink_parser).

-export([init/0, parse/2]).

-record(parser_state, {phase = reading_status, buffer = <<>>, headers=[]}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec init() -> #parser_state{}.
init() ->
    #parser_state{}.

-spec parse(binary(), #parser_state{}) -> 
    {MP3Bytes :: binary(), MetaDataBytes :: binary(), #parser_state{}}.
parse(Bytes, State = #parser_state{phase=reading_status, buffer=Buffer}) ->
    AllBytes = <<Buffer/binary, Bytes/binary>>,
    case erlang:decode_packet(line, AllBytes, []) of
        {ok, <<"ICY 200 OK\r\n">>, Rest} -> 
            State#parser_state{phase=reading_headers, buffer=Rest};
        {more, _Length} -> 
            State#parser_state{buffer=AllBytes}
    end;
parse(Bytes, State = #parser_state{phase=reading_headers, buffer=Buffer, headers=Headers}) ->
    AllBytes = <<Buffer/binary, Bytes/binary>>,
    case erlang:decode_packet(line, AllBytes, []) of
        {ok, <<>>, Rest} -> 
            State#parser_state{phase=reading_stream, buffer=Rest};
        {ok, Header, Rest} -> 
            State#parser_state{buffer=Rest, headers=[Header|Headers]};
        {more, _Length} -> 
            State#parser_state{buffer=AllBytes}
    end;
parse(_Bytes, State) ->
    {<<>>,<<>>,State}.

    
-ifdef(TEST).
parse_test_() ->
    [
     ?_assertEqual(#parser_state{phase=reading_headers, buffer= <<>>},
                   parse(<<"ICY 200 OK\r\n">>, #parser_state{})),
     ?_assertEqual(#parser_state{phase=reading_headers, buffer= <<"icy-">>},
                   parse(<<"ICY 200 OK\r\nicy-">>, #parser_state{})),
     ?_assertEqual(#parser_state{phase=reading_headers, buffer= <<>>},
                   parse(<<" 200 OK\r\n">>, #parser_state{buffer= <<"ICY">>})),

     ?_assertEqual(#parser_state{phase=reading_headers, buffer= <<>>, headers=[<<"icy-metaint: 1\r\n">>]},
                   parse(<<"icy-metaint: 1\r\n">>,
                         #parser_state{phase=reading_headers})),
     ?_assertEqual(#parser_state{phase=reading_headers, buffer= <<>>, headers=[<<"icy-metaint: 1\r\n">>]},
                   parse(<<"metaint: 1\r\n">>,
                         #parser_state{phase=reading_headers, buffer= <<"icy-">>}))

     ].
    

-endif.
