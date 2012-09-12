-module(streamsink_parser).

-export([init/0, parse/2]).

-type phase() :: reading_status | reading_headers | reading_stream.

-record(parser_state, {phase = reading_status :: phase(),
                       buffer = <<>> :: binary(), 
                       headers=[], 
                       mp3_chunksize :: integer() | undefined, 
                       mp3_chunkbytes=0 :: integer()}).
-type parser() :: #parser_state{}.
-type header() :: {binary(), binary()}.
-type headers() :: [header()].
-type metatag() :: {binary(), binary()}.
-type metatags() :: [metatag()].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec init() -> parser().
init() ->
    #parser_state{}.

-spec parse(binary(), parser()) -> 
    {more, parser()} |
    {empty, parser()} |
    {{status, binary()}, parser()} |
    {{headers, headers()}, parser()} |
    {{data, binary(), metatags()}, parser()}.
parse(<<>>, State = #parser_state{buffer= <<>>}) ->
    {empty, State};
parse(Bytes, State = #parser_state{phase=reading_status, buffer=Buffer}) ->
    AllBytes = <<Buffer/binary, Bytes/binary>>,
    case erlang:decode_packet(line, AllBytes, []) of
        {ok, <<"ICY 200 OK\r\n">>, Rest} -> 
            {{status, <<"ICY 200 OK">>},
             State#parser_state{phase=reading_headers, buffer=Rest}};
        {more, _Length} -> 
            {more, State#parser_state{buffer=AllBytes}}
    end;
parse(Bytes, State = #parser_state{phase=reading_headers, buffer=Buffer, headers=Headers}) ->
    AllBytes = <<Buffer/binary, Bytes/binary>>,
    case erlang:decode_packet(httph, AllBytes, []) of
        {ok, http_eoh, Rest} -> 
            ChuckSize = case proplists:get_value("Icy-Metaint", Headers) of
                            undefined ->
                                undefined;
                            Value ->
                                list_to_integer(Value)
                        end,
            {{headers, Headers}, 
             State#parser_state{phase=reading_stream, mp3_chunksize=ChuckSize, buffer=Rest}};
        {ok, {http_header, _, Name, _, Value}, Rest} -> 
            Header = {Name, Value},
            {more, State#parser_state{buffer=Rest, headers=[Header|Headers]}};
        {more, _Length} -> 
            {more, State#parser_state{buffer=AllBytes}}
    end;
parse(Bytes, State = #parser_state{phase=reading_stream,
                                   mp3_chunksize=undefined}) ->
    % If mp3_chunksize is undefined, that means all bytes in the stream
    % are mp3 bytes
    {{data, Bytes, []}, State};
parse(Bytes, State = #parser_state{phase=reading_stream,
                                   mp3_chunksize=ChunkSize,
                                   buffer=Buffer,
                                   mp3_chunkbytes=ChunkBytes}) ->
    {MP3Bytes, MetaData, Rest, BytesRead} =
        decode_stream(<<Buffer/binary, Bytes/binary>>,
                      ChunkSize, ChunkBytes),

    {{data, MP3Bytes, MetaData}, 
     State#parser_state{buffer=Rest,
                        mp3_chunkbytes=BytesRead}}.

% internal
-spec decode_stream(binary(), integer(), integer()) ->
    {MP3Bytes :: binary(), MetaData :: [{binary(),binary()}], Rest :: binary(), BytesRead :: integer()}.
decode_stream(Bytes, ChunkSize, BytesRead) 
  when size(Bytes) + BytesRead =< ChunkSize ->
    {Bytes, [], <<>>, size(Bytes) + BytesRead};
decode_stream(Bytes, ChunkSize, BytesRead) ->
    MP3Size = ChunkSize - BytesRead,
    <<MP3Bytes:MP3Size/bytes, MetaBytes/binary>> = Bytes,

    case decode_meta(MetaBytes) of
        {MetaData, Rest} ->
            {MP3Bytes, MetaData, Rest, 0};
        more ->
            {MP3Bytes, [], MetaBytes, ChunkSize}
    end.

-spec decode_meta(binary()) -> {metatags(), binary()} | more.
decode_meta(<<MetaLenDiv16:8, MetaPacket/binary>>) ->
    MetaLen = MetaLenDiv16 * 16,
    case MetaPacket of
        <<MetaDataBytes:MetaLen/bytes, Rest/binary>> ->
            {parse_meta(MetaDataBytes), Rest};
        _ ->
            more
    end.
    
-spec parse_meta(binary()) -> metatags().
parse_meta(Data) ->
    parse_meta(name, [], {<<>>, <<>>}, Data).

parse_meta(name, Accum, Item, <<0:8, _/binary>>) ->
    parse_meta(name, Accum, Item, <<>>);
parse_meta(name, Accum, _, <<>>) ->
    Accum;
parse_meta(name, Accum, Item, <<"='", Rest/binary>>) ->
    parse_meta(value, Accum, Item, Rest);
parse_meta(name, Accum, {Name, Value}, <<Byte:1/bytes, Rest/binary>>) ->
    Name1 = <<Name/binary, Byte/binary>>,
    parse_meta(name, Accum, {Name1, Value}, Rest);
parse_meta(value, Accum, Item, <<"';", Rest/binary>>) ->
    parse_meta(name, [Item|Accum], {<<>>,<<>>}, Rest);
parse_meta(value, Accum, {Name, Value}, <<Byte:1/bytes, Rest/binary>>) ->
    Value1 = <<Value/binary, Byte/binary>>,
    parse_meta(value, Accum, {Name, Value1}, Rest).

    
-ifdef(TEST).
parse_meta_test() ->
    Fixture = <<"StreamTitle='[LIVE] GourmetBeats Show with JoeNice';StreamUrl='http://www.sub.fm/';^@^@^@^@^@^@^@^@^@^@^@^@^@">>,

    ?assertEqual([{<<"StreamUrl">>, <<"http://www.sub.fm/">>},
                  {<<"StreamTitle">>, <<"[LIVE] GourmetBeats Show with JoeNice">>}],
                 parse_meta(name, [], {<<>>, <<>>}, Fixture)).


decode_meta_test_() ->
    Field = <<"abc='foo';", 0:48>>,
    Expected = [{<<"abc">>, <<"foo">>}],

    [
     ?_assertEqual(more,
                   decode_meta(<<1, "abc">>)),

     ?_assertEqual({Expected, <<>>},
                   decode_meta(<<1, Field/binary>>)),

     ?_assertEqual({Expected, <<"def">>},
                   decode_meta(<<1, Field/binary, "def">>))
     ].

decode_stream_test_() ->
    [
     ?_assertEqual({<<"abc">>, [], <<>>, 3},
                   decode_stream(<<"abc">>, 10, 0)),
     ?_assertEqual({<<"abc">>, [], <<>>, 10},
                   decode_stream(<<"abc">>, 10, 7))
     ].

parse_test_() ->
    MetaFieldBuffer1 = <<"abc='foo';">>,
    MetaFieldBuffer2 = <<0:48>>,
    MetaField = <<MetaFieldBuffer1/binary, MetaFieldBuffer2/binary>>,
    MetaExpected = [{<<"abc">>, <<"foo">>}],

    [
     % reading status
     ?_assertEqual({more,
                    #parser_state{phase=reading_status, buffer= <<"ICY 200 OK">>}},
                   parse(<<"ICY 200 OK">>, #parser_state{})),
     ?_assertEqual({{status, <<"ICY 200 OK">>},
                    #parser_state{phase=reading_headers, buffer= <<>>}},
                   parse(<<"ICY 200 OK\r\n">>, #parser_state{})),
     ?_assertEqual({{status, <<"ICY 200 OK">>},
                    #parser_state{phase=reading_headers, buffer= <<"icy-">>}},
                   parse(<<"ICY 200 OK\r\nicy-">>, #parser_state{})),
     ?_assertEqual({{status, <<"ICY 200 OK">>},
                   #parser_state{phase=reading_headers, buffer= <<>>}},
                   parse(<<" 200 OK\r\n">>, #parser_state{buffer= <<"ICY">>})),

     % reading headers
     ?_assertEqual({more,
                    #parser_state{phase=reading_headers,
                                  buffer= <<"icy-metaint: 1">>}},
                   parse(<<"icy-metaint: 1">>,
                         #parser_state{phase=reading_headers})),

     ?_assertEqual({more,
                    #parser_state{phase=reading_headers,
                                  buffer= <<"icy-metaint: 1\r\n">>}},
                   parse(<<"icy-metaint: 1\r\n">>,
                         #parser_state{phase=reading_headers})),

     ?_assertEqual({more,
                    #parser_state{phase=reading_headers,
                                  headers=[{"Icy-Metaint", "1"}],
                                  buffer= <<"\r\n">>}},
                   parse(<<"icy-metaint: 1\r\n\r\n">>,
                         #parser_state{phase=reading_headers})),
     
     ?_assertEqual({{headers, []},
                    #parser_state{phase=reading_stream, 
                                  mp3_chunksize=undefined,
                                  buffer= <<>>}},
                   parse(<<"\r\n">>,
                         #parser_state{phase=reading_headers,
                                       headers=[]})),

     ?_assertEqual({{headers, [{"Icy-Metaint", "8196"}]},
                    #parser_state{phase=reading_stream,
                                  mp3_chunksize=8196,
                                  headers=[{"Icy-Metaint", "8196"}],
                                  buffer= <<>>}},
                   parse(<<"\r\n">>,
                         #parser_state{phase=reading_headers,
                                       headers=[{"Icy-Metaint", "8196"}]})),

     
     ?_assertEqual({{headers, []},
                    #parser_state{phase=reading_stream, buffer= <<"a">>}},
                   parse(<<"\r\na">>,
                         #parser_state{phase=reading_headers})),

     % reading stream
     % no metaint
     ?_assertEqual({{data, <<"abc">>, []},
                    #parser_state{phase=reading_stream}},
                   parse(<<"abc">>,
                         #parser_state{phase=reading_stream})),

     % meta chunk not reached
     ?_assertEqual({{data, <<"abc">>, []},
                    #parser_state{phase=reading_stream,
                                  mp3_chunkbytes=3,
                                  mp3_chunksize=10}},
                   parse(<<"abc">>,
                         #parser_state{phase=reading_stream,
                                      mp3_chunksize=10})),

     % meta chunk reached but not full
     ?_assertEqual({{data, <<"abc">>, []},
                    #parser_state{phase=reading_stream,
                                  buffer= <<1, "buf">>,
                                  mp3_chunkbytes=10,
                                  mp3_chunksize=10}},
                   parse(<<"abc", 1, "buf">>,
                         #parser_state{phase=reading_stream,
                                       mp3_chunkbytes=7,
                                       mp3_chunksize=10})),
     % meta chunk complete
     ?_assertEqual({{data, <<"abc">>, MetaExpected},
                    #parser_state{phase=reading_stream,
                                  buffer= <<>>,
                                  mp3_chunkbytes=0,
                                  mp3_chunksize=10}},
                   parse(<<"abc", 1, MetaField/binary>>,
                         #parser_state{phase=reading_stream,
                                       mp3_chunkbytes=7,
                                       mp3_chunksize=10})),

     % meta chunk complete buffered
     ?_assertEqual({{data, <<>>, MetaExpected},
                    #parser_state{phase=reading_stream,
                                  buffer= <<>>,
                                  mp3_chunkbytes=0,
                                  mp3_chunksize=10}},
                   parse(MetaFieldBuffer2,
                         #parser_state{phase=reading_stream,
                                       buffer= <<1, MetaFieldBuffer1/binary>>,
                                       mp3_chunkbytes=10,
                                       mp3_chunksize=10})),
     ?_assertEqual({empty, #parser_state{buffer= <<>>}},
                   parse(<<>>, #parser_state{buffer= <<>>}))
     ].
    

-endif.
