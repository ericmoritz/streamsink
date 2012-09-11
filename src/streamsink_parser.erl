-module(streamsink_parser).

-export([init/0, parse/2]).

-type phase() :: reading_status | reading_headers | reading_stream.

-record(parser_state, {phase = reading_status :: phase(),
                       buffer = <<>> :: binary(), 
                       headers=[], 
                       mp3_chunksize :: integer() | undefined, 
                       mp3_chunkbytes=0 :: integer()}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec init() -> #parser_state{}.
init() ->
    #parser_state{}.

-spec parse(binary(), #parser_state{}) -> 
    {ok, MP3Bytes :: binary(), MetaDataBytes :: binary(), #parser_state{}} |
    {empty, #parser_state{}}.
parse(<<>>, State = #parser_state{buffer= <<>>}) ->
    {empty, State};
parse(Bytes, State = #parser_state{phase=reading_status, buffer=Buffer}) ->
    AllBytes = <<Buffer/binary, Bytes/binary>>,
    NewState = case erlang:decode_packet(line, AllBytes, []) of
                   {ok, <<"ICY 200 OK\r\n">>, Rest} -> 
                       State#parser_state{phase=reading_headers, buffer=Rest};
                   {more, _Length} -> 
                       State#parser_state{buffer=AllBytes}
               end,
    {<<>>, <<>>, NewState};
parse(Bytes, State = #parser_state{phase=reading_headers, buffer=Buffer, headers=Headers}) ->
    AllBytes = <<Buffer/binary, Bytes/binary>>,
    NewState = case erlang:decode_packet(httph, AllBytes, []) of
                   {ok, http_eoh, Rest} -> 
                       ChuckSize = case proplists:get_value("Icy-Metaint", Headers) of
                                       undefined ->
                                           undefined;
                                       Value ->
                                           list_to_integer(Value)
                                   end,
                       State#parser_state{phase=reading_stream, mp3_chunksize=ChuckSize, buffer=Rest};
                   {ok, {http_header, _, Name, _, Value}, Rest} -> 
                       Header = {Name, Value},
                       State#parser_state{buffer=Rest, headers=[Header|Headers]};
                   {more, _Length} -> 
                       State#parser_state{buffer=AllBytes}
               end,
    {<<>>, <<>>, NewState};
parse(Bytes, State = #parser_state{phase=reading_stream,
                                   mp3_chunksize=undefined}) ->
    % If mp3_chunksize is undefined, that means all bytes in the stream
    % are mp3 bytes
    {Bytes, <<>>, State};
parse(Bytes, State = #parser_state{phase=reading_stream,
                                   mp3_chunksize=ChunkSize,
                                   buffer=Buffer,
                                   mp3_chunkbytes=ChunkBytes}) ->
    {MP3Bytes, MetaBytes, Rest, BytesRead} =
        decode_stream(<<Buffer/binary, Bytes/binary>>,
                      ChunkSize, ChunkBytes),

    {MP3Bytes, MetaBytes, 
     State#parser_state{buffer=Rest,
                        mp3_chunkbytes=BytesRead}}.

% internal
-spec decode_stream(binary(), integer(), integer()) ->
    {MP3Bytes :: binary(), MetaBytes :: binary(), Rest :: binary(), BytesRead :: integer()}.
decode_stream(Bytes, ChunkSize, BytesRead) 
  when size(Bytes) + BytesRead =< ChunkSize ->
    {Bytes, <<>>, <<>>, size(Bytes) + BytesRead};
decode_stream(Bytes, ChunkSize, BytesRead) ->
    MP3Size = ChunkSize - BytesRead,
    <<MP3Bytes:MP3Size/bytes, MetaBytes/binary>> = Bytes,
    case decode_meta(MetaBytes) of
        {MetaData, Rest} ->
            {MP3Bytes, MetaData, Rest, 0};
        more ->
            {MP3Bytes, <<>>, MetaBytes, ChunkSize}
    end.

decode_meta(<<MetaLenDiv16:8, MetaBytes/binary>>) ->
    MetaLen = MetaLenDiv16 * 16,
    case MetaBytes of
        <<MetaData:MetaLen/bytes, Rest/binary>> ->
            {MetaData, Rest};
        _ ->
            more
    end.
    


    
    
-ifdef(TEST).
decode_meta_test_() ->
    [
     ?_assertEqual(more,
                   decode_meta(<<1, "abc">>)),

     ?_assertEqual({<<"abc", 0:104>>, <<>>},
                   decode_meta(<<1, "abc", 0:104>>)),

     ?_assertEqual({<<"abc", 0:104>>, <<"def">>},
                   decode_meta(<<1, "abc", 0:104, "def">>))
     ].

decode_stream_test_() ->
    [
     ?_assertEqual({<<"abc">>, <<>>, <<>>, 3},
                   decode_stream(<<"abc">>, 10, 0)),
     ?_assertEqual({<<"abc">>, <<>>, <<>>, 10},
                   decode_stream(<<"abc">>, 10, 7))
     ].

parse_test_() ->
    [
     % reading status
     ?_assertEqual({<<>>, <<>>,
                    #parser_state{phase=reading_headers, buffer= <<>>}},
                   parse(<<"ICY 200 OK\r\n">>, #parser_state{})),
     ?_assertEqual({<<>>, <<>>,
                    #parser_state{phase=reading_headers, buffer= <<"icy-">>}},
                   parse(<<"ICY 200 OK\r\nicy-">>, #parser_state{})),
     ?_assertEqual({<<>>, <<>>,
                   #parser_state{phase=reading_headers, buffer= <<>>}},
                   parse(<<" 200 OK\r\n">>, #parser_state{buffer= <<"ICY">>})),

     % reading headers
     ?_assertEqual({<<>>, <<>>,
                    #parser_state{phase=reading_headers,
                                  buffer= <<"icy-metaint: 1">>}},
                   parse(<<"icy-metaint: 1">>,
                         #parser_state{phase=reading_headers})),

     ?_assertEqual({<<>>, <<>>,
                    #parser_state{phase=reading_headers,
                                  buffer= <<"icy-metaint: 1\r\n">>}},
                   parse(<<"icy-metaint: 1\r\n">>,
                         #parser_state{phase=reading_headers})),

     ?_assertEqual({<<>>, <<>>,
                    #parser_state{phase=reading_headers,
                                  headers=[{"Icy-Metaint", "1"}],
                                  buffer= <<"\r\n">>}},
                   parse(<<"icy-metaint: 1\r\n\r\n">>,
                         #parser_state{phase=reading_headers})),
     
     ?_assertEqual({<<>>, <<>>,
                    #parser_state{phase=reading_stream, 
                                  mp3_chunksize=undefined,
                                  buffer= <<>>}},
                   parse(<<"\r\n">>,
                         #parser_state{phase=reading_headers})),

     ?_assertEqual({<<>>, <<>>,
                    #parser_state{phase=reading_stream,
                                  mp3_chunksize=8196,
                                  headers=[{"Icy-Metaint", "8196"}],
                                  buffer= <<>>}},
                   parse(<<"\r\n">>,
                         #parser_state{phase=reading_headers,
                                       headers=[{"Icy-Metaint", "8196"}]})),

     
     ?_assertEqual({<<>>, <<>>,
                    #parser_state{phase=reading_stream, buffer= <<"a">>}},
                   parse(<<"\r\na">>,
                         #parser_state{phase=reading_headers})),

     % reading stream
     % no metaint
     ?_assertEqual({<<"abc">>, <<>>,
                    #parser_state{phase=reading_stream}},
                   parse(<<"abc">>,
                         #parser_state{phase=reading_stream})),

     % meta chunk not reached
     ?_assertEqual({<<"abc">>, <<>>,
                    #parser_state{phase=reading_stream,
                                  mp3_chunkbytes=3,
                                  mp3_chunksize=10}},
                   parse(<<"abc">>,
                         #parser_state{phase=reading_stream,
                                      mp3_chunksize=10})),

     % meta chunk reached but not full
     ?_assertEqual({<<"abc">>, <<>>,
                    #parser_state{phase=reading_stream,
                                  buffer= <<1, "buf">>,
                                  mp3_chunkbytes=10,
                                  mp3_chunksize=10}},
                   parse(<<"abc", 1, "buf">>,
                         #parser_state{phase=reading_stream,
                                       mp3_chunkbytes=7,
                                       mp3_chunksize=10})),
     % meta chunk complete
     ?_assertEqual({<<"abc">>, <<"buf", 0:104>>,
                    #parser_state{phase=reading_stream,
                                  buffer= <<>>,
                                  mp3_chunkbytes=0,
                                  mp3_chunksize=10}},
                   parse(<<"abc", 1, "buf", 0:104>>,
                         #parser_state{phase=reading_stream,
                                       mp3_chunkbytes=7,
                                       mp3_chunksize=10})),

     % meta chunk complete buffered
     ?_assertEqual({<<>>, <<"buf", 0:104>>,
                    #parser_state{phase=reading_stream,
                                  buffer= <<>>,
                                  mp3_chunkbytes=0,
                                  mp3_chunksize=10}},
                   parse(<<0:104>>,
                         #parser_state{phase=reading_stream,
                                       buffer= <<1, "buf">>,
                                       mp3_chunkbytes=10,
                                       mp3_chunksize=10})),
     ?_assertEqual({empty, #parser_state{buffer= <<>>}},
                   parse(<<>>, #parser_state{buffer= <<>>}))
     ].
    

-endif.
