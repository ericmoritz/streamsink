-module(streamsink_stream).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Url) ->
    gen_server:start_link(?MODULE, [Url], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-record(state, {url, parser, sock}).

init([Url]) ->
    {ok, #state{url=Url, parser=streamsink_parser:init()}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State=#state{url=Url}) ->
    {Host, Port, Request} = http_request(Url),
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, raw},
                                              {active, true}]),
    gen_tcp:send(Sock, Request),
    {noreply, State#state{sock=Sock}};
handle_info({tcp, _, Data}, State=#state{parser=Parser}) ->
    Parser2 = case streamsink_parser:parse(Data, Parser) of
                  {{data, _, []}, P} ->
                      P;
                  {{data, _, MetaData}, P} ->
                      error_logger:info_msg("~p~n", [MetaData]),
                      P;
                  {_, P} ->
                      P
              end,
    {noreply, State#state{parser=Parser2}};
handle_info({tcp_closed, _}, State) ->
    {stop, tcp_closed, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, {tcp_error, Reason}, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
http_request(Url) ->
    {"http", Netloc, Path, Query, _} = mochiweb_util:urlsplit(Url),
    PathAndQuery = case Query of
                       "" ->
                           Path;
                       _ ->
                           [Path, "?", Query]
                   end,

    {Host, Port} = host_split(Netloc),

    {Host, Port, 
     ["GET ", PathAndQuery, " HTTP/1.1\r\n",
      "Host: ", Netloc, "\r\n",
      "icy-metadata: 1\r\n\r\n"]}.


host_split(HostAndPort) ->
    case string:tokens(HostAndPort, ":") of
        [Host, PortString] ->
            {Host, list_to_integer(PortString)};
        [Host] ->
            {Host, 80}
    end.


-ifdef(TEST).
host_split_test_() ->
    [
     ?_assertEqual({"example.com", 80},
                   host_split("example.com")),
     
     ?_assertEqual({"example.com", 1337},
                   host_split("example.com:1337"))
     ].

http_request_test_() ->
    [
     ?_assertEqual({"example.com", 80,
                    ["GET ", "/", " HTTP/1.1\r\n",
                     "Host: ", "example.com", "\r\n",
                     "icy-metadata: 1\r\n\r\n"]},
                    http_request("http://example.com/")),

     ?_assertEqual({"example.com", 80,
                    ["GET ", "/", " HTTP/1.1\r\n",
                     "Host: ", "example.com", "\r\n",
                     "icy-metadata: 1\r\n\r\n"]},
                    http_request("http://example.com/")),

     ?_assertEqual({"example.com", 1337,
                    ["GET ", ["/","?", "foo=1"], " HTTP/1.1\r\n",
                     "Host: ", "example.com:1337", "\r\n",
                     "icy-metadata: 1\r\n\r\n"]},
                    http_request("http://example.com:1337/?foo=1"))
     ].
-endif.
