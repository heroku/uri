-module(uri_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include("uri.hrl").

parse_test_() ->
    [ ?_assertMatch(#uri{scheme=http,
                         host = <<"[2010:836B:4179::836B:4179]">>, port=80, path = <<"/foobar.html">>},
                    uri:parse(<<"http://[2010:836B:4179::836B:4179]/foobar.html">>, [{foo, false}])),
      %% ?_assertMatch(#uri{scheme=http,
      %%                    host = <<"2010:836B:4179::836B:4179">>,port=80,path = <<"/foobar.html">>},
      %%               uri:parse(<<"http://[2010:836B:4179::836B:4179]/foobar.html">>,
      %%                                 [{ipv6_host_with_brackets, false}])),
      %% ?_assertError(malformed_url, uri:parse(<<"http://2010:836B:4179::836B:4179/foobar.html">>, [])),
      %% ipv4
      ?_assertMatch(#uri{scheme=http,host = <<"127.0.0.1">>,port=80,path = <<"/foobar.html">>},
                    uri:parse(<<"http://127.0.0.1/foobar.html">>, [])),
      %% host
      ?_assertMatch(#uri{scheme=http,host = <<"localhost">>,port=8888,path = <<"/foobar.html">>},
                    uri:parse(<<"http://localhost:8888/foobar.html">>, [])),
      %% Userinfo
      ?_assertMatch(#uri{scheme=http,userauth = <<"nisse:foobar">>,
                         host = <<"localhost">>,port=8888,path = <<"/foobar.html">>},
                    uri:parse(<<"http://nisse:foobar@localhost:8888/foobar.html">>, [])),
      %% Scheme error
      ?_assertError(no_scheme,
                    uri:parse(<<"localhost/foobar.html">>, [])),
      ?_assertError(no_scheme,
                    uri:parse(<<"localhost:8888/foobar.html">>, [])),

      %% Query
      ?_assertMatch(#uri{scheme=http,host = <<"localhost">>,port=8888,path = <<"/foobar.html">>,q = <<"?foo=bar&foobar=42">>},
                    uri:parse(<<"http://localhost:8888/foobar.html?foo=bar&foobar=42">>, [])),
      %%  Esc chars
      ?_assertMatch(#uri{scheme=http,host = <<"www.somedomain.com">>,port=80,path = <<"/%2Eabc">>},
                    uri:parse(<<"http://www.somedomain.com/%2Eabc">>, [])),

      ?_assertMatch(#uri{scheme=http,host = <<"www.somedomain.com">>,port=80,path = <<"/%252Eabc">>},
                    uri:parse(<<"http://www.somedomain.com/%252Eabc">>, [])),
      ?_assertMatch(#uri{scheme=http,host = <<"www.somedomain.com">>,port=80,path = <<"/%25abc">>},
                    uri:parse(<<"http://www.somedomain.com/%25abc">>, [])),
      ?_assertMatch(#uri{scheme=http,host = <<"www.somedomain.com">>,port=80,path = <<"/%25abc">>, q = <<"?foo=bar">>},
                    uri:parse(<<"http://www.somedomain.com/%25abc?foo=bar">>, []))
      ].
