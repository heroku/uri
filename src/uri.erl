%% @copyright Heroku 2012
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc URI Handling Library.
%% @end
-module(uri).

-export([parse/1
         ,parse/2
        ]).

parse(Uri) ->
    uri_parser:parse(Uri).

parse(Uri, Opts) ->
    uri_parser:parse(Uri, Opts).
