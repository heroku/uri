%% @copyright Heroku 2012
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc URI Handling Library.
%% @end
-module(uri).

-export([parse/1
         ,parse/2
        ]).

-type scheme() :: atom().

-type parsed_uri() ::
        {Scheme::scheme(),
         Auth::iolist(),
         Host::iolist(),
         inet:port_number(),
         Path::iolist(),
         Query::iolist()}.

-export_types([ scheme/0,
                parsed_uri/0
              ]).

parse(Uri) ->
    uri_parser:parse(Uri).

parse(Uri, Opts) ->
    uri_parser:parse(Uri, Opts).
