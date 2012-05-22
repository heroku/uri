%% @copyright Heroku 2012
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc URI Handling Library.
%% @end
-module(uri).

-export([parse/1
         ,parse/2
         ,format/1
        ]).

-type scheme() :: atom().

-type parsed_uri() ::
        {Scheme::scheme(),
         Auth::iolist(),
         Host::iolist(),
         inet:port_number(),
         Path::iolist(),
         Query::iolist()}.

-export_type([ scheme/0,
               parsed_uri/0
             ]).

parse(Uri) ->
    uri_parser:parse(Uri).

parse(Uri, Opts) ->
    uri_parser:parse(Uri, Opts).

format(Uri) when is_tuple(Uri) ->
    uri_format:to_iolist(Uri).
