%% @copyright Heroku 2012
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc URI Handling Library.
%% @end
-module(uri).

-export([parse/1
         ,parse/2
         ,to_iolist/1
         ,to_iolist/2
         ,to_binary/1
         ,to_binary/2
         ,to_string/1
         ,to_string/2
         ,full_host_iolist/2
         ,host_port/1
        ]).

-include("uri.hrl").

-type scheme() :: atom().
-type host() :: binary() | iolist().
-type userauth() :: binary() | iolist().
-type path() :: binary() | iolist().
-type querystring() :: binary() | iolist().
-type frag() :: binary() | iolist().

-type opt() :: {scheme_defaults, uri_defaults:scheme_defaults()} |
               uri_format:opt().
-type opts() :: [opt()].

-type parsed_uri() :: #uri{}.

-export_type([ scheme/0,
               host/0,
               userauth/0,
               path/0,
               querystring/0,
               frag/0,
               parsed_uri/0,
               opts/0
             ]).

parse(Uri) ->
    uri_parser:parse(Uri, []).

parse(Uri, Opts) ->
    uri_parser:parse(Uri, Opts).

to_iolist(Uri) when is_tuple(Uri) ->
    to_iolist(Uri, []).

to_binary(Uri) when is_tuple(Uri) ->
    to_binary(Uri, []).

to_string(Uri) when is_tuple(Uri) ->
    to_string(Uri, []).

to_iolist(Uri, Opts) when is_tuple(Uri), is_list(Opts) ->
    uri_format:to_iolist(Uri, Opts).

to_binary(Uri, Opts) when is_tuple(Uri), is_list(Opts) ->
    iolist_to_binary(uri_format:to_iolist(Uri, Opts)).

to_string(Uri, Opts) when is_tuple(Uri), is_list(Opts) ->
    binary_to_list(to_binary(Uri, Opts)).

full_host_iolist(Uri, Opts) when is_tuple(Uri), is_list(Opts) ->
    uri_format:full_host(Uri, Opts).

host_port({_, _, Host, Port, _, _}) ->
    {Host, Port}.

