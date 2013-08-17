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
         ,join/1
        ]).

-type scheme() :: atom().

-type opt() :: {scheme_defaults, list()} |
               uri_format:opt().
-type opts() :: [opt()].

-type parsed_uri() ::
        {Scheme::scheme(),
         Auth::iolist(),
         Host::iolist(),
         inet:port_number(),
         Path::iolist(),
         Query::iolist()}.

-export_type([ scheme/0,
               parsed_uri/0,
               opts/0
             ]).

parse(Uri) ->
    uri_parser:parse(Uri).

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

-spec join(Uri :: [string()]) -> string().
join(Uri) ->
    %% remove empty strings
    Uri1 = lists:filter(fun(V) ->
                                if V == "" -> false;
                                   true -> true
                                end
                        end, Uri),
    join(Uri1, []).

join([], Acc) ->
    lists:concat(Acc);
join([H|T], Acc) ->
    join(T, Acc ++ [H] ++ ["/"]).
