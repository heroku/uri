%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc URI Scheme defaults
%% @end
-module(uri_defaults).

-export([scheme_defaults/0
         ,default_port/1
         ,default_port/2
         ,is_default_port/2
         ,is_default_port/3
        ]).

-type scheme_defaults() ::
    [{uri:scheme(), DefaultPort::inet:port_number()}].

-export_type([ scheme_defaults/0 ]).

-spec scheme_defaults() -> scheme_defaults().
scheme_defaults() ->
    [{http,  80}
     ,{https, 443}
     ,{ftp,   21}
     ,{ssh,   22}
     ,{sftp,  22}
     ,{tftp,  69}
     ,{syslog, 601}
    ].

default_port(Scheme) ->
    default_port(Scheme, scheme_defaults()).

default_port(Scheme, Schemes) ->
    case proplists:get_value(Scheme, Schemes) of
        Port when is_integer(Port) ->
            Port;
        undefined ->
            {error, {unknown_scheme, Scheme}}
    end.

is_default_port(Scheme, Port) ->
    is_default_port(Scheme, Port, scheme_defaults()).

is_default_port(Scheme, Port, Schemes) ->
    case default_port(Scheme, Schemes) of
        Port -> default;
        Other when is_integer(Other) -> non_default;
        Err -> Err
    end.
