%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc URI Formatting functions
%% @end
-module(uri_format).

-export([to_iolist/2
         ,full_host/2
        ]).

-include("uri.hrl").

-type opt() :: {hide_user_info, boolean()}.
-export_type([opt/0]).

-spec to_iolist(uri:parsed_uri(), uri:opts()) -> iolist().
to_iolist(#uri{scheme=Scheme,
               userauth=UserInfo,
               path=Path,
               q=Query,
               frag=Frag} = Uri, Opts) ->
    [ scheme_to_iolist(Scheme),
      <<"://">>,
      user_info_to_iolist(UserInfo, Opts),
      full_host(Uri, Opts),
      path(Path),
      querystring(Query),
      frag(Frag)
    ].

schemes(Opts) ->
    proplists:get_value(scheme_defaults, Opts,
                        uri_defaults:scheme_defaults()).

scheme_to_iolist(Scheme) when is_atom(Scheme) ->
    atom_to_binary(Scheme, latin1).

user_info_to_iolist("", _) -> "";
user_info_to_iolist(UserInfo, Opts) ->
    case proplists:get_value(hide_user_info, Opts, false) of
        true ->
            <<"xxx:yyy@">>;
        false ->
            [UserInfo, $@]
    end.

port_info_to_iolist(Scheme, Port, Schemes) ->
    case uri_defaults:is_default_port(Scheme, Port, Schemes) of
          default -> "";
          non_default -> [":", integer_to_list(Port)]
    end.

-spec full_host(uri:parsed_uri(), uri:opts()) -> iolist().
full_host(#uri{scheme=Scheme, host=Host, port=Port}, Opts) ->
    [Host, port_info_to_iolist(Scheme, Port, schemes(Opts))].

%% If defined, Path starts with /
path(undefined) -> [];
path(Path) -> Path.

%% If defined, querystring starts with ?
querystring(undefined) -> [];
querystring(Qstr) -> Qstr.

%% If defined, frag starts with #
frag(undefined) -> [];
frag(Frag) -> Frag.
