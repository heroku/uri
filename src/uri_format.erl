%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc URI Formatting functions
%% @end
-module(uri_format).

-export([to_iolist/2]).

-spec to_iolist(uri:parsed_uri(), uri:opts()) -> iolist().
to_iolist({Scheme, UserInfo, Host, Port, Path, Query}, Opts) ->
    [ scheme_to_iolist(Scheme),
      "://",
      user_info_to_iolist(UserInfo),
      Host,
      port_info_to_iolist(Scheme, Port, Opts),
      Path,
      Query
    ].

scheme_to_iolist(Scheme) when is_atom(Scheme) ->
    atom_to_binary(Scheme, latin1).

user_info_to_iolist("") ->
    "";
user_info_to_iolist(IoList) ->
    [ IoList, "@" ].

port_info_to_iolist(Scheme, Port, Opts) ->
    Schemes = proplists:get_value(scheme_defaults, Opts,
                                  uri_defaults:scheme_defaults()),
    case uri_defaults:is_default_port(Scheme, Port, Schemes) of
          default -> "";
          non_default -> [":", integer_to_list(Port)]
    end.
