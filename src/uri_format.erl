%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc URI Formatting functions
%% @end
-module(uri_format).

-export([to_iolist/1
         ,to_binary/1
         ,to_string/1]).

-spec to_iolist(uri:parsed_uri()) -> iolist().
to_iolist({Scheme, UserInfo, Host, Port, Path, Query}) ->
    case uri_defaults:is_default_port(Scheme, Port) of
        default ->
            [ scheme_to_iolist(Scheme),
              "://",
              user_info_to_iolist(UserInfo),
              Host,
              Path,
              Query
            ];
        non_default ->
            [ scheme_to_iolist(Scheme),
              "://",
              user_info_to_iolist(UserInfo),
              Host,
              ":", integer_to_list(Port),
              Path,
              Query
            ]
    end.

-spec to_string(uri:parsed_uri()) -> [char()].
to_string(ParsedUri) ->
    binary_to_list(to_binary(ParsedUri)).

-spec to_binary(uri:parsed_uri()) -> binary().
to_binary(ParsedUri) ->
    iolist_to_binary(to_iolist(ParsedUri)).


scheme_to_iolist(Scheme) when is_atom(Scheme) ->
    atom_to_binary(Scheme, latin1).

user_info_to_iolist("") ->
    "";
user_info_to_iolist(IoList) ->
    [ IoList, "@" ].
