%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc New uri parser
%% @end

%% This is from chapter 3, Syntax Components, of RFC 3986:
%%
%% The generic URI syntax consists of a hierarchical sequence of
%% components referred to as the scheme, authority, path, query, and
%% fragment.
%%
%%    URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
%%
%%    hier-part   = "//" authority path-abempty
%%                   / path-absolute
%%                   / path-rootless
%%                   / path-empty
%%
%%    The scheme and path components are required, though the path may be
%%    empty (no characters).  When authority is present, the path must
%%    either be empty or begin with a slash ("/") character.  When
%%    authority is not present, the path cannot begin with two slash
%%    characters ("//").  These restrictions result in five different ABNF
%%    rules for a path (Section 3.3), only one of which will match any
%%    given URI reference.
%%
%%    The following are two example URIs and their component parts:
%%
%%          foo://example.com:8042/over/there?name=ferret#nose
%%          \_/   \______________/\_________/ \_________/ \__/
%%           |           |            |            |        |
%%        scheme     authority       path        query   fragment
%%           |   _____________________|__
%%          / \ /                        \
%%          urn:example:animal:ferret:nose
%%
%%    scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
%%    authority   = [ userinfo "@" ] host [ ":" port ]
%%    userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
%%
%%

-module(uri_parser).

-include("uri.hrl").

-export([parse/2]).

parse(List, Options) when is_list(List), is_list(Options) ->
    parse_scheme(iolist_to_binary(List), Options, #uri{});
parse(Str, Options) when is_binary(Str), is_list(Options) ->
    parse_scheme(Str, Options, #uri{}).

parse_scheme(Str, Options, #uri{scheme=undefined} = URI) ->
    case binary:split(Str, <<"://">>) of
        [Scheme, Rest] ->
            parse_authority(Rest, Options, decode_scheme(Scheme, Options, URI));
        [_] ->
            erlang:error(no_scheme)
            %% parse_authority(Str, Options, URI#uri{scheme=no_scheme})
    end.

parse_authority(Str, Options, URI = #uri{}) ->
    case binary:split(Str, <<"/">>) of
        [Authority, Rest] ->
            parse_path(Rest, Options,
                       decode_authority(Authority, Options, URI));
        [_] ->
            case binary:split(Str, <<"?">>) of
                [Authority, Rest] ->
                    parse_query(Rest, Options,
                                decode_authority(Authority, Options, URI));
                [_] ->
                    case binary:split(Str, <<"#">>) of
                        [Authority, Frag] ->
                            decode_frag(Frag, Options,
                                        decode_authority(Authority, Options, URI));
                        [Authority] ->
                            decode_authority(Authority, Options, URI)
                    end
            end
    end.

parse_path(Str, Options, URI) ->
    case binary:split(Str, <<"?">>) of
        [Path, Rest] ->
            parse_query(Rest, Options,
                        decode_path(Path, Options, URI));
        [_] ->
            case binary:split(Str, <<"#">>) of
                [Path, Frag] ->
                    decode_frag(Frag, Options,
                                decode_path(Path, Options, URI));
                [Path] ->
                    decode_path(Path, Options, URI)
            end
    end.

parse_query(Str, Options, URI) ->
    case binary:split(Str, <<"#">>) of
        [Query, Frag] ->
            decode_frag(Frag, Options,
                        decode_query(Query, Options, URI));
        [Query] ->
            decode_query(Query, Options, URI)
    end.

decode_scheme(Str, _Options, Uri) ->
    try binary_to_existing_atom(Str, latin1) of
        Scheme ->
            Uri#uri{scheme=Scheme}
    catch
        error:badarg ->
            Uri#uri{scheme=Str}
    end.

decode_authority(Str, Options, Uri) ->
    case binary:split(Str, <<"@">>) of
        [AuthInfo, Rest] ->
            parse_hostport(Rest, Options,
                           decode_userauth(AuthInfo, Options, Uri));
        [Rest] ->
            parse_hostport(Rest, Options, Uri)
    end.

parse_hostport(Str, Options, Uri) ->
    case re:run(Str, "^(.*):(\\d+)$", [{capture, all_but_first, binary}]) of
        {match, [Host, Port]} ->
            Uri#uri{host=decode_host(Host, Options),
                    port=decode_port(Port, Options)};
        nomatch ->
            default_port(Uri#uri{host=decode_host(Str, Options)}, Options)
    end.

decode_host(Host, Options) ->
    case proplists:get_value(parse_ip_addresses, Options, false) of
        false ->
            Host;
        true ->
            decode_ip_address(Host, Options)
    end.

decode_port(Port, _Options) ->
    list_to_integer(binary_to_list(Port)).

decode_userauth(UserAuth, _Options, Uri = #uri{}) ->
    Uri#uri{userauth=UserAuth}.

decode_path(Path, _Options, Uri = #uri{}) ->
    Uri#uri{path = <<$/, Path/binary>>}.

decode_query(Query, _Options, Uri = #uri{}) ->
    Uri#uri{q = <<$?, Query/binary>>}.

decode_frag(Frag, _Options, Uri = #uri{}) ->
    Uri#uri{frag = <<$#, Frag/binary>>}.

%% parse_test() ->
%%     URI = <<"redis://fccab7a749daf4dda5f33d44@10.2.67.96:6379/?foo#blah">>,
%%     ?assertMatch(,parse_scheme(URI, [], empty())

decode_ip_address(Host, _Options) ->
    case re:run(Host, "^\\[(.*)\\]", [{capture, all_but_first, binary}]) of
        {match, [V6Addr]} ->
            {ok, Addr} = inet_parse:ipv6_address(binary_to_list(V6Addr)),
            Addr;
        nomatch ->
            case inet_parse:ipv4_address(binary_to_list(Host)) of
                {ok, Addr} ->
                    Addr;
                {error, _} ->
                    Host
            end
    end.

default_port(Uri = #uri{scheme = Scheme, port = default}, Options) ->
    SchemeDefaults = proplists:get_value(scheme_defaults, Options,
                                         uri_defaults:scheme_defaults()),
    Uri#uri{port = uri_defaults:default_port(Scheme, SchemeDefaults)}.
