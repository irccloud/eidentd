% ident server, as per http://tools.ietf.org/html/rfc1413
-module(eidentd_responder).
-export([loop/1]).
-define(TIMEOUT, 10000). % if no query in this time, close socket

get_peername(Socket) ->
    {ok, {RemoteAddress, _ThisRemPort}} = inet:peername(Socket),
    conv(RemoteAddress).

%% Convert IPv4 address masquerading as IPv6 ones:
conv({0,0,0,0,0,65535,AB,CD}) ->
    {AB bsr 8,
     AB band 2#0000000011111111,
     CD bsr 8,
     CD band 2#0000000011111111};
conv(IP) ->
    IP.

line_to_ports(Line) ->
    case string:tokens(Line, ",") of
        [OurPortS0, TheirPortS0] ->
            OurPortS = re:replace(OurPortS0, "\\s+", "", [global, {return, list}]),
            TheirPortS = re:replace(TheirPortS0, "\\s+", "", [global, {return, list}]),
            {ok, {OurPortS, TheirPortS}};
        _ ->
            {error, invalid}
    end.     

loop(Socket) ->
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Line} ->
            RemoteAddress = get_peername(Socket),
            case line_to_ports(Line) of
                {ok, {OurPortS, TheirPortS}} ->
                    {OurPort, _} = string:to_integer(OurPortS),
                    {TheirPort, _} = string:to_integer(TheirPortS),
                    case {validport(OurPort), validport(TheirPort)} of
                        {true, true} ->
                            %% Sometimes, ircds do an ident request before we have registered it
                            %% so a small delay before checking is added:
                            timer:sleep(1000),
                            case eidentd_registry:ident(RemoteAddress, TheirPort, OurPort) of
                                {ok, {_Pid, UserId}} ->
                                    R = format_response(OurPort, TheirPort, UserId),
                                    gen_tcp:send(Socket, R),
                                    loop(Socket);
                                
                                undefined ->
                                    R = format_error(OurPortS, TheirPortS, "NO-USER"),
                                    gen_tcp:send(Socket, R),
                                    ok
                            end;
                        
                        _ -> 
                            R = format_error(OurPortS, TheirPortS, "INVALID-PORT"),
                            gen_tcp:send(Socket, R),
                            ok
                    end;
                _ ->
                    % Invalid request format, just close
                    ok
            end;
        {error, timeout} ->
            ok;
        
        {error, closed} ->
            ok
    end.
    
validport(P) when is_integer(P), P > 0, P < 65536 -> true;
validport(_) -> false.
    

format_response(OurPort, TheirPort, UserId) when is_list(UserId), 
                                                 is_integer(OurPort), 
                                                 is_integer(TheirPort) ->
    io_lib:format("~B, ~B : USERID : LINUX : ~s\r\n", 
                  [OurPort, TheirPort, UserId]).


format_error(OurPortS, TheirPortS, Reason) when is_list(OurPortS),
                                              is_list(TheirPortS),
                                              is_list(Reason) ->
    io_lib:format("~s, ~s : ERROR : ~s\r\n", [OurPortS, TheirPortS, Reason]).
