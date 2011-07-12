-module(eidentd_server).
-behavior(gen_server).

-export([accept_loop/2, start_link/0]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TCP_OPTIONS, [list, 
                      {packet, line}, 
                      {active, false}, 
                      {reuseaddr, true} ]).

-record(state, { port,
                 ip=any,
                 lsocket=null }).

start_link() ->
    Port = get_opt(port, 20113),
    Ip   = get_opt(ip,   any),
    State = #state{port = Port, ip=Ip},
    error_logger:info_msg("eidentd listening on ~p:~w\n", [Ip,Port]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

init(State = #state{port=Port,ip=Ip}) ->
    case gen_tcp:listen(Port, [{ip, Ip} | ?TCP_OPTIONS]) of
        {ok, LSocket} ->
            NewState = State#state{lsocket = LSocket},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State=#state{}) ->
    {noreply, accept(State)}.

accept_loop(Server, LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            gen_server:cast(Server, {accepted, self()}),
            eidentd_responder:loop(Socket);
        _ ->
            stop
    end.
   
% To be more robust we should be using spawn_link and trapping exits
accept(State = #state{lsocket=LSocket}) ->
    proc_lib:spawn(?MODULE, accept_loop, [self(), LSocket]),
    State.

handle_call(_Msg, _From, State)     -> {noreply, State}.
handle_info(_Msg,  State)           -> {noreply, State}.
code_change(_Vsn, State, _Extra)    -> {ok, State}.
terminate(_Reason, _State)          -> ok.

%%
%%

get_opt(K, Def) ->
    case application:get_env(eidentd, K) of
        {ok, Val} -> Val;
        undefined -> Def
    end.
