%% Keeps track of connections, to make identd work
-module(eidentd_registry).
-behaviour(gen_server).

-export([start_link/0, ident/3, set_ident/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {identdb}).

-define(SERVER, {global, ?MODULE}).

start_link() -> 
    gen_server:start_link(?SERVER, ?MODULE, [], []).

ident(RemoteIp, RemotePort, LocalPort) ->
    gen_server:call(?SERVER, {ident, RemoteIp, RemotePort, LocalPort}).

set_ident(Pid, Uid, RemoteIp, RemotePort, LocalPort) ->
    gen_server:call(?SERVER, {set_ident, Pid, Uid, RemoteIp, RemotePort, LocalPort}).

%% --------------------------------------------------------------------
init([]) ->
    Idb = ets:new(identdb, []),
    {ok, #state{identdb=Idb}}.

handle_call({set_ident, Pid, Uid, RemoteIp, RemotePort, LocalPort}, _From, State) ->
    erlang:monitor(process, Pid),
    Key = {RemoteIp, RemotePort, LocalPort},
    ets:insert(State#state.identdb, {Key, {Pid, Uid}}),
    ets:insert(State#state.identdb, {Pid, Key}),
    {reply, ok, State};

handle_call({ident, RemoteIp, RemotePort, LocalPort}, _From, State) ->
    Key = {RemoteIp, RemotePort, LocalPort},
    case ets:lookup(State#state.identdb, Key) of
        [{Key, {Pid, Uid}}] ->
            {_, NewState} = remove_by_pid(Pid, State),
            {reply, {ok, {Pid, Uid}}, NewState};
        _ ->
            {reply, undefined, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Info}, State) ->
    {_, NewState} = remove_by_pid(Pid, State),
    {noreply, NewState};

handle_info(_M, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

remove_by_pid(Pid, State) when is_pid(Pid) ->
    case ets:lookup(State#state.identdb, Pid) of
        [{Pid,Key}] ->
            ets:delete(State#state.identdb, Pid),
            ets:delete(State#state.identdb, Key),
            {ok, State};
        _X ->
            {not_found, State}
    end.

