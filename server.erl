-module(server).

-behaviour(gen_server).
-export([start/0, stop/0, notify/2, subscribe/0]).

%% gen_servercallbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(client).
-import(receiver).

start() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
stop()  -> gen_server:call({global, ?MODULE}, stop).

notify(Who, Msg) -> gen_server:call({global, ?MODULE}, {msg, Who, Msg}).
subscribe()      -> gen_server:call({global, ?MODULE}, subscribe).

init([]) ->
    receiver:start(1337),
    {ok, []}.

handle_call({msg, Who, Msg}, _From, Pids) ->
    lists:foreach(fun(Pid) -> client:notify(Pid, Who, Msg) end, Pids),
    {reply, ok, Pids};
handle_call(subscribe, {Pid, _Tag}, Pids) ->
    io:format("Received subscription ~p~n", [Pid]),
    {reply, ok, [Pid | Pids]};
handle_call(stop, _From, Pids) ->
    receiver:stop(),
    {stop, normal, stopped, Pids}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
