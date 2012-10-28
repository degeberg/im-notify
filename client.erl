-module(client).

-behaviour(gen_server).
-export([start/0, stop/0, notify/3]).

%% gen_servercallbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(server).
-import(notify). % https://github.com/msantos/erlang-notify-osd

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop).

notify(CPid, Who, Msg) -> gen_server:cast(CPid, {msg, Who, Msg}).

init([]) ->
    server:subscribe(),
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast({msg, Who, Msg}, State) ->
    notify:osd([
            {summary,  io_lib:format("~s says:", [Who])},
            {body,     Msg},
            {icon,     "notification-message-im"},
            {category, "im.received"}
        ]),
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
