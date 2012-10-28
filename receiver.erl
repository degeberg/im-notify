-module(receiver).

-behaviour(gen_server).
-export([start/1, stop/0]).

%% gen_servercallbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(server).

-define(SOCK(Msg), {tcp, _Port, Msg}).
-record(state, {lsocket, socket}).

start(Port) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).
stop()      -> gen_server:call({local, ?MODULE}, stop).

init(Port) ->
    case gen_tcp:listen(Port, [{active, false}, {ip, {127,0,0,1}}, {packet, 0}]) of
        {ok, ListenSocket} ->
            gen_server:cast(self(), accept),
            {ok, #state{lsocket=ListenSocket}};
        {error, Reason} ->
            {error, Reason}
    end.

handle_call(stop, _From, S = #state{lsocket=ListenSocket}) ->
    gen_tcp:close(ListenSocket),
    {stop, normal, stopped, S}.

handle_cast(accept, S = #state{lsocket=ListenSocket}) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    inet:setopts(Socket, [{active, once}]),
    {noreply, S#state{socket=Socket}}.

handle_info(?SOCK(Str), S) ->
    [Who, Msg | _X] = string:tokens(Str, "\n"),
    server:notify(Who, Msg),
    gen_server:cast(self(), accept),
    {noreply, S};
handle_info({tcp_closed, _Socket}, S) ->
    {stop, normal, S};
handle_info({tcp_error, _Socket}, S) ->
    {stop, normal, S}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
