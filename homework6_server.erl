-module(homework6_server).
-behaviour(gen_server).

-export([start_link/0, create/1, insert/3, insert/4, lookup/2]).
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3]).

%% API
start_link() ->
    gen_server:start_link({local, homework6_server}, homework6_server, [], []).

create(TableName) ->
    gen_server:call(homework6_server, {create, TableName}).

insert(TableName, Key, Value) ->
    gen_server:call(homework6_server, {insert, TableName, Key, Value}).

insert(TableName, Key, Value, TTL) ->
    gen_server:call(homework6_server, {insert, TableName, Key, Value, TTL}).

lookup(TableName, Key) ->
    gen_server:call(homework6_server, {lookup, TableName, Key}).

%% Callbacks
init([]) ->
    %% Ініціалізація кешу
    {ok, #{} }.

handle_call({create, TableName}, _From, State) ->
    ets:new(TableName, [named_table, public, set]),
    {reply, ok, State};

handle_call({insert, TableName, Key, Value}, _From, State) ->
    Timestamp = infinity,
    ets:insert(TableName, {Key, Value, Timestamp}),
    {reply, ok, State};

handle_call({insert, TableName, Key, Value, TTL}, _From, State) ->
    {_, CurrentTime} = calendar:local_time(),
    Expiration = CurrentTime + TTL,
    ets:insert(TableName, {Key, Value, Expiration}),
    {reply, ok, State};

handle_call({lookup, TableName, Key}, _From, State) ->
    case ets:lookup(TableName, Key) of
        [] -> {reply, undefined, State};
        [{_, Value, infinity}] -> {reply, Value, State};
        [{_, Value, Expiration}] ->
            {_, CurrentTime} = calendar:local_time(),
            if
                Expiration > CurrentTime -> {reply, Value, State};
                true -> {reply, undefined, State}
            end
    end.

handle_info(cleanup, State) ->
    %% Видалення застарілих даних
    {_, CurrentTime} = calendar:local_time(),
    Tables = ets:all(),
    lists:foreach(
        fun(TableName) ->
            ObsoleteKeys =
                [Key || {Key, _, Expiration} <- ets:tab2list(TableName),
                 Expiration =/= infinity, Expiration =< CurrentTime],
            lists:foreach(fun(Key) -> ets:delete(TableName, Key) end, ObsoleteKeys)
        end,
        Tables
    ),
    %% Планування наступного очищення через 60 секунд
    erlang:send_after(60000, self(), cleanup),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

%% Необхідні функції для gen_server
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
