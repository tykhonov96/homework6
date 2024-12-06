-module(homework6_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% API
start_link() ->
    supervisor:start_link({local, homework6_sup}, homework6_sup, []).

%% Callback для ініціалізації супервізора
init([]) ->
    % Стратегію 'one_for_one' можна вказати так:
    % Один процес може бути перезапущений, якщо інший зазнає збою.
    {ok, {{one_for_one, 5, 10}, [
        % тут вказуємо дії для запуски вашого процесу з кешем
        {homework6_server, {homework6_server, start_link, []}, permanent, 5000, worker, [homework6_server]}
    ]}}.
