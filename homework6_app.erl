-module(homework6_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    homework6_sup:start_link().

stop(_State) ->
    ok.
