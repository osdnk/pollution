-module(pollution_server_sup).
-export([start/0, init/0]).

start() ->
  %% io:format("pollution_server_sup:start()~n"),
  spawn_link(?MODULE, init, []).

init() ->
  %% io:format("pollution_server_sup:init()~n"),
  process_flag(trap_exit, true),
  loop().

loop() ->
  %% io:format("pollution_server_sup:loop()~n"),
  pollution_server:start_link(),
  receive
    {'EXIT', _Pid, _Reason} ->
      %% io:format("Restarting pollution server~n"),
      loop()
  end.
