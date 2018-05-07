-module(pollution_server).
-export([start/0, stop/0]).

start() ->
  spawn(pollution_server, init, []).

init() ->
  Monitor = pollution:create_monitor(),
  loop(Monitor).

loop(Monitor) ->
  receive
    {add_station, Name, {_, _} = Location} ->
      loop(pollution:add_station(Monitor, Name, Location));
    {add_value, {_, _} = Location, Time, MeasurementType, Value} ->
      loop(pollution:add_value(Monitor, Location, Time, MeasurementType, Value));
    {remove_value, {_, _} = Location, Time, MeasurementType} ->
      loop(pollution:remove_value(Monitor, Location, Time, MeasurementType));
    {get_one_value, MeasurementType, Time, Station} ->
      loop(pollution:get_one_value(MeasurementType, Time, Station));
    {get_station_mean, MeasurementType, Station} ->
      loop(pollution:get_station_mean(MeasurementType, Station));
    {get_daily_mean, MeasurementType, Day} ->
      loop(pollution:get_daily_mean(Monitor, MeasurementType, Day));
    {get_daily_average_data_count} ->
      loop(pollution:get_daily_average_data_count(Monitor));
    stop ->
      terminate()
  end.

terminate() ->
  ok.

stop() ->
  self ! stop.
