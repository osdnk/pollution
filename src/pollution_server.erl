-module(pollution_server).
-export([
  start/0,
  stop/0,
  add_station/2,
  add_value/4,
  remove_value/3,
  get_one_value/3,
  get_station_mean/2,
  get_daily_mean/2,
  get_daily_average_data_count/0
]).

start() ->
  spawn(pollution_server, init, []).

init() ->
  Monitor = pollution:create_monitor(),
  loop(Monitor).

%% API start

add_station(Name, {_, _} = Location) ->
  handler(add_station, [Name, Location]).

add_value({_, _} = Location, Time, MeasurementType, Value) ->
  handler(add_value, [Location, Time, MeasurementType, Value]).

remove_value({_, _} = Location, Time, MeasurementType) ->
  handler(remove_value, [Location, Time, MeasurementType]).

get_one_value(MeasurementType, Time, Station) ->
  handler(get_one_value, [MeasurementType, Time, Station]).

get_station_mean(MeasurementType, Station) ->
  handler(get_station_mean, [MeasurementType, Station]).

get_daily_mean(MeasurementType, Day) ->
  handler(get_daily_mean, [MeasurementType, Day]).

get_daily_average_data_count() ->
  handler(get_daily_average_data_count, []).

%% API end

handler(RequestType, Args) when is_list(Args) ->
  server ! {RequestType, self(), Args},
  receive
    Message -> Message
  after
    1000 -> {error, timeout}
  end.

response(Pid, Monitor) ->
  Pid ! {ok, Monitor}.

loop(Monitor) ->
  receive
    {add_station, Pid, [Name, {_, _} = Location]} ->
      NewMonitor = pollution:add_station(Monitor, Name, Location),
      response(Pid, NewMonitor),
      loop(NewMonitor);

    {add_value, Pid, [{_, _} = Location, Time, MeasurementType, Value]} ->
      NewMonitor = pollution:add_value(Monitor, Location, Time, MeasurementType, Value),
      response(Pid, NewMonitor),
      loop(NewMonitor);

    {remove_value, Pid, [{_, _} = Location, Time, MeasurementType]} ->
      NewMonitor = pollution:remove_value(Monitor, Location, Time, MeasurementType),
      response(Pid, NewMonitor),
      loop(NewMonitor);

    {get_one_value, Pid, [MeasurementType, Time, Station]} ->
      NewMonitor = pollution:get_one_value(MeasurementType, Time, Station),
      response(Pid, NewMonitor),
      loop(NewMonitor);

    {get_station_mean, Pid, [MeasurementType, Station]} ->
      NewMonitor = pollution:get_station_mean(MeasurementType, Station),
      response(Pid, NewMonitor),
      loop(NewMonitor);

    {get_daily_mean, Pid, [MeasurementType, Day]} ->
      NewMonitor = pollution:get_daily_mean(Monitor, MeasurementType, Day),
      response(Pid, NewMonitor),
      loop(NewMonitor);

    {get_daily_average_data_count, Pid, []} ->
      NewMonitor = pollution:get_daily_average_data_count(Monitor),
      response(Pid, NewMonitor),
      loop(NewMonitor);

    {stop, Pid, []} ->
      response(Pid, Monitor),
      terminate()
  end.

terminate() ->
  ok.

stop() ->
  handler(stop, []).
