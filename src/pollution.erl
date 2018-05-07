-module(pollution).
-export([
  create_monitor/0,
  add_station/3,
  add_value/5,
  remove_value/4,
  get_one_value/3,
  get_station_mean/2,
  get_daily_mean/3,
  get_daily_average_data_count/1
]).

-record(monitor, {
  locationToName = #{},
  stations = #{}}).
-record(station, {
  name = "",
  location,
  measurements = #{}}).
-record(measurement, {
  type,
  value,
  time}).

create_monitor() ->
  {ok, #monitor{}}.

add_station(Monitor, Name, {_, _} = Location) ->
  NameUsed = find_name(maps:find(Name, Monitor#monitor.stations)),
  LocationUsed = was_location_used(
    maps:find(Location, Monitor#monitor.locationToName)),
  add_station(not NameUsed and not LocationUsed, Monitor, Name, Location);
add_station(_, _, _) ->
  {error, "Wrong location format"}.
add_station(true, Monitor, Name, Location) ->
  {ok, #monitor{
    locationToName = maps:put(Location, Name, Monitor#monitor.locationToName),
    stations = maps:put(
      Name,
      #station{name = Name, location = Location},
      Monitor#monitor.stations)}};
add_station(false, _, _, _) ->
  {error, "Station already exists"}.

find_name({ok, _}) -> true;
find_name(error) -> false.

was_location_used({ok, _}) -> true;
was_location_used(error) -> false.

add_value(Monitor, {_, _} = Location, Time, MeasurementType, Value) when
  is_tuple(Time),
  is_atom(MeasurementType)
->
  add_value(Monitor, Location, Time, MeasurementType, Value,
    maps:find(Location, Monitor#monitor.locationToName));
add_value(Monitor, Name, Time, MeasurementType, Value) when
  is_tuple(Time),
  is_atom(MeasurementType)
->
  add_value(Monitor, Name, Time, MeasurementType, Value, {ok, Name}).

add_value(Monitor, _, Time, MeasurementType, Value, {ok, StationName}) ->
  {ok, Station} = maps:find(StationName, Monitor#monitor.stations),
  add_value(Monitor, StationName, Time, MeasurementType, Value, Station,
    maps:find({Time, MeasurementType}, Station#station.measurements));
add_value(_, _, _, _, _, _) ->
  {error, "Cannot add measurement to a non-existent station"}.

add_value(_, _, _, _, _, _, {ok, _}) ->
  {error, "Cannot add another measurement with the same location, time and type"};
add_value(Monitor, _, Time, MeasurementType, Value, Station, error) ->
  {ok, add_value_no_check(Monitor, Station, Time, MeasurementType, Value)}.

add_value_no_check(Monitor, Station, Time, MeasurementType, Value) ->
  Monitor#monitor{
    stations = maps:put(
      Station#station.name,
      Station#station{measurements = maps:put(
        {Time, MeasurementType}, #measurement{
          type = MeasurementType,
          value = Value,
          time = Time},
        Station#station.measurements)},
      Monitor#monitor.stations)}.

remove_value(Monitor, {_, _} = Location, Time, MeasurementType) when
  is_tuple(Time),
  is_atom(MeasurementType)
->
  remove_value(Monitor, Location, Time, MeasurementType,
    maps:find(Location, Monitor#monitor.locationToName));
remove_value(Monitor, Name, Time, MeasurementType) when
  is_tuple(Time),
  is_atom(MeasurementType)
->
  remove_value(Monitor, Name, Time, MeasurementType, {ok, Name}).

remove_value(Monitor, _, Time, MeasurementType, {ok, Name}) ->
  remove_value(Monitor, Name, Time, MeasurementType, {ok, Name},
    maps:find(Name, Monitor#monitor.stations));
remove_value(Monitor, _, _, _, error) -> {ok, Monitor}.

remove_value(Monitor, _, Time, MeasurementType, _, {ok, Station}) ->
  {ok, remove_value_no_check(Monitor, Station, Time, MeasurementType)};
remove_value(Monitor, _, _, _, _, error) -> {ok, Monitor}.

remove_value_no_check(Monitor, Station, Time, MeasurementType) ->
  Monitor#monitor{
    stations = maps:update(
      Station#station.name,
      Station#station{
        measurements = maps:remove(
          {Time, MeasurementType},
          Station#station.measurements)},
      Monitor#monitor.stations)}.

get_one_value(MeasurementType, Time, Station) ->
  value(maps:find({Time, MeasurementType}, Station#station.measurements)).

value({ok, #measurement{value = Value}}) -> Value;
value(_) -> {error, "No value for given specs"}.

get_station_mean(MeasurementType, Station) when is_atom(MeasurementType) ->
  {ElementsCount, TotalSum} =
    maps:fold(
      fun(_, M, {Length, Sum}) -> {Length + 1, Sum + M#measurement.value} end,
      {0, 0},
      maps:filter(
        fun(_, M) -> M#measurement.type == MeasurementType end,
        Station#station.measurements)),
  safe_division(TotalSum, ElementsCount).

get_daily_mean(Monitor, MeasurementType, Day) ->
  {TotalLength, TotalSum} =
    maps:fold(
      fun(_, {L, S}, {Length, Sum}) -> {Length + L, Sum + S} end,
      {0, 0},
      maps:map(
        fun(_, S) -> get_station_daily_mean(S, MeasurementType, Day) end,
        Monitor#monitor.stations)),
  safe_division(TotalSum, TotalLength).

get_station_daily_mean(Station, MeasurementType, Day) ->
  maps:fold(
    fun(_, M, {L, S}) -> {L + 1, S + M#measurement.value} end,
    {0, 0},
    maps:filter(
      fun(_, M) ->
        (M#measurement.type == MeasurementType)
          and (datetime_to_day(M#measurement.time) == Day)
      end,
      Station#station.measurements)).

get_daily_average_data_count(Monitor) ->
  Averages =
    maps:values(
      maps:map(
        fun(_, S) -> get_station_daily_data_count(S) end,
        Monitor#monitor.stations)),
  safe_division(lists:sum(Averages), length(Averages)).

get_station_daily_data_count(Station) ->
  Counts = maps:values(
    maps:map(
      fun(_, Measurements) -> length(Measurements) end,
      group_by(
        fun(M) -> datetime_to_date(M#measurement.time) end,
        maps:values(Station#station.measurements)))),
  safe_division(lists:sum(Counts), length(Counts)).

%% helpers

datetime_to_day(Date) -> {{_, _, Day}, {_, _, _}} = Date, Day.
datetime_to_date(DateTime) -> {Date, {_, _, _}} = DateTime, Date.

group_by(F, L) ->
  lists:foldr(
    fun({K, V}, M) ->
      case maps:find(K, M) of
        {ok, T} -> maps:put(K, [V | T], M);
        error -> maps:put(K, [V], M)
      end
    end,
    #{},
    [{F(X), X} || X <- L]).

safe_division(A, 0) when is_number(A) -> 0;
safe_division(A, B) when is_number(A), is_number(B) -> A / B.
