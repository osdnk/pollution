-module(pollution_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

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

create_monitor_test() ->
  {ok, {monitor, #{}, #{}}} = pollution:create_monitor().

add_station_test() ->
  {ok, Monitor} = pollution:create_monitor(),

  StationA = #station{name = "A", location = {0, 1}},
  StationB = #station{name = "B", location = {2, 3}},

  {ok, MonitorA} = pollution:add_station(Monitor, "A", {0, 1}),

  % Check if there is the new station present in monitor
  ?assertEqual(
    maps:get("A", MonitorA#monitor.stations),
    StationA),

  {ok, MonitorB} = pollution:add_station(MonitorA, "B", {2, 3}),

  % Check if adding another station does not remove the previous one
  ?assertEqual(
    #monitor{
      locationToName = #{{0, 1} => "A", {2, 3} => "B"},
      stations = #{
      "A" => StationA,
      "B" => StationB
    }},
    MonitorB),

  ?assertMatch(
    {error, "Wrong location format"},
    pollution:add_station(MonitorB, "X", 1)),

  % Make sure it is not possible to have another station with existing name
  % or location
  ?assertMatch(
    {error, "Station already exists"},
    pollution:add_station(MonitorB, "A", {0, 1})).

add_value_test() ->
  Time = calendar:local_time(),
  {ok, Monitor} = pollution:create_monitor(),
  {ok, MonitorA} = pollution:add_station(Monitor, "A", {0, 1}),
  {ok, MonitorB} = pollution:add_value(MonitorA, {0, 1}, Time, pm10, 100),

  ?assertEqual(
    #monitor{
      locationToName = #{{0, 1} => "A"},
      stations = #{
        "A" => #station{
          name = "A",
          location = {0, 1},
          measurements = #{
            {Time, pm10} => #measurement{
              type = pm10,
              value = 100,
              time = Time}}}}},
    MonitorB),

  % Don't allow to add duplicated measurement
  ?assertMatch(
    {error,
      "Cannot add another measurement with the same location, time and type"},
    pollution:add_value(MonitorB, {0, 1}, Time, pm10, 72)),

  % Don't allow to add measurement to a station that does not exist
  ?assertMatch(
    {error, "Cannot add measurement to a non-existent station"},
    pollution:add_value(
      MonitorB,
      {-1000, 0},
      calendar:local_time(),
      temperature, 21.5)).

remove_value_test() ->
  Time = calendar:local_time(),
  {ok, Monitor} = pollution:create_monitor(),
  {ok, MonitorA} = pollution:add_station(Monitor, "A", {0, 1}),
  {ok, MonitorB} = pollution:add_value(MonitorA, {0, 1}, Time, pm10, 100),

  % Make sure the measurement is added
  ?assertEqual(
    #monitor{
      locationToName = #{{0, 1} => "A"},
      stations = #{
        "A" => #station{
          name = "A",
          location = {0, 1},
          measurements = #{
            {Time, pm10} => #measurement{
              type = pm10,
              value = 100,
              time = Time}}}}},
    MonitorB
  ),

  {ok, MonitorC} = pollution:remove_value(MonitorB, "A", Time, pm10),

  % And now that it is gone
  ?assertEqual(
    #monitor{
      locationToName = #{{0, 1} => "A"},
      stations = #{
        "A" => #station{
          name = "A",
          location = {0, 1},
          measurements = #{}}}},
    MonitorC).

get_one_value_test() ->
  Time = calendar:local_time(),
  {ok, Monitor} = pollution:create_monitor(),
  {ok, MonitorA} = pollution:add_station(Monitor, "A", {0, 1}),
  {ok, MonitorB} = pollution:add_value(MonitorA, {0, 1}, Time, pm10, 100),
  Station = maps:get("A", MonitorB#monitor.stations),

  ?assertEqual(
    100,
    pollution:get_one_value(pm10, Time, Station)),

  % Check if it fails correctly
  ?assertMatch(
    {error, "No value for given specs"},
    pollution:get_one_value(pm25, calendar:local_time(), Station)).

get_station_mean_test() ->
  {ok, Monitor} = pollution:create_monitor(),
  {ok, MonitorA} = pollution:add_station(Monitor, "A", {0, 1}),
  {Date, {Hour, Minute, Second}} = calendar:local_time(),

  {ok, MonitorB} = pollution:add_value(
    MonitorA, {0, 1}, {Date, {Hour, Minute, Second}}, pm10, 100),
  {ok, MonitorC} = pollution:add_value(
    MonitorB,
    {0, 1},
    {Date, {(Hour + 1) rem 24, Minute, Second}},
    pm10,
    50),
  {ok, MonitorD} = pollution:add_value(
    MonitorC,
    {0, 1},
    {Date, {(Hour + 2) rem 24, Minute, Second}},
    pm25,
    10),

  Station = maps:get("A", MonitorD#monitor.stations),

  ?assertEqual(75.0, pollution:get_station_mean(pm10, Station)).

get_daily_mean_test() ->
  {ok, Monitor} = pollution:create_monitor(),
  {ok, MonitorA} = pollution:add_station(Monitor, "A", {0, 1}),
  {ok, MonitorB} = pollution:add_station(MonitorA, "B", {2, 3}),
  {{_, _, Day} = Date, {Hour, Minute, Second}} = calendar:local_time(),

  {ok, MonitorC} = pollution:add_value(
    MonitorB, "A", {Date, {Hour, Minute, Second}}, pm10, 80),
  {ok, MonitorD} = pollution:add_value(
    MonitorC, "A", {Date, {(Hour + 1) rem 24, Minute, Second}}, pm10, 120),
  {ok, MonitorE} = pollution:add_value(
    MonitorD, "B", {Date, {(Hour + 2) rem 24, Minute, Second}}, pm10, 40),

  ?assertEqual(80.0, pollution:get_daily_mean(MonitorE, pm10, Day)).

get_daily_average_data_count_test() ->
  {ok, Monitor} = pollution:create_monitor(),
  {ok, MonitorA} = pollution:add_station(Monitor, "A", {0, 1}),
  {ok, MonitorB} = pollution:add_station(MonitorA, "B", {2, 3}),
  {{Year, Month, Day}, Time} = calendar:local_time(),

  {ok, MonitorC} = pollution:add_value(
    MonitorB, "A", {{Year, Month, Day}, Time}, pm10, 80),
  {ok, MonitorD} = pollution:add_value(
    MonitorC, "A", {{Year, Month, (Day + 1) rem 30}, Time}, pm10, 120),
  {ok, MonitorE} = pollution:add_value(
    MonitorD, "A", {{Year, Month, (Day + 1) rem 30}, Time}, humidity, 40),
  {ok, MonitorF} = pollution:add_value(
    MonitorE, "B", {{Year, Month, Day}, Time}, pm10, 40),
  {ok, MonitorG} = pollution:add_value(
    MonitorF, "B", {{Year, Month, Day}, Time}, pm25, 80),

  ?assertEqual(1.75, pollution:get_daily_average_data_count(MonitorG)).

