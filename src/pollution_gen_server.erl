-module(pollution_gen_server).
-behaviour(gen_server).
-export([
  %% gen_server API
  start_link/0,
  init/1,
  stop/0,
  handle_cast/2,
  handle_call/3,
  handle_info/2,
  terminate/2,
  code_change/3,

  %% pollution server API
  crash/0,
  add_station/2,
  add_value/4,
  remove_value/3,
  get_one_value/3,
  get_station_mean/2,
  get_daily_mean/2,
  get_daily_average_data_count/0
]).

start_link() ->
  gen_server:start_link({local, pollution_server}, ?MODULE, []).

init([]) ->
  {ok, initializing, 0}.

stop() ->
  gen_server:cast(pollution_server, stop).

crash() ->
  gen_server:cast(pollution_server, crash).

handle_cast(stop, Monitor) ->
  {stop, normal, Monitor};

handle_cast(_, Value) ->
  {noreply, Value}.

handle_call({add_station, [Name, Location]}, _From, Monitor) ->
  case pollution:add_station(Monitor, Name, Location) of
    {ok, NewMonitor} ->
      {reply, ok, NewMonitor};
    {error, Reason} ->
      {reply, {error, Reason}, Monitor}
  end.

handle_info(timeout, _State) ->
  {noreply, ready}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% API

add_station(Name, {_, _} = Location) ->
  gen_server:call(pollution_server, {add_station, [Name, Location]}).

add_value({_, _} = Location, Time, MeasurementType, Value) ->
  gen_server:call(pollution_server, {add_value, [Location, Time, MeasurementType, Value]}).

remove_value({_, _} = Location, Time, MeasurementType) ->
  gen_server:call(pollution_server, {remove_value, [Location, Time, MeasurementType]}).

get_one_value(MeasurementType, Time, Station) ->
  gen_server:call(pollution_server, {get_one_value, [MeasurementType, Time, Station]}).

get_station_mean(MeasurementType, Station) ->
  gen_server:call(pollution_server, {get_station_mean, [MeasurementType, Station]}).

get_daily_mean(MeasurementType, Day) ->
  gen_server:call(pollution_server, {get_daily_mean, [MeasurementType, Day]}).

get_daily_average_data_count() ->
  gen_server:call(pollution_server, {get_daily_average_data_count, []}).
