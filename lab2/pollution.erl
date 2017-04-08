-module(pollution).
-compile(export_all).


-record(station,{name, coords}).
-record(value,{date, type, val}).


createStation(Name, {X, Y}) ->
  #station{name = Name, coords = {X, Y}}.

createValue({{Year,Month,Day},{Hour,Minute,Second}}, Type, Val) ->
  #value{date = {{Year,Month,Day},{Hour,Minute,Second}}, type = Type, val = Val}.



stationExists(Name, Coords, Monitor) ->
  lists:foldl(fun(X, Acc) -> X#station.name == Name orelse X#station.coords == Coords orelse Acc end, false, maps:keys(Monitor)).

getAllDatesAndTypes(Vals) ->
  lists:map(fun(X) -> {X#value.date, X#value.type} end, Vals).

getAllValues(Monitor) ->
  lists:concat(lists:map(fun(X) -> maps:get(X,Monitor) end, maps:keys(Monitor))).



getKey(Coords, Monitor)
  when is_tuple(Coords) ->
    lists:filter(fun(X) -> X#station.coords == Coords end, maps:keys(Monitor));
getKey(Name, Monitor) ->
  lists:filter(fun(X) -> X#station.name == Name end, maps:keys(Monitor)).

getVal(Date, Type, Vals) ->
  lists:filter(fun(X) -> X#value.date == Date andalso X#value.type == Type end, Vals).

getValsOfType(Type, Vals) ->
  lists:filtermap(fun(X) -> case X#value.type of Type -> {true, X#value.val}; _ -> false end end, Vals).

getValuesFromDay(Day, Vals) ->
  lists:filter(fun(X) -> sameDay(X#value.date, Day) end, Vals).

sameDay({{Y,M,D},{_,_,_}}, TheDay) ->
  {Y,M,D} == TheDay.

calcMean(Vals) ->
  lists:foldl(fun(X,Acc) -> X + Acc end, 0, Vals) / length(Vals).

sameTypeDayAndOverLimit(Type, Day, Key, Monitor) ->
  lists:foldl(fun(Val, Acc) -> (Val#value.type == Type andalso sameDay(Val#value.date, Day) andalso overLimit(Val#value.val, Type)) orelse Acc end, false, maps:get(Key,Monitor)).

overLimit(Val, Type) ->
  case Type of
    "PM10" -> Val > 50;
    "PM2,5" -> Val > 25;
    "Pb" -> Val > 0.5;
    "CO" -> Val > 10000;
    "SO2" -> Val > 125;
    "NO2" -> Val > 40;
    "C6H6" -> Val > 5;
    _ -> false
  end.




createMonitor() ->
  #{}.

addStation(Name, Coords, Monitor) ->
  case stationExists(Name, Coords, Monitor) of
    false -> maps:put(createStation(Name, Coords), [], Monitor);
    true -> io:format("Station with that name/coords is already in Monitor~n")
  end.

addValue(Station, Date, Type, Val, Monitor) ->
  case getKey(Station, Monitor) of
    [] -> io:format("Station with that name/coords doesn't exist~n");
    [Key] -> OldVals = maps:get(Key, Monitor),
             case lists:member({Date, Type}, getAllDatesAndTypes(OldVals)) of
               false -> maps:update(Key, OldVals ++ [createValue(Date,Type,Val)], Monitor);
               true -> io:format("This exact value is already added in this station~n")
             end
  end.

removeValue(Station, Date, Type, Monitor) ->
  case getKey(Station, Monitor) of
    [] -> io:format("Station with that name/coords doesn't exist~n");
    [Key] -> OldVals = maps:get(Key, Monitor),
             case getVal(Date, Type, OldVals) of
               [] -> io:format("No value of that date and type found~n");
               [Val] -> maps:update(Key, lists:delete(Val, OldVals), Monitor)
             end
  end.

getOneValue(Station, Date, Type, Monitor) ->
  case getKey(Station, Monitor) of
    [] -> io:format("Station with that name/coords doesn't exist~n");
    [Key] -> case getVal(Date, Type, maps:get(Key, Monitor)) of
               [] -> io:format("No value of that date and type found~n");
               [Val] -> Val#value.val
             end
  end.

getStationMean(Station, Type, Monitor) ->
  case getKey(Station, Monitor) of
    []-> io:format("Station with that name/coords doesn't exist~n");
    [Key] -> case getValsOfType(Type, maps:get(Key, Monitor)) of
               [] -> io:format("No values of that type found~n");
               Vals -> calcMean(Vals)
             end
  end.

getDailyMean(Day, Type, Monitor) ->
  case getValsOfType(Type, getValuesFromDay(Day, getAllValues(Monitor))) of
    [] -> io:format("No values of given type and day~n");
    Vals -> calcMean(Vals)
  end.

getDailyOverLimit(Day, Type, Monitor) ->
  length(lists:filter(fun(Key) -> sameTypeDayAndOverLimit(Type,Day,Key,Monitor) end, maps:keys(Monitor))).
