-module(lab2).

-export([randomElems/3, map/2, filter/2, digits/1, digitsSum/1, millionElems/0]).

randomElems(N,Min,Max)
  -> [Min - 1 + rand:uniform(Max - Min + 1) || _ <- lists:seq(1,N)].

map(_, []) -> [];
map(Fun, List)
  -> [Fun(X) || X <- List].

filter(_, []) -> [];
filter(Fun, List)
  -> [X || X <- List, Fun(X)].

digits(0) -> [];
digits(X) -> digits(X div 10) ++ [X rem 10].

digitsSum(Number)
  -> lists:foldl(fun(X, Acc) -> X + Acc end, 0, digits(Number)).

millionElems()
  -> lists:filter(fun(X) -> digitsSum(X) rem 3 == 0 end, randomElems(1000000,-1000,1000)).
