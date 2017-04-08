-module(qsort).

-export([lessThan/2, grtEqThan/2, qs/1, randomElems/3, compareSpeeds/3]).


lessThan(List, Arg)
  -> [X || X <- List, X < Arg].

grtEqThan(List, Arg)
  -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot|Tail])
  -> qs(lessThan(Tail,Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail,Pivot)).

randomElems(N,Min,Max)
  -> [Min - 1 + rand:uniform(Max - Min + 1) || _ <- lists:seq(1,N)].

compareSpeeds(List, Fun1, Fun2)
  -> {timer:tc(Fun1, [List]), timer:tc(Fun2, [List])}.
