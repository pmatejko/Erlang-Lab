-module(lab1).
-compile(export_all).

-record(produkt, {typ, nazwa, cena, wlasciwosci}).

createProdukt(T,N,C,W) ->
  #produkt{typ = T, nazwa = N, cena = C, wlasciwosci = W}.


power(0,0) -> error;
power(_,0) -> 1;
power(A,N) -> A * power(A,N-1).

contains([],_) -> false;
contains([X|Xs], A) -> (A =:= X) orelse contains(Xs, A).

duplicateElements([]) -> [];
duplicateElements([X|Xs]) -> [X,X] ++ duplicateElements(Xs).

divisibleBy([],_) -> [];
divisibleBy([X|Xs],N)
  when(X rem N == 0) -> [X] ++ divisibleBy(Xs, N);
divisibleBy([_|Xs],N) -> divisibleBy(Xs,N).

toBinary(0) -> 0;
toBinary(N) -> toBinary(N div 2) * 10 + (N rem 2).


%1 + 2 * 3 - 4 / 5 + 6
%1 + 2 + 3 + 4 + 5 + 6 * 7
%( (4 + 7) / 3 ) * (2 - 19)
%17 * (31 + 4) / ( (26 - 15) * 2 - 22 ) - 1
%
% 1 2 3 * + 4 5 / - 6 +
% 1 2 + 3 + 4 + 5 + 6 7 * +
% 4 7 + 3 / 2 19 - *
% 17 31 4 + * 26 15 - 2 * 22 - / 1 -
