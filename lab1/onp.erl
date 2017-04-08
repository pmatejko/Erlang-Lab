-module(onp).
-compile(export_all).

-import(lists,[map/2]).
-import(string,[tokens/2]).
-import(string,[str/2]).
-import(math,[sqrt/1]).
-import(math,[pow/2]).
-import(math,[sin/1]).
-import(math,[cos/1]).
-import(math,[tan/1]).


divide(Line)
  -> tokens(Line, " ").


charsParse(X)
  when (X == "+" orelse X == "-" orelse X == "*" orelse X == "/" orelse X == "sqrt" orelse X == "pow" orelse X == "sin" orelse X == "cos" orelse X == "tan")
    -> X;
charsParse(X) ->
  case str(X,".") > 0 of
    true -> list_to_float(X);
    false -> list_to_integer(X)
  end.


calc([]) -> 0;
calc([X]) -> X;
calc([X1,X2|Tail]) -> calc(calcFirstSign([X1,X2|Tail])).


calcFirstSign([X1,X2|Tail])
  when (X2 == "sqrt" orelse X2 == "sin" orelse X2 == "cos" orelse X2 == "tan")
    -> [calcPart(X1,X2) | Tail];
calcFirstSign([X1,X2,X3|Tail])
  when (X3 == "+" orelse X3 == "-" orelse X3 == "*" orelse X3 == "/" orelse X3 == "pow")
    -> [calcPart(X1,X2,X3) | Tail];
calcFirstSign([X1,X2|Tail])
  -> [X1 | calcFirstSign([X2|Tail]) ].


calcPart(X1,X2,Sign) ->
  case Sign of
    "+" -> X1 + X2;
    "-" -> X1 - X2;
    "*" -> X1 * X2;
    "/" -> X1 / X2;
    "pow" -> pow(X1,X2)
  end.

calcPart(X,Sign) ->
  case Sign of
    "sqrt" -> sqrt(X);
    "sin" -> sin(X);
    "cos" -> cos(X);
    "tan" -> tan(X)
  end.



onp(Line) ->
  calc(map(fun(X) -> charsParse(X) end, divide(Line))).





%1 + 2 * 3 - 4 / 5 + 6
%1 + 2 + 3 + 4 + 5 + 6 * 7
%( (4 + 7) / 3 ) * (2 - 19)
%17 * (31 + 4) / ( (26 - 15) * 2 - 22 ) - 1
%
% 1 2 3 * + 4 5 / - 6 +
% 1 2 + 3 + 4 + 5 + 6 7 * +
% 4 7 + 3 / 2 19 - *
% 17 31 4 + * 26 15 - 2 * 22 - / 1 -
