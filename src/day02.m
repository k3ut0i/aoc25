:- module(day02).
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module char, list, string, int, utils, pair.

:- pred parse_pair1(string::in, pair(int, int)::out) is semidet.
parse_pair1(S, S1-S2) :-
    map(to_int, split_at_char('-', S), [S1, S2]).

:- func split_intervals(pair(int, int)) = list(pair(int, int)).
split_intervals(X-Y) = Ls :-
    Xn = ndigits(X), Yn = ndigits(Y),
    (  Xn = Yn
    -> Ls = [X-Y]
    ; Ls = [(X-maxnum(Xn))|split_intervals(minnum(Xn+1)-Y)]).

:- func ndigits(int) = int.
ndigits(X) = length(int_to_string(X)). %% dint' have time.

:- func maxnum(int) = int.
maxnum(X) = pow(10, X)-1.

:- func minnum(int) = int.
minnum(X) = pow(10, X-1).

:- func count_inv(pair(int, int)) = int.
count_inv(X-Y) = Z :-
    (  odd(ndigits(X))
    -> Z = 0
    ;  Xh = splithalf(X), Yh = splithalf(Y),
       Z = Yh-Xh-1+belowrep(X, Xh)+aboverep(Y, Yh)).

:- func splithalf(int) = int.
splithalf(X) = to_int(Z) :-
    left(int_to_string(X), ndigits(X)/2, Z).

:- func belowrep(int, int) = int.
belowrep(X, Xh) = Z :-
    S = int_to_string(Xh),
    (  to_int(S++S) <= X
    -> Z = 1
    ;  Z = 0 ).

:- func aboverep(int, int) = int.
aboverep(X, Xh) = Z :-
    S = int_to_string(Xh),
    (  to_int(S++S) >= X
    -> Z = 1
    ;  Z = 0 ).

main(!IO) :- read_line_det(L, !IO),
             Ls = split_at_char(',', L),
             (map(parse_pair1, Ls, Os); Os = []),
             print_line(map(split_intervals, Os), !IO).
