:- module(utils).
:- interface.
:- import_module list, io, string, char.

:- pred parse_pair(string::in, {int, int}::out) is semidet.
:- pred read_lines(list(string)::out, io::di, io::uo) is det.
:- pred read_lines_char(list(list(char))::out, io::di, io::uo) is det.
:- pred read_line_det(string::out, io::di, io::uo) is det.
:- func iszero(int) = int.
:- pred transpose(list(list(T))::in, list(list(T))::out) is semidet.
:- pred uncons(list(T)::in, T::out, list(T)::out) is semidet.
:- pred det_uncons(list(T)::in, T::out, list(T)::out) is det.
:- pred split_on(T::in, list(T)::in, list(list(T))::out) is det.
:- pred split_when((pred(T))::in(pred(in) is semidet),
                   list(T)::in, list(list(T))::out) is det.
:- implementation.
:- import_module std_util.

parse_pair(S, {X, Y}) :-
    map(to_int, words(S), [X, Y]).

read_lines(Ls, !IO) :-
    read_line_as_string(Lr, !IO),
    ( Lr = error(_), Ls = []
    ; Lr = eof, Ls = []
    ; Lr = ok(L), Ls=[chomp(L)|Ls1], read_lines(Ls1, !IO)).

:- func chomp_char(list(char)) = list(char).
chomp_char([]) = [].
chomp_char([X]) = Z :- ( X = '\n' -> Z = [] ; Z = [X]).
chomp_char([X|[Y|Ys]]) = [X|chomp_char([Y|Ys])].

read_lines_char(Ls, !IO) :-
    read_line(Lr, !IO),
    ( Lr = error(_), Ls = []
    ; Lr = eof, Ls = []
    ; Lr = ok(L), Ls=[chomp_char(L)|Ls1], read_lines_char(Ls1, !IO)).

read_line_det(chomp(L), !IO) :-
    read_line_as_string(Lr, !IO),
    ( Lr = error(_), L = ""
    ; Lr = eof, L = ""
    ; Lr = ok(L)).


iszero(X) = Y :- X = 0 -> Y = 1; Y = 0.

uncons([X|Xs], X, Xs).
det_uncons(A, det_head(A), det_tail(A)).

transpose(Xss, Yss) :-
    (  map2(uncons, Xss, Ys, Xss1)
    -> transpose(Xss1, Yss1), Yss = [Ys|Yss1]
    ;  Yss = []
    ).

split_on(X, Xs, Xss) :-
    split_when((pred(Y::in) is semidet :- Y = X), Xs, Xss).

split_when(P, Xs, [Y|Ys]) :-
    take_while(isnt(P), Xs, Y, Xs1),
    (  uncons(Xs1, _XP, Xs2)
    -> split_when(P, Xs2, Ys)
    ;  Ys = []).
