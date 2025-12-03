:- module(day03).
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module char, list, string, int, utils, pair.

main(!IO) :-
    read_lines_char(Ls, !IO),
    filter_map(max_jolt, Ls, Ls1),
    print_line(foldl((func(X, Y) = Z :- X + Y = Z) , Ls1, 0), !IO).


:- pred max_jolt(list(char)::in, int::out) is semidet.
max_jolt(Ls, det_to_int(from_char_list([C1, C2]))) :-
    max_two(Ls, C1-C2).

:- pred max_two(list(T)::in, pair(T, T)::out) is semidet.
max_two([X|Xs], M1-M2) :-
    max_init(Xs, X, Xs, M1, [Y|Ys]),
    max_list(Ys, Y, M2).

:- pred max_init(list(T)::in, T::in, list(T)::in, T::out, list(T)::out) is semidet.
max_init([_], X, Xs, X, Xs). % don't consider the last element.
max_init([Y, Z | Zs], X, Xs, M, Mt) :-
    compare(R, X, Y),
    (  R = (<)
    -> max_init([Z|Zs], Y, [Z|Zs], M, Mt)
    ;  max_init([Z|Zs], X, Xs, M, Mt)).


:- pred max_list(list(T)::in, T::in, T::out) is det.
max_list([], X, X).
max_list([X|Xs], Y, M) :-
    compare(R, X, Y),
    (R = (<) -> max_list(Xs, Y, M); max_list(Xs, X, M)).

% is det if the input list is atleast _n_ in length.
:- pred max_n(int::in, list(T)::in, list(T)::out) is semidet.
max_n(0, _, []).
max_n(N, Xs, [X|Ys]) :-
    N > 0,
    max_start(N, Xs, X, Xs1),
    max_n(N-1, Xs1, Ys).
