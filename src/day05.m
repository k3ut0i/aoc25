:- module(day05).
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module char, list, string, int, utils, diet.

main(!IO) :-
    read_lines(Ls, !IO),
    (  parse_input(Ls, Is, Es)
    -> create_set(Is, S),
       filter((pred(X::in) is semidet :- member(X, S)), Es, Ms),
       length(Ms, Part1),
       Part2 = count(S)
    ; Part1 = -1, Part2 = -1 ),
    print_line({Part1, Part2}, !IO).

:- pred parse_input(list(string)::in,
                    list({int, int})::out, list(int)::out) is semidet.
parse_input(Ls, Is, Es) :-
    take_while((pred(X::in) is semidet :- length(X) > 0), Ls, Is1, Es1),
    map(parse_interval, Is1, Is),
    map(to_int, det_tail(Es1), Es).

:- pred create_set(list({int, int})::in, diet(int)::out) is det.
create_set(Xs, S) :-
    foldl((pred({A, B}::in, Xi::in, Xo::out) is det:- insert_interval(A, B, Xi, Xo)),
         Xs, init, S).

:- pred parse_interval(string::in, {int, int}::out) is semidet.
parse_interval(S, {det_to_int(X), det_to_int(Y)}) :-
    split_at_char('-', S) = [X, Y].
