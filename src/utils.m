:- module(utils).
:- interface.
:- import_module list, io, string.

:- pred parse_pair(string::in, {int, int}::out) is semidet.
:- pred read_lines(list(string)::out, io::di, io::uo) is det.

:- implementation.

parse_pair(S, {X, Y}) :-
    map(to_int, words(S), [X, Y]).

read_lines(Ls, !IO) :-
    read_line_as_string(Lr, !IO),
    ( Lr = error(_), Ls = []
    ; Lr = eof, Ls = []
    ; Lr = ok(L), Ls=[chomp(L)|Ls1], read_lines(Ls1, !IO)).
