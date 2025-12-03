:- module(utils).
:- interface.
:- import_module list, io, string, char.

:- pred parse_pair(string::in, {int, int}::out) is semidet.
:- pred read_lines(list(string)::out, io::di, io::uo) is det.
:- pred read_lines_char(list(list(char))::out, io::di, io::uo) is det.
:- pred read_line_det(string::out, io::di, io::uo) is det.
:- func iszero(int) = int.

:- implementation.

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
