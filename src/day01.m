:- module(day01).
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module char, list, string, int, utils.

:- pred parse_inst(list(char)::in, int::out) is semidet.
parse_inst(['L'|Nums], -1*I) :- to_int(from_char_list(Nums), I).
parse_inst(['R'|Nums], I) :- to_int(from_char_list(Nums), I).

:- pred parse_inst_string(string::in, int::out) is semidet.
parse_inst_string(S, I) :- parse_inst(to_char_list(S), I).

%% stashed in utils
%% :- func iszero(int) = int.
%% iszero(X) = Y :- X = 0 -> Y = 1; Y = 0.

:- pred part1(int::in, int::in, int::out, int::in, int::out) is det.
part1(Diff, Pos1, Pos2, C1, C1+iszero(Pos2)) :- Pos2 = (Pos1+Diff) mod 100.

%%% THIS IS WRONG needs correction.
:- pred part2(int::in, int::in, int::out, int::in, int::out) is det.
part2(Diff, Pos1, (Pos1+Diff) mod 100, C1, C1+ncirc(Pos1, Diff)).

:- func ncirc1(int, int) = int.
ncirc1(Pos, Diff) = abs(div(Pos+Diff, 100)).

:- func ncirc(int, int) = int.
ncirc(Pos, Diff) = abs(div(Pos+Diff, 100)) + I :-
    ( Pos+Diff = 0
    -> I = 1
    ; ( (Pos = 0, Diff < 0) -> I = -1; I = 0 )).

main(!IO) :- read_lines(Ls, !IO),
             (map(parse_inst_string, Ls, Is); Is = []),
             foldl2(part1, Is, 50, _End1, 0, Part1),
             foldl2(part2, Is, 50, _End2, 0, Part2),
             print_line(Part1, !IO),
             print_line(Part2, !IO).

