:- module(day06).
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, list, string, int, utils, bool.

main(!IO) :-
    read_lines(Ls, !IO),
    (  parse_input(Ls, Xss, Ops)
    -> reduce_sum(Xss, Ops, 0, Part1)
    ;  Part1 = -1 ),
    (  parse_input2(Ls, Xss1, Ops1)
    -> foldl_corresponding(app_op_acc, Ops1, Xss1, 0, Part2)
    ; Part2 = 0),
    print_line({Part1, Part2}, !IO).

:- type op ---> mult ; plus.

:- pred parse_input(list(string)::in,
                    list(list(int))::out, list(op)::out) is semidet.
parse_input(Ls, Xss, Ops) :-
    split_last(Ls, SXss, SOps),
    map(to_ints, SXss, Xss),
    map(to_op, words(SOps), Ops).

:- pred to_ints(string::in, list(int)::out) is semidet.
to_ints(S, Xs) :- map(to_int, words(S), Xs).

:- pred to_op(string::in, op::out) is semidet.
to_op("*", mult).
to_op("+", plus).

:- pred reduce_sum(list(list(int))::in, list(op)::in, int::in, int::out) is det.
reduce_sum(_, [], N, N).
reduce_sum(Xss, [Op|Ops], Ni, No) :-
    map2(det_uncons, Xss, Xs, Xss1), % det_uncons from utils
    reduce_sum(Xss1, Ops, Ni+apply_op(Op, Xs), No).

:- func apply_op(op, list(int)) = int.
apply_op(mult, Xs) = foldl(times, Xs, 1).
apply_op(plus, Xs) = foldl(plus, Xs, 0).

:- pred app_op_acc(op::in, list(int)::in, int::in, int::out) is det.
app_op_acc(O, Xs, Ni, No) :-
    No = apply_op(O, Xs)+Ni.

:- pred parse_input2(list(string)::in,
                     list(list(int))::out, list(op)::out) is semidet.
parse_input2(Ls, Xss, Ops) :-
    split_last(Ls, SXss, SOps),
    Xss1 = map(to_char_list, SXss),
    transpose(Xss1, Xss2),
    map(det_to_num, Xss2, Xss3),
    split_on(0, Xss3, Xss),
    map(to_op, words(SOps), Ops).

:- pred det_to_num(list(char)::in, int::out) is det.
det_to_num(Xs, N) :-
    Ys = filter(is_digit, Xs),
    (  is_empty(Ys)
    -> N = 0
    ; from_char_list(Ys, S),
      N = det_to_int(S)).

