% Input: Put here positions of orcs, humans and touchdown as <type>(X,Y)
%  include(input.pl).  % If you want to include input file with facts, uncomment this line and put file uinput,pl near the current file.

o(0, 1).
o(1, 0).
h(2, 2).
t(3, 2).
% ================= Facts =================
size(4).  % The size of one side of the field
attemp(100000). % The number of attemps for random search
:- dynamic([flag/1, solved/1, path/2, score/1, min_hypt/2, flag2/1]).  % Dynamic fact

% ================= Rules =================
wall_check(X, Y) :-  % Check that player is the inside field
    size(Size),
    X >= 0, X =< (Size - 1),
    Y >= 0, Y =< (Size - 1).

check_position(X, Y, Visited) :-  % Check position on safety: not orc and not visited.
    not(o(X, Y)),
    wall_check(X, Y),
    not(memberchk((X, Y), Visited)).

is_touchdown(X, Y) :-  % Check: is it touchdown point
    t(X, Y).

% ================= SEARCH VARIANTS =================
choose_search(backtracking, Moves) :- assert(flag(0)), assert(score(0)), backtracking_search(0, 0, Moves, [(0, 0)]).  % Backtracking search
choose_search(random, Moves) :- assert(flag(0)), assert(score(0)), assert(path([], inf)), retractall(solved(_)), attemp(Attemp), random_loop(Attemp), retractall(score(_)), path(Moves, Score), assert(score(Score)).  % Random search 
choose_search(greedy, Moves) :- assert(flag(0)), assert(score(0)), assert(min_hypt(up, inf)), greedy_search(0, 0, Moves, [(0, 0)]).  % Greedy search
choose_search(greedy_improoved, Moves) :- assert(flag(0)), assert(score(0)), assert(min_hypt(up, inf)), greedy_search_imp(0, 0, Moves, [(0, 0)]).  % Improoved greedy search

% ================= BACKTRACKING SEARCH =================
backtracking_search(X, Y, [], _) :- is_touchdown(X, Y),  % Base case
    write("We got it! "), write(X), write(" "), write(Y), nl.
backtracking_search(X, Y, [Step | Moves], Visited) :-  % Recursion step
    step(X, Y, X_NEXT, Y_NEXT, Step), 
    check_position(X_NEXT, Y_NEXT, Visited),
    score(Score), (h(X_NEXT, Y_NEXT) -> true; (retractall(score(_)), Score1 is Score + 1, assert(score(Score1)))),
    backtracking_search(X_NEXT, Y_NEXT, Moves, [(X, Y) | Visited]).

% ================= RANDOM SEARCH =================
random_loop(0) :-  solved(1) -> !; writeln("A path is not found:(").  % Loop base case 
random_loop(NumberAttemp) :-  % Loop step case
    retractall(score(_)), retractall(flag(_)), assert(score(0)), assert(flag(0)),
    (random_search(0, 0, Moves, [(0, 0)]) -> (score(Score_new), path(_, Score_old))
    -> (Score_new < Score_old -> (retractall(path(_,_)), assert(path(Moves, Score_new))) ; true)
    ); 
    NumberAttemp1 is NumberAttemp - 1,
    random_loop(NumberAttemp1).

random_search(X, Y, [], _) :- is_touchdown(X, Y), assert(solved(1)),  % Base case
    write("We got it! "), write(X), write(" "), write(Y), nl.
random_search(X, Y, [Step | Moves], Visited) :-  % Recursion step
    random_member(Step, [up, right, down, left, pass_up, pass_right, pass_down, pass_left]),
    step(X, Y, X_NEXT, Y_NEXT, Step), !, 
    check_position(X_NEXT, Y_NEXT, Visited) -> 
    (
        score(Score), (h(X_NEXT, Y_NEXT) -> true; (retractall(score(_)),
         Score1 is Score + 1, assert(score(Score1)))),
        random_search(X_NEXT, Y_NEXT, Moves, [(X, Y) | Visited])
    ).

% ================= GREEDY SEARCH =================
calc_hypt(X, Y, Step, Length, Visited) :-
    (flag(0) -> (retractall(flag2(_)), assert(flag2(0))) ; 
    (retractall(flag2(_)), assert(flag2(1)))),
    t(X_TOUCH, Y_TOUCH),
    (step(X, Y, X_NEXT, Y_NEXT, Step) -> 
    (flag2(0) -> (retractall(flag(_)), assert(flag(0))); true),
    (check_position(X_NEXT, Y_NEXT, Visited), X_DIF is abs((X_NEXT - X)), Y_DIF is abs((Y_NEXT-Y)), Y_DIF =< 1, X_DIF =< 1
    -> (Length is sqrt((X_TOUCH - X_NEXT)**2 + (Y_TOUCH - Y_NEXT)**2)) ; Length is inf)
    ;
        Length is inf
    ).
greedy(Step, [], _, _, _) :- min_hypt(Step, _).
greedy(Step, [First | Tail], X, Y, Visited) :-
    calc_hypt(X, Y, First, Length, Visited), min_hypt(_, Min_length),
    (Length < Min_length -> (retractall(min_hypt(_,_)), assert(min_hypt(First, Length))) ; true),
    greedy(Step, Tail, X, Y, Visited). 

greedy_search(X, Y, [], _) :- is_touchdown(X, Y),  % Base case
    write("We got it! "), write(X), write(" "), write(Y), nl.
greedy_search(X, Y, [Step | Moves], Visited) :-  % Recursion step
    retractall(min_hypt(_,_)), assert(min_hypt(up, inf)),
    retractall(flag2(_)), assert(flag2(0)),
    greedy(Step, [up, right, down, left, pass_up, pass_right, pass_down, pass_left, pass_up_right, pass_down_right, pass_down_left, pass_up_left], X, Y, Visited),
    step(X, Y, X_NEXT, Y_NEXT, Step), !, 
    check_position(X_NEXT, Y_NEXT, Visited) -> 
    (
        score(Score), (h(X_NEXT, Y_NEXT) -> true; (retractall(score(_)), 
        Score1 is Score + 1, assert(score(Score1)))),
        greedy_search(X_NEXT, Y_NEXT, Moves, [(X, Y) | Visited])
    ).

% ================= IMPROOVED GREEDY SEARCH =================
calc_hypt_imp(X, Y, Step, Length, Visited) :-
    (flag(0) -> (retractall(flag2(_)), assert(flag2(0))) ; 
    (retractall(flag2(_)), assert(flag2(1)))),
    t(X_TOUCH, Y_TOUCH),

    (step(X, Y, X_NEXT, Y_NEXT, Step) -> 
    (flag2(0) -> (retractall(flag(_)), assert(flag(0))); true),
    (check_position(X_NEXT, Y_NEXT, Visited), X_DIF is abs((X_NEXT - X)), Y_DIF is abs((Y_NEXT-Y)), Y_DIF =< 2, X_DIF =< 2
    -> (Length is sqrt((X_TOUCH - X_NEXT)**2 + (Y_TOUCH - Y_NEXT)**2)) ; Length is inf)
    ;
        Length is inf
    ).
    
greedy_imp(Step, [], _, _, _) :- min_hypt(Step, _).
greedy_imp(Step, [First | Tail], X, Y, Visited) :-
    calc_hypt_imp(X, Y, First, Length, Visited), min_hypt(_, Min_length),
    (Length < Min_length -> (retractall(min_hypt(_,_)), assert(min_hypt(First, Length))) ; true),
    greedy_imp(Step, Tail, X, Y, Visited). 

greedy_search_imp(X, Y, [], _) :- is_touchdown(X, Y),  % Base case
    write("We got it! "), write(X), write(" "), write(Y), nl.
greedy_search_imp(X, Y, [Step | Moves], Visited) :-  % Recursion step
    retractall(min_hypt(_,_)), assert(min_hypt(up, inf)),
    retractall(flag2(_)), assert(flag2(0)),
    greedy_imp(Step, [up, right, down, left, pass_up, pass_right, pass_down, pass_left, pass_up_right, pass_down_right, pass_down_left, pass_up_left], X, Y, Visited),
    step(X, Y, X_NEXT, Y_NEXT, Step), !, 
    check_position(X_NEXT, Y_NEXT, Visited) -> 
    (
        score(Score), (h(X_NEXT, Y_NEXT) -> true; (retractall(score(_)), 
        Score1 is Score + 1, assert(score(Score1)))),
        greedy_search_imp(X_NEXT, Y_NEXT, Moves, [(X, Y) | Visited])
    ).

% ================= PASS =================
step(X, Y, X_NEXT, Y_NEXT, pass_up) :-  % Pass UP
    flag(0),
    h(X, Y_NEXT),
    Y_NEXT > Y,
    not((h(X, Y_hum), Y_hum > Y, Y_hum < Y_NEXT)),
    not((o(X, Y_orc), Y_orc > Y, Y_orc < Y_NEXT)),
    X_NEXT is X,
    retract(flag(_)),
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_right) :-  % Pass RIRHT
    flag(0),
    h(X_NEXT, Y),
    X_NEXT > X,
    not((h(X_hum, Y), X_hum > X, X_hum < X_NEXT)),
    not((o(X_orc, Y), X_orc > X, X_orc < X_NEXT)),
    Y_NEXT is Y,
    retract(flag(_)),
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_down) :-  % Pass DOWN
    flag(0),
    h(X, Y_NEXT),
    Y_NEXT < Y,
    not((h(X, Y_hum), Y_hum < Y, Y_hum > Y_NEXT)),
    not((o(X, Y_orc), Y_orc < Y, Y_orc > Y_NEXT)),
    X_NEXT is X,
    retract(flag(_)),
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_left) :-  % Pass Left
    flag(0),
    h(X_NEXT, Y),
    X_NEXT < X,
    not((h(X_hum, Y), X_hum < X, X_hum > X_NEXT)),
    not((o(X_orc, Y), X_orc < X, X_orc > X_NEXT)),
    Y_NEXT is Y,
    retract(flag(_)),
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_up_right) :-  % Pass UP-Right
    flag(0),
    h(X_NEXT, Y_NEXT), (X_NEXT - X) =:= (Y_NEXT - Y), X_NEXT > X, Y_NEXT > Y,
    ((h(X_hum, Y_hum), X_DIF_HUM is X_hum - X, Y_DIF_HUM is Y_hum - Y)),
    not(((Y_hum < Y_NEXT), (Y_hum > Y), (X_hum < X_NEXT), (X_hum > X), (X_DIF_HUM == Y_DIF_HUM))),
    ((o(X_orc, Y_orc), X_DIF is X_orc - X, Y_DIF is Y_orc - Y)),
    not(((X_orc - X) =:= (Y_orc - Y),(Y_orc < Y_NEXT), (Y_orc > Y), (X_orc < X_NEXT), (X_orc > X), (X_DIF == Y_DIF))),
    retract(flag(_)),
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_down_right) :-  % Pass RIRHT-Down
    flag(0),
    h(X_NEXT, Y_NEXT), (X_NEXT - X) =:= (Y - Y_NEXT), Y_NEXT < Y, X_NEXT > X,
    (( h(X_hum, Y_hum), X_DIF_HUM is X_hum - X, Y_DIF_HUM is Y - Y_hum)),
    not(((Y_hum > Y_NEXT), (Y_hum < Y), (X_hum < X_NEXT), (X_hum > X), (X_DIF_HUM == Y_DIF_HUM))),
    (( o(X_orc, Y_orc), X_DIF is X_orc - X, Y_DIF is Y - Y_orc)), 
    not(((X_orc - X) =:= (Y - Y_orc),(Y_orc > Y_NEXT), (Y_orc < Y), (X_orc < X_NEXT), (X_orc > X), (X_DIF == Y_DIF))),
    retract(flag(_)),
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_down_left) :-  % Pass Left-DOWN
    flag(0),
    h(X_NEXT, Y_NEXT), (X - X_NEXT) =:= (Y - Y_NEXT), X_NEXT < X, Y_NEXT < Y,
    ((h(X_hum, Y_hum), X_DIF_HUM is X - X_hum, Y_DIF_HUM is Y - Y_hum)),
    not(((Y_hum > Y_NEXT), (Y_hum < Y), (X_hum > X_NEXT), (X_hum < X), (X_DIF_HUM == Y_DIF_HUM))),
    ((o(X_orc, Y_orc), X_DIF is X - X_orc, Y_DIF is Y - Y_orc)), 
    not(((X - X_orc) =:= (Y - Y_orc),(Y_orc > Y_NEXT), (Y_orc < Y), (X_orc > X_NEXT), (X_orc < X), (X_DIF == Y_DIF))),
    retract(flag(_)),
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_up_left) :-  % Pass Up-Left
    flag(0),
    h(X_NEXT, Y_NEXT), (X - X_NEXT) =:= (Y_NEXT - Y), Y_NEXT > Y, X_NEXT < X,
    ((h(X_hum, Y_hum), X_DIF_HUM is X - X_hum, Y_DIF_HUM is Y_hum - Y)),
    not(((Y_hum < Y_NEXT), (Y_hum > Y), (X_hum > X_NEXT), (X_hum < X), (X_DIF_HUM == Y_DIF_HUM))),
    ((o(X_orc, Y_orc), X_DIF is X - X_orc, Y_DIF is Y_orc - Y)), 
    not(((X - X_orc) =:= (Y_orc - Y),(Y_orc < Y_NEXT), (Y_orc > Y), (X_orc > X_NEXT), (X_orc < X), (X_DIF == Y_DIF))),
    retract(flag(_)),
    assert(flag(1)).

% ================= STEPS =================
step(X, Y, X_NEXT, Y_NEXT, up) :-  % Step UP
    X_NEXT is X,
    Y_NEXT is Y + 1.

step(X, Y, X_NEXT, Y_NEXT, right) :-  % Step RIRHT
    X_NEXT is X + 1,
    Y_NEXT is Y.

step(X, Y, X_NEXT, Y_NEXT, down) :-  % Step DOWN
    X_NEXT is X,
    Y_NEXT is Y - 1.

step(X, Y, X_NEXT, Y_NEXT, left) :-  % Step Left
    X_NEXT is X - 1,
    Y_NEXT is Y.

% ================= MAIN =================
go:-
    statistics(runtime,[Start|_]),

    % Simple backtracking
    % choose_search(backtracking, Moves),

    % Random 
    % choose_search(random, Moves),

    % Greedy
    choose_search(greedy, Moves),

    % Improoved greedy search
    % choose_search(greedy_improoved, Moves),

    % Calculate output
    statistics(runtime,[Stop|_]),
    ExecutionTime is Stop - Start,
    score(X),
    write("Number of steps: "), writeln(X),
    write("The path is "), write(Moves), nl,
    write('Execution time: '), write(ExecutionTime), write(' ms.'), nl, abort.