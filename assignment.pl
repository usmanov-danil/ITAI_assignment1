 % Input: Put here positions of orcs, humans and touchdown as <type>(X,Y)
o(2, 2).
h(2, 0).
h(0, 2).
h(1, 1).
t(3, 2).

% ================= Facts =================
size(4).  % The size of one side of the field
:- dynamic([flag/1, solved/1, path/2, score/1]).  % Dynamic fact

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
choose_search(random, Moves) :- assert(flag(0)), assert(score(0)), assert(path([], inf)), retractall(solved(_)), random_loop(10000), path(Moves, _).  % Random search  % random_search(0, 0, Moves, [(0, 0)])
choose_search(greedy, Moves) :- assert(flag(0)), greedy_search(0, 0, Moves, [(0, 0)]).  % Greedyy search

% ================= BACKTRACKING SEARCH =================
backtracking_search(X, Y, [], _) :- is_touchdown(X, Y),  % Base case
    write("Jesus we got it! "), write(X), write(" "), write(Y), nl.
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
    write("Jesus we got it! "), write(X), write(" "), write(Y), nl.
random_search(X, Y, [Step | Moves], Visited) :-  % Recursion step
    random_member(Step, [up, right, down, left]),% pass_up, pass_right, pass_down, pass_left, pass_up_rigth, pass_down_rigth, pass_down_left, pass_up_left]),
    step(X, Y, X_NEXT, Y_NEXT, Step), !, 
    check_position(X_NEXT, Y_NEXT, Visited) -> 
    (
        score(Score), (h(X_NEXT, Y_NEXT) -> true; (retractall(score(_)), Score1 is Score + 1, assert(score(Score1)))),
        random_search(X_NEXT, Y_NEXT, Moves, [(X, Y) | Visited])
    ).

% ================= GREEDY SEARCH =================
greedy_search(X, Y, [], _) :- is_touchdown(X, Y),  % Base case
    write("Jesus we got it! "), write(X), write(" "), write(Y), nl.
greedy_search(X, Y, [Step | Moves], Visited) :-  % Recursion step
    
    step(X, Y, X_NEXT, Y_NEXT, Step), !, 
    check_position(X_NEXT, Y_NEXT, Visited) -> 
    (
        score(Score), (h(X_NEXT, Y_NEXT) -> true; (retractall(score(_)), Score1 is Score + 1, assert(score(Score1)))),
        greedy_search(X_NEXT, Y_NEXT, Moves, [(X, Y) | Visited])
    ).

% ================= PASS =================
step(X, Y, X_NEXT, Y_NEXT, pass_up) :-  % Pass UP
    flag(0),
    h(X, Y_NEXT),
    Y_NEXT > Y,
    o(X, Y_orc), Y_orc > Y, Y_orc < Y_NEXT,
    X_NEXT is X,
    retract(flag(_));
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_right) :-  % Pass RIRHT
    flag(0),
    h(X_NEXT, Y),
    X_NEXT > X,
    o(X_orc, Y), X_orc > X, X_orc < X_NEXT,
    Y_NEXT is Y,
    retract(flag(_));
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_down) :-  % Pass DOWN
    flag(0),
    h(X, Y_NEXT),
    Y_NEXT > Y,
    o(X, Y_orc), Y_orc < Y, Y_orc > Y_NEXT,
    X_NEXT is X,
    retract(flag(_));
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_left) :-  % Pass Left
    flag(0),
    h(X_NEXT, Y),
    X_NEXT > X,
    o(X_orc, Y), X_orc < X, X_orc > X_NEXT,
    Y_NEXT is Y,
    retract(flag(_));
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_up_rigth) :-  % Pass UP-Right
    flag(0),
    h(X_NEXT, Y_NEXT),
    o(X_orc, Y_orc), X_orc > X, X_orc < X_NEXT, Y_orc > Y, Y_orc < Y_NEXT,
    retract(flag(_));
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_down_rigth) :-  % Pass RIRHT-Down
    flag(0),
    h(X_NEXT, Y_NEXT),
    o(X_orc, Y_orc), X_orc > X, X_orc < X_NEXT, Y_orc < Y, Y_orc > Y_NEXT,
    retract(flag(_));
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_down_left) :-  % Pass Left-DOWN
    flag(0),
    h(X_NEXT, Y_NEXT),
    o(X_orc, Y_orc), X_orc < X, X_orc > X_NEXT, Y_orc < Y, Y_orc > Y_NEXT,
    retract(flag(_));
    assert(flag(1)).

step(X, Y, X_NEXT, Y_NEXT, pass_up_left) :-  % Pass Up-Left
    flag(0),
    h(X_NEXT, Y_NEXT),
    o(X_orc, Y_orc), X_orc < X, X_orc > X_NEXT, Y_orc > Y, Y_orc < Y_NEXT,
    retract(flag(_));
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
    % Init
    statistics(runtime,[Start|_]),


    % Simple backtracking
    % choose_search(backtracking, Moves),

    % Random 
    choose_search(random, Moves),

    % Greedy
    % choose_search(greedy, Moves),


    % Calculate output
    statistics(runtime,[Stop|_]),
    ExecutionTime is Stop - Start,
    score(X),
    write("Number of steps: "), writeln(X),
    write("The path is "), write(Moves), nl,
    write('Execution time: '), write(ExecutionTime), write(' ms.'), nl, abort.