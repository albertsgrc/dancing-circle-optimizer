men([fabio-174, jordan-176, joe-180, george-171, eddard-172, tyrion-180, gregor-175]).
women([cersei-165, sansa-171, melisandre-165, brienne-170, margaery-162, ygritte-168, missandei-164]).

% RULE SEMANTICS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dancesOnRightSide(X, Y) <=> X must dance on the right side of Y

% doesNotDanceOnRightSide(X, Y) <=> X cannot dance on the right side of Y
% doesNotDance(X, Y) <=> X cannot dance with Y, on any side
% doesNotDance(X) <=> X does not dance

% doesNotDanceBetween(X, Y, Z) <=> Z cannot dance between X and Y
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% MANUALLY SPECIFIED CONDITIONS
dancesOnRightSide(sansa, tyrion).

doesNotDanceOnRightSide(cersei, jordan).
doesNotDanceOnRightSide(brienne, tyrion).
doesNotDance(george, brienne).
doesNotDance(fabio, cersei).
doesNotDanceOnRightSide(george, cersei).

doesNotDance(margaery).
doesNotDance(gregor).

doesNotDanceBetween(george, eddard, _).  % No one can dance between these two men




%% DERIVATE PREDICATES
doesNotDanceOnRightSide(X, Y) :- doesNotDance(X, Y).
doesNotDanceOnRightSide(Y, X) :- doesNotDance(X, Y).
doesNotDanceOnRightSide(X, _) :- doesNotDance(X).
doesNotDanceOnRightSide(X, Y) :- dancesOnRightSide(X, W), W \= Y, Y \= wildcard.

doesNotDanceBetween(X, Y, Z) :- doesNotDanceBetween(Y, W, Z), W = X, !.

man(X) :- men(L), member(X,L).
woman(X) :- women(L), member(X,L).

manDancer(X-M) :- man(X-M), \+doesNotDance(X).
womanDancer(X-F) :- woman(X-F), \+doesNotDance(X).

numMaleDancers(X) :- findall(Y, manDancer(Y), L), length(L, X).
numFemaleDancers(X) :- findall(Y, womanDancer(Y), L), length(L, X).

firstMan(X) :- men([X|_]).




%% WRITING THE DANCING CIRLES
printDancingCircle([]).
printDancingCircle([D-_|L]) :-
    write(-), write(D), printDancingCircle(L).

printDancingCircles([]).
printDancingCircles([R|L]) :- printDancingCircle(R.dancingCircle), write(' ('), write(R.score), write(')'), nl, printDancingCircles(L).




%% DANCING CIRCLE SCORING (HIGHER SCORE MEANS WORSE)
% Sum of the squares of height differences between dancers who dance next to each other
sumSquaredHeightDifferencesImm([], 0).
sumSquaredHeightDifferencesImm([_], 0).
sumSquaredHeightDifferencesImm([_-X,_-Y|L], P) :-
    DiffSq is (X - Y)*(X - Y),
    sumSquaredHeightDifferencesImm([_-Y|L], R),
    P is DiffSq + R.
sumSquaredHeightDifferences(L, X) :-
    sumSquaredHeightDifferencesImm(L, Y), nth1(1, L, _-A1), !, length(L, LL), nth1(LL, L, _-A2),
    X is Y + (A1-A2)*(A1-A2).
sumSquaredHeightDifferences([], 0).

% Sum of the squares of height differences between dancers who dance at distance 2 (someone in between them)
sumSquaredHeightDifferencesDist2Imm(L, 0) :- length(L, X), X < 3.
sumSquaredHeightDifferencesDist2Imm([_-X,_,_-Z|L], P) :-
    DiffSq is (X - Z)*(X - Z),
    sumSquaredHeightDifferencesDist2Imm([_-Z|L], R),
    P is DiffSq + R.
sumSquaredHeightDifferencesDist2(L, X) :-
    sumSquaredHeightDifferencesDist2Imm(L, Y), nth1(1, L, _-A1), !, length(L, LL), LL1 is LL-1, nth1(LL1, L, _-A2),
    X is Y + (A1-A2)*(A1-A2).
sumSquaredHeightDifferencesDist2(L, 0) :- length(L, X), X < 2.

scoreDancingCircle(L, X) :-
    sumSquaredHeightDifferences(L, S1),
    sumSquaredHeightDifferencesDist2(L, S2),
    X is S2*10 + S1.

scoreDancingCircles([], []).
scoreDancingCircles([R|L], [_{ score: P, dancingCircle: R }|L2]) :-
    scoreDancingCircle(R, P),
    scoreDancingCircles(L, L2).




%% GENERATION OF DANCING CIRCLES
fulfilsRestrictions(Man, Woman, ManA, WomanA) :-
    \+doesNotDanceBetween(Man, ManA, Woman),
    \+doesNotDanceBetween(Woman, WomanA, ManA),
    \+doesNotDanceOnRightSide(Man, Woman),
    \+doesNotDanceOnRightSide(Woman, ManA).

dancingCircle(L, X, X, PreviousMan-_, PreviousWoman-_, L) :-
    !,
    X2 is X << 1,
    X21 is X2 - 1,
    nth1(X2, L, WomanFinal-_),
    nth1(X21, L, ManFinal-_),
    \+doesNotDanceOnRightSide(WomanFinal, PreviousMan),
    \+doesNotDanceBetween(PreviousMan, ManFinal, WomanFinal),
    \+doesNotDanceBetween(PreviousWoman, WomanFinal, PreviousMan).

dancingCircle(R, I, X, ManA-_, WomanA-_, L) :-
    manDancer(Man-M),
    \+member(Man-M, R),
    womanDancer(Woman-F),
    \+member(Woman-F, R),

    fulfilsRestrictions(Man, Woman, ManA, WomanA),

    I1 is I + 1,
    dancingCircle([Man-M, Woman-F|R], I1, X, Man-M, Woman-F, L).

dancingCircle(X, R) :-
    firstMan(Man-M),
    womanDancer(Woman-F),
    fulfilsRestrictions(Man, Woman, wildcard, wildcard),
    dancingCircle([Man-M, Woman-F], 1, X, Man-M, Woman-F, R).

dancingCircles :-
    numMaleDancers(X),
    findall(R, dancingCircle(X, R), L),
    scoreDancingCircles(L, P),
    sort(score, @=<, P, S),
    printDancingCircles(S).

checkDancers :- numMaleDancers(X), numFemaleDancers(X), !.
checkDancers :- write('There is not the same number of man and woman dancers!'), nl, halt.

main :-
    checkDancers,
    dancingCircles,
    halt.
