

%Complete Prolog Code:
% test:-go([4,1,3,7,2,5,0,8,6],[1,2,3,4,5,6,7,8,0]).
%problem specific part
move(S,Snew):-
  	right(S,Snew).

right([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
  	R3>0,
  	R6>0,
  	R9>0,
  	blank_right([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

blank_right([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
  	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  	Z is N+1,
 	  nth0(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  	substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  	substitute(0,Q,R,V),
  	substitute(10,V,0,S).

move(S,Snew):-
  	left(S,Snew).

left([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
  	R1>0,
  	R4>0,
  	R7>0,
  	blank_left([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

blank_left([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
  	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  	Z is N-1,
  	nth0(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  	substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  	substitute(0,Q,R,V),
  	substitute(10,V,0,S).

move(S,Snew):-
  	down(S,Snew).

down([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
  	R7>0,
  	R8>0,
  	R9>0,
  	blank_down([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

blank_down([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
  	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  	Z is N+3,
  	nth0(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  	substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  	substitute(0,Q,R,V),
  	substitute(10,V,0,S).

move(S,Snew):-
  	up(S,Snew).

up([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
  	R1>0,
  	R2>0,
  	R3>0,
  	blank_up([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

blank_up([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
    % get position of blank cell
  	nth0(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  	Z is N-3,
      % get element in pos Z
  	nth0(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
    %substitute element of pos Z with blank cell “0”
  	substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  	substitute(0,Q,R,V),
  	substitute(10,V,0,S).

substitute(_, [], _, []):-!.
substitute(X, [X|T], Y, [Y|T1]):-
	substitute(X, T, Y, T1),!.
substitute(X, [Y|T], Y, [X|T1]):-
	substitute(X, T, Y, T1),!.
substitute(X, [H|T], Y, [H|T1]):-
	substitute(X, T, Y, T1).
% end of specific part


%general algorithm
go(Start,Goal):-
		getHeuristic(Start, H, Goal),
		path([[Start,null, 0, H, H]],[],Goal).
    %open, closed, goal, path_cost, heuristic, total cost


path([], _, _):-
		write('No solution'),nl,!.

path(Open, Closed, Goal):-
		getBestChild(Open, [Goal, Parent, PC, H, TC], RestOfOpen),
		write('A solution is found'),  nl ,
		printsolution([Goal,Parent, PC, H, TC], Closed),!.

path(Open, Closed, Goal):-
		getBestChild(Open, [State, Parent, PC, H, TC], RestOfOpen),
		%write('Best child chosen is '),write(State),write(' with TC= '),write(TC), nl,
		getchildren(State, Open, Closed, Children, PC, Goal),
		addListToOpen(Children , RestOfOpen, NewOpen),
		path(NewOpen, [[State, Parent, PC, H, TC] | Closed], Goal).

getchildren(State, Open ,Closed , Children, PC, Goal):-
		bagof(X, moves( State, Open, Closed, X, PC, Goal), Children) .
getchildren(_,_,_, [],_,_).

addListToOpen(Children, [], Children).
addListToOpen(Children, [H|Open], [H|NewOpen]):-
		addListToOpen(Children, Open, NewOpen).

getBestChild([Child], Child, []).
getBestChild(Open, Best, RestOpen):-
	getBestChild1(Open, Best),
	removeFromList(Best, Open, RestOpen).

getBestChild1([State], State):-!.
getBestChild1([State|Rest], Best):-
	getBestChild1(Rest, Temp),
	getBest(State, Temp, Best).

getBest([State, Parent, PC, H, TC], [_, _, _, _, TC1], [State, Parent, PC, H, TC]):-
	TC < TC1, !.
getBest([_, _, _, _, _], [State1, Parent1, PC1, H1, TC1], [State1, Parent1, PC1, H1, TC1]).

removeFromList(_, [], []):-!.
removeFromList(H, [H|T], V):-
	!, removeFromList(H, T, V).
removeFromList(H, [H1|T], [H1|T1]):-
	removeFromList(H, T, T1).

moves( State, Open, Closed,[Next,State, NPC, H, TC], PC, Goal):-
		move(State,Next),
		\+ member([Next, _, _, _, _],Open),
		\+ member([Next, _, _, _, _],Closed),
		NPC is PC + 1,
		getHeuristic(Next, H, Goal),
		TC is NPC + H.

getHeuristic([], 0, []):-!.
getHeuristic([H|T1],V,[H|T2]):-!,
	getHeuristic(T1,V, T2).

getHeuristic([_|T1],H,[_|T2]):-
	getHeuristic(T1,TH, T2),
	H is TH + 1.

printsolution([State, null, PC, H, TC],_):-!,
		write(State), write(' PC: '), write(PC), write(' H:'), write(H), write(' TC: '), write(TC), nl.
printsolution([State, Parent, PC, H, TC], Closed):-
		member([Parent, GrandParent, PC1, H1, TC1], Closed),
		printsolution([Parent, GrandParent, PC1, H1, TC1], Closed),
		write(State), write(' PC: '), write(PC), write(' H:'), write(H), write(' TC: '), write(TC), nl.
