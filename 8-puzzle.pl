
/*
  Initial:[1,2,3,8,0,4,7,6,5].
  Goal:[1,2,3,4,5,6,7,8,0]
*/

substitute(_,[],_,[]):-!.
substitute(OriginalItem , [H|T] , ReplacedItem , [H|T2]):-
  OriginalItem \= H,
  substitute(OriginalItem , T , ReplacedItem , T2),!.
substitute(OriginalItem , [_|T] , ReplacedItem , [ReplacedItem|T2]):-
  substitute(OriginalItem , T , ReplacedItem , T2).

nthElement(1,[H|_],H).
nthElement(Idx,[_|T],Number):-
  NewIdx is Idx -1,
  nthElement(NewIdx,T,Number).

nth(Idx,List,N):-
  nth2(Idx,0,List,N).

nth2(N,N,[H|_],H):-!.
nth2(Idx,Idx2,[_|T],N):-
  NewIdx2 is Idx2 + 1,
  nth2(Idx,NewIdx2,T,N).

blank_up([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-    % get position of blank cell
  nth(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  Z is N-3,% get element in pos Z
  nth(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  %substitute element of pos Z with blank cell “0”
  substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  substitute(0,Q,R,V),
  substitute(10,V,0,S).

up([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
  R1>0,
  R2>0,
  R3>0,
  blank_up([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

down([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
  R7>0,
  R8>0,
  R9>0,
  blank_down([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

blank_down([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
  nth(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  Z is N+3,
  nth(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  substitute(0,Q,R,V),
  substitute(10,V,0,S).

left([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
  R1>0,
  R4>0,
  R7>0,
  blank_left([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

blank_left([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
  nth(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  Z is N-1,
  nth(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  substitute(0,Q,R,V),
  substitute(10,V,0,S).

right([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew):-
  R3>0,
  R6>0,
  R9>0,
  blank_right([R1,R2,R3,R4,R5,R6,R7,R8,R9],Snew).

blank_right([R1,R2,R3,R4,R5,R6,R7,R8,R9],S):-
  nth(N,[R1,R2,R3,R4,R5,R6,R7,R8,R9],0),
  Z is N+1,
  nth(Z,[R1,R2,R3,R4,R5,R6,R7,R8,R9],R),
  substitute(R,[R1,R2,R3,R4,R5,R6,R7,R8,R9],10,Q),
  substitute(0,Q,R,V),
  substitute(10,V,0,S).

move(S,Snew):-
    up(S,Snew).
move(S,Snew):-
    down(S,Snew).
move(S,Snew):-
    left(S,Snew).
move(S,Snew):-
    right(S,Snew).

go(Start,Goal):-
  path([[Start,null]],[],Goal).

path([],_,_):-
      write('No solution'),nl,!.

path([[Goal,Parent] | _], Closed, Goal):-
      write('A solution is found'), nl ,
      printsolution([Goal,Parent],Closed),!.

path(Open, Closed, Goal):-
      removeFromOpen(Open, [State, Parent], RestOfOpen),
      getchildren(State, Open, Closed, Children),
      addListToOpen(Children , RestOfOpen, NewOpen),
      path(NewOpen, [[State, Parent] | Closed], Goal).

getchildren(State, Open ,Closed , Children):-
      bagof(X, moves( State, Open, Closed, X), Children), ! .

getchildren(_,_,_,[]).

addListToOpen(Children,[],Children).

addListToOpen(L,[H|Open],[H|NewOpen]):-
      addListToOpen(L,Open,NewOpen).

removeFromOpen([State|RestOpen], State, RestOpen).

moves( State, Open, Closed,[Next,State]):-
      move(State,Next),
      \+ member([Next,_],Open),
      \+ member([Next,_],Closed).

printsolution([State, null],_):-
      write(State),write(' <Initial>'),nl,!.

printsolution([State, Parent], Closed):-
      member([Parent, GrandParent], Closed),
      printsolution([Parent, GrandParent], Closed),!,
      write(State),
      getDirection(State,Parent), nl.

getDirection(State,Parent):-
  nth(N,State,0),
  nth(N2,Parent,0),
  N3 is N - N2,
  writeDirection(N3).

writeDirection(N):-
  N =:= -3,
  write(' <Up>').

writeDirection(N):-
  N =:= 3,
  write(' <Down>').

writeDirection(N):-
  N =:= -1,
  write(' <Left>').

writeDirection(N):-
  N =:= 1,
  write(' <Right>').
