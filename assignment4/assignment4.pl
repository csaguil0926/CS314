/* YOUR CODE HERE (Problem 1, delete the following line) */
reverseL([],[]).
reverseL([H|T], RevX) :- reverseL(T, RevX1), append(RevX1, [H], RevX).

?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

/* YOUR CODE HERE (Problem 2, delete the following line) */
remove_element(Ele, [], []).
remove_element(Ele, [H|T], Result) :- Ele = H, remove_element(H, T, Result).
remove_element(Ele, [H|T], [H|Result]) :- not(Ele = H), remove_element(Ele, T, Result).

remove_duplicates([],[]).
remove_duplicates([H|T], [H|L2]) :- remove_element(H, T, Nomorehlist), remove_duplicates(Nomorehlist, L2).

?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

/* Your CODE HERE (Problem 3, delete the following line) */
count(Ele, [], 0).
count(Ele, [H|T], N) :- Ele = H, count(Ele,T,M), N is M+1.
count(Ele, [H|T], N) :- not(Ele = H), count(Ele,T,M), N is M.

assoc_list([],[]).
assoc_list([H|T], AL) :- remove_element(H, T, Nomorehlist), assoc_list(Nomorehlist, AL1), count(H, [H|T], Count), append([H-Count], AL1, AL).

?- assoc_list([1], [1-1]).
?- assoc_list([1,1,2,2,2,3,1], [1-3, 2-3, 3-1]).
?- assoc_list([1,1,4,2,2,2,3,1,1,3,1], X).

/* YOUR CODE HERE (Problem 4, delete the following line) */
intersectionL([],_,[]).
intersectionL([H|T], L2, [H|Result]) :- member(H, L2), intersectionL(T, L2, Result).
intersectionL([H|T], L2, Result) :- intersectionL(T, L2, Result).

?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

/* YOUR CODE HERE (Problem 5, delete the following line) */
take([H|T],H,T).
take([H|T],R,[H|S]) :- take(T,R,S).

perm([],[]).
perm(L,[H|T]) :- take(L,H,R), perm(R,T).

sorted([]).
sorted([H]).
sorted([A,B|T]) :- A =< B, sorted([B|T]).

permsort(L,SL) :- perm(L,SL), sorted(SL).

sum([],0).
sum([H | T], N) :- sum(T,M), N is H+M.

prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).

maxL3(L,X) :- length(L, L1), L1 >= 3, permsort(L, SL), reverseL(SL, RSL), prefix(P, RSL), length(P, Pl), Pl is 3, sum(P, X).

?- not(maxL3([1], X)).
?- maxL3([1,2,3,4], 9).
?- maxL3([10,3,2,3,10], X).

/* YOUR CODE HERE (Problem 6, delete the following line) */
partition([],[],[]).
partition([H], [H], []).
partition([H|T], P, S) :- length([H|T], Ll), not(Ll = 1), prefix(P, [H|T]), length(P, Pl), Pl is div(Ll,2), suffix(S, [H|T]), length(S, Sl), Sl is Ll - div(Ll,2).

?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

/* YOUR CODE HERE (Problem 7, delete the following line) */
merge(X,Y,Z) :- append(X,Y,U), permsort(U, Z).

?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

/* YOUR CODE HERE (Problem 8, delete the following line) */
mergesort([],[]).
mergesort([H],[H]).
mergesort(L,SL) :- partition(L, P, S), mergesort(P, SP), mergesort(S, SS), merge(SP, SS, SL).

?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).


segment([], []).
segment(SL, L) :- suffix(SL1, L), prefix(SL, SL1).

?- segment([3,5], [1,2,3,4,5]).
?- segment([X,Y], [1,2,3,4]).
?- segment([3,4,X], [1,2,3,4,5]).
