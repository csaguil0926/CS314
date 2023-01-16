:- include(assignment4).

:- begin_tests(reverseL).

test('reverseL1', [true(X == [])]) :- reverseL([],X).
test('reverseL2', [true(X == [3,2,1])]) :- reverseL([1,2,3],X).
test('reverseL3', [true(X == [c,b,a])]) :- reverseL([a,b,c],X).

:- end_tests(reverseL).



:- begin_tests(remove_duplicates).

test('remove_duplicates1', [true(X == [1,2,3,4])]) :- remove_duplicates([1,2,3,4,2,3],X).
test('remove_duplicates2', [true(X == [1, 4, 5, 2, 7, 3])]) :- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
test('remove_duplicates3', [true(X == [ ])]) :- remove_duplicates([ ], X).

:- end_tests(remove_duplicates).



:- begin_tests(assoc_list).

test('assoc_list1', [true]) :- assoc_list([1], [1-1]).
test('assoc_list2', [true]) :- assoc_list([1,1,2,2,2,3,1], [1-3, 2-3, 3-1]).
test('assoc_list3', true(X == [1-5, 2-3, 3-2])) :- assoc_list([1,1,2,2,2,3,1,1,3,1], X).


:- end_tests(assoc_list).



:- begin_tests(intersectionL).

test('intersectionL1', [true]) :- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
test('intersectionL2', [true(X == [1,3])]) :- intersectionL([1,2,3,4],[1,3,5,6],X).
test('intersectionL3', [true]) :- intersectionL([1,2,3],[4,3],[3]).

:- end_tests(intersectionL).



:- begin_tests(maxL3).

test('maxL31', [true]) :- not(maxL3([1], X)).
test('maxL32', [true]) :- maxL3([1,2,3,4], 9).
test('maxL33', true(X == 23)) :- maxL3([10,3,2,3,10], X).

:- end_tests(maxL3).



:- begin_tests(partition).

test('partition1', [true]) :- partition([a],[a],[]).
test('partition2', [true]) :- partition([1,2,3],[1],[2,3]).
test('partition3', [true(X-Y == [a,b]-[c,d])]) :- partition([a,b,c,d],X,Y).

:- end_tests(partition).



:- begin_tests(merge).

test('merge1', [true]) :- merge([],[1],[1]).
test('merge2', [true]) :- merge([1],[],[1]).
test('merge3', [true(X == [1,2,3,4,5,6])]) :- merge([1,3,5],[2,4,6],X).

:- end_tests(merge).



:- begin_tests(mergesort).

test('mergesort1', [true(X == [1,2,3])]) :- mergesort([3,2,1],X).
test('mergesort2', [true(Y == [1,2,3])]) :- mergesort([1,2,3],Y).
test('mergesort3', [true(Z == [ ])]) :- mergesort([],Z).
test('mergesort4', [true(X == [1,2,3,4,5,6])]) :- mergesort([1,3,5,2,4,6],X).

:- end_tests(mergesort).
