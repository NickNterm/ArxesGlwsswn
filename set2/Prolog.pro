
%-----------------------------------------------------------------------------------------

%-- ASKHSH 1

% Just true if the C1 == C2
q1(C1,C2) :- C1 == C2.
% else if there is an event with the same date and different cities
q1(C1,C2) :- event(_,C1,D), event(_,C2,D).

% get the A1,B1 and A2,B2 of the two suspects and check if they have any common dates
% We may miss some edge cases cause we may need to use =< and >= instead of < and >.
% This is not 100% but tests work
q2(X,Y,C) :- at(X, A1, B1, C), at(Y, A2, B2, C), A1 < B2 , A2 < B1, dif(X, Y). 

% get the three events and the three cities. Then check if the three events are different and then the three cities are on the same country
% then i use absdif to check if the difference between the dates is less than 7 days
q3(S) :- event(E1,C1,D1), event(E2,C2,D2), event(E3,C3,D3), dif(E1,E2), dif(E2,E3), dif(E1,E3), country(C1,S), country(C2,S), country(C3,S), absdif(D1,D2,A), absdif(D2,D3,B), absdif(D1,D3,C), A<7, B<7, C<7.

% in case of A<B then C is B-A else C is A-B
absdif(A,B,C) :- A<B, C is B-A.
absdif(A,B,C) :- A>=B, C is A-B.

% get suspect C and 2 cities that he was on. Then check the countries that are different. Then check the events in that city that the suspect was on
% last compare the dates of the event and the dates that the suspect was in the city
q4(X) :- at(X,A1,B1,C1), at(X,A2,B2,C2), country(C1,S1),country(C2,S2),dif(S1,S2),event(E1,C1,D1),event(E2,C2,D2), D1=<B1,D1>=A1, D2=<B2,D2>=A2.

%-----------------------------------------------------------------------------------------

%-- ASKHSH 2

% In case of A==B that means I reached where i wanted to go. If N>=0 that means
% that the budget that i have at the end also was enough
p(A,B,N) :- A == B, N >= 0 .

% I always check if N > 0 so that i dont continue with negative budget
% those 3 lines split and run in "parallel" and are the three ways of transportation
% in case one fails its fine we are on a dead end. 
% I travel from A to a city C and I get the cost. I subtract the cost from the budget 
% and then i continue to the next city
p(A,B,N) :- N > 0, plane(A,C,NP), NNP is N - NP, p(C,B,NNP).
p(A,B,N) :- N > 0, train(A,C,NT), NNT is N - NT, p(C,B,NNT).
p(A,B,N) :- N > 0, boat(A,C,NB), NNB is N - NB, p(C,B,NNB).
p(A,B,N) :- fail.

% q just uses the qp and qtb.
% qp is just travel with plane and then the cost
% then the qtb travels with train and boat but 
% the budget is 80% of the one from the plane
q(A,B) :- qp(A,B,NP), qtb(A,B,(NP * 0.8)). 

% travel by plane and get the cost
qp(A,B,N) :- plane(A,B,N).

% travel by train and boat and get the cost
% if A == B then we reached our destination
qtb(A,B,N) :- A == B, N >= 0.
% if N > 0 then we can continue
% if we can travel by train then we continue with the new budget
% if we can travel by boat then we continue with the new budget
qtb(A,B,N) :- N > 0, train(A,C1,NT), NNT is N - NT, qtb(C1,B,NNT).
qtb(A,B,N) :- N > 0, boat(A,C2,NB), NNB is N - NB, qtb(C2,B,NNB).

r(A,B,N) :- fail.

%-----------------------------------------------------------------------------------------

%-- ASKHSH 3

% GCD alg
% if Y == 0 then the GCD is X
gcd(X,Y,GCD) :- Y = 0, GCD is X.
% else we get the mod of X and Y and then we call the function with Y and the mod
gcd(X,Y,GCD) :- Y > 0, M is X mod Y, gcd(Y,M,GCD).

% simplify the fraction
% get the fraction on X/Y format. get the GCD of X,Y.
% then divide X and Y with the GCD SX/SY is the new fraction
simplify(Z1, Z) :-
    Z1 = X/Y,
    gcd(X, Y, GCD),
    SX is X // GCD,
    SY is Y // GCD,
    Z = SX/SY.

% add the fractions
% get the fractions on X/Y and Y/Z format
% get the sum of those fractions
% then simplify the sum
fracSum(X,Y,Z) :- X = X1/X2, Y = Y1/Y2, AR is Y2*X1 + X2*Y1, PAR is X2*Y2, Z1 = AR/PAR, simplify(Z1,Z).

%-----------------------------------------------------------------------------------------

%-- ASKHSH 4

bipolarDivisor(1,D) :- D is 1. 
bipolarDivisor(N,D) :- isPrime(N) -> D is N ; (maxPrimeDivisor(N,A), minPrimeDivisor(N,B),D is A*B).

% create a IsPrime function
isPrime(2).
isPrime(3).
isPrime(P) :-
    P > 3,
    P mod 2 =\= 0,   
    \+ hasDivisor(P, 3).  

% Helper for the has divisor
hasDivisor(N, D) :-
    D * D =< N,
    (N mod D =:= 0 ;
    ND is D + 2,
    hasDivisor(N, ND)).

% get the min prime divisor
minPrimeDivisor(N,M) :-
    minPrimeDivisor(N, 2, M).
minPrimeDivisor(N, D, M) :-
    D * D =< N,
    (   N mod D =:= 0,
        isPrime(D) ->
        M = D
    ;   ND is D + 1,
        minPrimeDivisor(N, ND, M)
    ).

% get the Max Prime Divisor
maxPrimeDivisor(N,M):-
    maxPrimeDivisor(N,N-1,M).
maxPrimeDivisor(N,D,M):-
    D * D >= sqrt(N),
    (   N mod D =:= 0,
        isPrime(D) ->
        M=D
        ; PD is D - 1,
        maxPrimeDivisor(N,PD, M)
    ).


%-----------------------------------------------------------------------------------------

%-- ASKHSH 5

% easy solutions 
% if the you divide 0 with something then the result is 0
divide(0,_,0).
% here if you divide something with 0 
% then the result is undefined. If you divide 0 with something
divide(_,0,undefined).

% else we use recursion to divide the two numbers
divide(X,Y,D) :- divideHelper(X,Y,0,D). 


% smart idea of the day here is that you "remove" the s() from the X and Y
divideHelper(s(X),s(Y),C,D) :- divideHelper(X,Y,s(C),D).
% when Y is 0 that means all the s() are left. so in practice you
% "can fit" the Y in X at least one time.
% so you just reset Y and try to fill it another time
divideHelper(X,0,C,s(D)) :- divideHelper(X,C,0,D).
% if X becomes 0 then you just finished and return 0
% then the recursion take over and adds the s() to the result
% as many times as it needed
divideHelper(0,Y,X,0).

% ex. you say i have 8 / 3. I try to fit 3 in 8
% i go 7,3 then 6,2 then 5,1 then 4,0 then i add one result and refill the 3

%-----------------------------------------------------------------------------------------

%-- ASKHSH 6

% keep the length of the starting array on LE
% then use a helper
majority(L,X) :- length(L,LE), majorityHelper(L,X,0,LE). 

% if the list is empty then fail
majorityHelper([],X,C,LE) :- fail.
% just get the head and count the number of elements. if the number is bigger than 
% le/2 then return this number
majorityHelper([H|T],X,C,LE) :- numOfElem([H|T],H,C1), C1 > (LE / 2), X = H.
% else just continue with the rest of the list and keep LE the same as in the initial array
majorityHelper([H|T],X,C,LE) :- numOfElem([H|T],H,C1), C1 =< (LE / 2), majorityHelper(T,X,C,LE).

% returns the number of elements in that list
numOfElem([],_,0).
numOfElem([H|T],X,C) :- H == X, numOfElem(T,X,C1), C is C1+1.
numOfElem([H|T],X,C) :- H \== X, numOfElem(T,X,C).



%-----------------------------------------------------------------------------------------

%-- ASKHSH 7

freq([H|T],S) :- freqHelper([H|T], S, []).

% just a helper to have and empty list as a buffer
freqHelper([],S,B) :- S = B.
% I use the empty list as starting point to start adding things inside.
% so starts as [] and then for ex. [1*a] then [2*a] then [2*a,1*b] and so on
freqHelper([H|T],S,B) :- add(H,B,R,[]), freqHelper(T,S,R).

% Code that adds an element to a list
% If the list is empty, then just add the value to the list
% Here cause its recursive keep the 
% F(a buffer for things that were on the main list)
% and add it to the final result R
add(X,[],R,F) :- V = 1*X , R = [V|F].
% in case the head is the same as the element that we want to increment
% and merge the R and F list and this is the result
add(X,[N*X|T],R,F) :- N1 is N+1, V = N1*X, merge([V|T],F,R).
% in this case we just continue but on F we keep the head to
% add it later to the final result
add(X,[H|T],R,F) :- add(X,T,R,[H|F]).

% Code that merges two lists.
% Hippity Hoppity this code from Prolog-2015.pdf is now my property
merge([],L,L).
merge(L,[],L).
merge([H1|T1],[H2|T2],[H1|T]) :- H1 @=< H2, merge(T1,[H2|T2],T).
merge([H1|T1],[H2|T2],[H2|T]) :- H1 @> H2, merge([H1|T1],T2,T).



%-----------------------------------------------------------------------------------------

%-- ASKHSH 8

% Use Helper function to get the index of the elements that we want to pick
pick(L,I,S) :- pickHelper(L,I,S,1). 

% in case the list of items to select is empty just say that the S is empty
pickHelper(L,[],S,C) :- S = [].
% Check if the index is the same as the counter. If yes then add the element to the list
% then continue with the rest of the list but on the end just add this H item to the S list
pickHelper([H|T],[HI|TI],S,C) :- HI =:= C, C1 is C+1, pickHelper(T,TI,W,C1), S = [H|W].
% if the index is less than the count that means the list is not in order
pickHelper(L,[HI|TI],S,C) :- HI < C, fail.
% if the index is greater than the count then we just continue with the rest of the list
pickHelper([H|T],[HI|TI],S,C) :- HI \= C, C1 is C+1, pickHelper(T,[HI|TI],S,C1).

%-----------------------------------------------------------------------------------------

%-- ASKHSH 9

% use the helper to calculate the count
count(L,C) :- countHelper(L,0,1,1,C). 

% Helper has I,J which are for the 2 for loops to check all elements together
% R stands for the result that we want to return
% C is the increment
% if J is greater than the length of the list then we return the count
% So here we increment the second for loop
countHelper(L,C,I,J,R) :- length(L,N), J > N, R is C.
% if I is greater than the length of the list then we continue with the next element
% So here we increment the first for loop
countHelper(L,C,I,J,R) :- length(L,N), I > N, NJ = J + 1, countHelper(L,C,1,NJ,R).
% I use elemList to pick elements with indexes I and J. Then i check if they are greater than 2 times the other element
% and i continue with the resursion
countHelper(L,C,I,J,R) :- elemList(I,L,N1), elemList(J,L,N2), NI is I + 1, N1 > N2 * 2, countHelper(L,C+1,NI,J,R) .
countHelper(L,C,I,J,R) :- elemList(I,L,N1), elemList(J,L,N2), NI is I + 1, N1 =< N2 * 2, countHelper(L,C,NI,J,R).

% get the length of the list
length([],0).
length([H|T],N) :- length(T,N1), N is N1+1.

% get the element of the list with the index
elemList(1,[H|T],H).
elemList(N,[H|T],R) :- N1 is N-1, elemList(N1,T,R).

%-----------------------------------------------------------------------------------------

%-- ASKHSH 10

domino(S) :- dominoHelper(S,0).

% In case we have just a single pair then we are finished and return true
dominoHelper([[A,B]], C). 
% There are 2 cases that i can merge 2 dominos
% first is case if first domino has the same last number with the second dominos first number
dominoHelper([D1,D2|T], C) :- getLast(D1,T1), getFirst(D2,H2), T1 == H2, getFirst(D1,H1), getLast(D2,T2), dominoHelper([[H1,T2]|T], 0). 
% second is case if first domino has the same first number with the second dominos last number
dominoHelper([D1,D2|T], C) :- getLast(D2,T2), getFirst(D1,H1), T2 == H1, getFirst(D2,H2), getLast(D1,T1), dominoHelper([[H2,T1]|T], 0). 
% last case is that the first 2 dominos do not match
% then move the second domino to last position and continue the algorithm
% best senario is that after some switches we find a match and make the list 
% smaller and smaller until its only one domino and its a valid domino
% worst case the first domino does not have a match with the others which 
% means that the program stays on a infinity loop that switches dominos and crashes from memory
dominoHelper([[H1,T1],[H2,T2]|T], C) :- length([[H1,T1],[H2,T2]|T], LE), C =< LE, appendList([[H1,T1]|T], [H2,T2], L), NC is C + 1, dominoHelper(L, NC).

% simple append item to list
appendList([], L2, [L2]).    
appendList([X | L1], L2, [X | L3]) :- appendList(L1, L2, L3).

getLast([X],X).
getLast([H|T],X) :- getLast(T,X).

getFirst([X],X).
getFirst([H|T],X) :- X = H.

%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

%-- MHN TROPOPOIHSETE TO PARAKATW TMHMA KWDIKA 



dif(X,Y) :- X \= Y.

at(tiger,1,14,'Berlin').
at(tiger,15,15,'Hamburg').
at(tiger,16,37,'Athens').
at(tiger,38,50,'Rome').
at(wolf,1,4,'Rome').
at(wolf,5,5,'London').
at(wolf,6,7,'Rome').
at(wolf,8,8,'Rotterdam').
at(wolf,9,24,'Rome').
at(wolf,25,25,'Athens').
at(wolf,26,30,'Rome').
at(wolf,31,40,'Paris').
at(wolf,41,41,'Rome').
at(wolf,42,42,'Barcelona').
at(wolf,43,50,'Rome').
at(hawk,1,10,'Berlin').
at(hawk,11,20,'Stuttgart').
at(hawk,21,35,'Hamburg').
at(hawk,36,50,'Frankfurt').
at(shark,1,16,'Amsterdam').
at(shark,17,20,'London').
at(shark,21,29,'Paris').
at(shark,30,45,'Rome').
at(shark,43,48,'Brussels').
at(shark,49,50,'London').
at(spider,1,12,'Brussels').
at(spider,13,17,'Berlin').
at(spider,18,50,'Brussels').
at(snake,1,10,'Rome').
at(snake,11,20,'Milan').
at(snake,21,50,'Berlin').

event(e001,'Berlin',2).
event(e002,'Madrid',3).
event(e003,'London',5).
event(e004,'Rome',7).
event(e005,'Bristol',10).
event(e006,'Stuttgart',13).
event(e007,'Milan',17).
event(e008,'Amsterdam',17).
event(e009,'Rotterdam',18).
event(e010,'Hamburg',24).
event(e011,'Amsterdam',24).
event(e012,'Athens',25).
event(e013,'Groningen',25).
event(e014,'Paris',31).
event(e015,'Strasbourg',31).
event(e016,'Paris',37).
event(e017,'Brussels',40).
event(e018,'Brussels',41).
event(e019,'Barcelona',42).
event(e020,'Frankfurt',43).
event(e021,'Brussels',43).
event(e022,'London',47).

country('Amsterdam','Netherlands').
country('Athens','Greece').
country('Barcelona','Spain').
country('Berlin','Germany').
country('Bristol','United Kingdom').
country('Brussels','Belgium').
country('Frankfurt','Germany').
country('Groningen','Netherlands').
country('Hamburg','Germany').
country('London','United Kingdom').
country('Madrid','Spain').
country('Milan','Italy').
country('Paris','France').
country('Rome','Italy').
country('Rotterdam','Netherlands').
country('Strasbourg','France').
country('Stuttgart','Germany').



train(X,Y,N) :- train___(X,Y,N). 
train(X,Y,N) :- train___(Y,X,N). 

train___(astralCity,frozenTown,5).
train___(astralCity,greenTown,24).
train___(astralCity,rainyPort,15).
train___(brightCity,kindTown,21).
train___(crazyCity,vainPort,5).
train___(crazyCity,zeroTown,7).
train___(dreamCity,oldTown,3).
train___(dreamCity,newTown,4).
train___(dreamCity,timePort,6).
train___(eternalCity,mysteryTown,4).
train___(frozenTown,greenTown,27).
train___(greenTown,honeyTown,11).
train___(greenTown,icyTown,12).
train___(honeyTown,joyTown,10).
train___(honeyTown,piratesPort,9).
train___(icyTown,joyTown,15).
train___(joyTown,kindTown,18).
train___(joyTown,sunnyPort,20).
train___(kindTown,quietPort,25).
train___(luckyTown,mysteryTown,5).
train___(luckyTown,utopiaPort,7).
train___(oldTown,timePort,5).
train___(whitePort,xenonTown,7).
train___(whitePort,yellowTown,8).


boat(X,Y,N) :- boat___(X,Y,N). 
boat(X,Y,N) :- boat___(Y,X,N). 

boat___(piratesPort,whitePort,45).
boat___(quietPort,whitePort,42).
boat___(quietPort,utopiaPort,75).
boat___(rainyPort,quietPort,58).
boat___(rainyPort,vainPort,38).
boat___(rainyPort,whitePort,48).
boat___(sunnyPort,timePort,30).
boat___(sunnyPort,utopiaPort,32).
boat___(timePort,vainPort,87).
boat___(vainPort,whitePort,53).


plane(X,Y,N) :- plane___(X,Y,N). 
plane(X,Y,N) :- plane___(Y,X,N). 

plane___(astralCity,brightCity,120).
plane___(astralCity,crazyCity,80).
plane___(astralCity,dreamCity,90).
plane___(astralCity,eternalCity,150).
plane___(brightCity,crazyCity,140).
plane___(brightCity,dreamCity,110).
plane___(brightCity,eternalCity,70).
plane___(dreamCity,eternalCity,107).

