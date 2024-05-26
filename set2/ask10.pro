%-- ASKHSH 10

% In case we have just a single pair then we are finished and return true
domino([[A,B]]). 
% There are 2 cases that i can merge 2 dominos
% first is case if first domino has the same last number with the second dominos first number
domino([[H1,T1],[T1,T2]|T]) :- domino([[H1,T2]|T]). 
% second is case if first domino has the same first number with the second dominos last number
domino([[H1,T1],[T2,H1]|T]) :- domino([[T2,T1]|T]).
% last case is that the first 2 dominos do not match
% then move the second domino to last position and continue the algorithm
% best senario is that after some switches we find a match and make the list 
% smaller and smaller until its only one domino and its a valid domino
% worst case the first domino does not have a match with the others which 
% means that the program stays on a infinity loop that switches dominos and crashes from memory
domino([[H1,T1],[H2,T2]|T]) :- appendList([[H1,T1]|T], [H2,T2], L), domino(L).

% simple append item to list
appendList([], L2, [L2]).    
appendList([X | L1], L2, [X | L3]) :- appendList(L1, L2, L3).
