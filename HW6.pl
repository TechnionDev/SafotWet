% Constraint Logic Programming
:- use_module(library(dif)).	% Sound inequality
:- use_module(library(clpfd)).	% Finite domain constraints
:- use_module(library(clpb)).	% Boolean constraints
:- use_module(library(chr)).	% Constraint Handling Rules
:- use_module(library(when)).	% Coroutining
%:- use_module(library(clpq)).  % Constraints over rational numbers

% Your program goes here
/**Q1.1*/
card(number,suit).
card(X,Y):- X #>= 2, X #=< 14, (Y ='clubs';Y ='hearts';Y ='spades';Y ='diamonds').
x_smaller_y(X,Y):- 
    Y='diamonds';
    (Y='spades',(X='clubs';X='hearts';X='spades'));
    (Y='hearts',(X='clubs';X='hearts'));
    (Y='clubs',X='clubs').

find_smallest([], card(H_X,H_Y),Res):-
    Res = card(H_X,H_Y).

find_smallest([card(X,Y)|XS], card(H_X,_),Res) :- 
    X<H_X,
    find_smallest(XS, card(X,Y),Res).

find_smallest([card(X,_)|XS], card(H_X,H_Y),Res) :- 
    X>H_X,
    find_smallest(XS, card(H_X,H_Y),Res).

find_smallest([card(X,Y)|XS], card(H_X,H_Y),Res) :- 
    X=H_X,
	x_smaller_y(Y,H_Y),
    find_smallest(XS, card(X,Y),Res).

find_smallest([card(X,Y)|XS], card(H_X,H_Y),Res) :- 
    X=H_X,
	x_smaller_y(H_Y,Y),
    find_smallest(XS, card(H_X,H_Y),Res).
lowest([],Res) :-
    Res = card(14,diamonds).
lowest(X,Res) :-
    find_smallest(X, card(14,diamonds),Res).


/**Q1.2*/

bigger(card(X,_),card(H_X,_)) :-
    H_X>X.
bigger(card(X,Y),card(H_X,H_Y)) :-
    H_X=X,
    x_smaller_y(Y,H_Y).

filter_t(_,[],[]).

filter_t(card(P1,P2),[X|Xs],[X|Ys]) :-
    bigger(card(P1,P2),X),
    filter_t(card(P1,P2),Xs, Ys).

filter_t(card(P1,P2),[_|Xs],Ys) :-
    filter_t(card(P1,P2),Xs, Ys).

filter(card(P1,P2),[H|T],Res) :- 
    filter_t(card(P1,P2),[H|T],Res).

/**Q1.3*/
x_smaller_y_w(X,Y):- 
    Y='diamonds';
    (Y='spades',(X='clubs';X='hearts';X='spades'));
    (Y='hearts',(X='clubs';X='hearts'));
    (Y='clubs',X='clubs').

bigger_winner(card(X,_),card(Y,_),Res) :-
    X>Y,
    Res = 2. 
bigger_winner(card(X,_),card(Y,_),Res) :-
    X<Y,
    Res = 1.
bigger_winner(card(X,XS),card(Y,YS),Res) :-
    Y=X,
    XS = YS,
    Res = 0.
bigger_winner(card(X,XS),card(Y,YS),Res) :-
    Y=X,
    x_smaller_y_w(XS,YS),
    Res = 1.
bigger_winner(card(X,_),card(Y,_),Res) :-
    Y=X,
    Res = 2.

winner([X|XS],[Y|YS],card(P1,P2),Res) :-
    filter(card(P1,P2),[X|XS],XRes),
    filter(card(P1,P2),[Y|YS],YRes),
    lowest(XRes,Xlowest),
    lowest(YRes,Ylowest),
    bigger_winner(Xlowest,Ylowest,Res).

/**Q2.1*/

replace_t(Head,[_|Tail],N,New,Res) :-
    N #= 0,
    append(Head, [New], T),
    append(T,Tail,Res).

replace_t(Head,[X|Tail], N, New, Res) :-
    N#>0,
    append(Head,[X],T),
    replace_t(T,Tail,N-1,New,Res).

replace([_|Tail], N, New, Res) :-
    N#=0,
    Res=[New|Tail].

replace([L|Tail], N, New, Res) :-
    N#>0,
    replace_t([L], Tail,N-1,New,Res).


/**Q2.2*/

turing(State, Actions, tape(Data, Index), Res) :-
    replace(Data, Index, X, Data),
    replace(Actions, _, action(State, _, X, Y, h), Actions),
    replace(Data, Index, Y, NData),
    Index #= N,
    Res=tape(NData, N).

turing(State, Actions, tape(Data, Index), Res) :-
    replace(Data, Index, X, Data),
    replace(Actions, _, action(State, NState, X, Y, l), Actions),
    replace(Data, Index, Y, NData),
    turing(NState, Actions, tape(NData, Index-1), Res).

turing(State, Actions, tape(Data, Index), Res) :-
    replace(Data, Index, X, Data),
    replace(Actions, _, action(State, NState, X, Y, r), Actions),
    replace(Data, Index, Y, NData),
    turing(NState, Actions, tape(NData, Index+1), Res).

/**Queries*/
