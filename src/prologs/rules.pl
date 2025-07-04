:- module(rules, [grandparent/2]).
:- use_module(facts).

grandparent(X, Z) :-
    facts:parent(X, Y),
    facts:parent(Y, Z).
