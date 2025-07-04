:- module(triple_facts, [rdf_to_facts/0]).

:- dynamic triple/3.

rdf_to_facts :-
    forall(
        rdf(S, P, O),
        assertz(triple(S, P, O))
    ).

