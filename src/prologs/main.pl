:- consult(facts).
:- consult(rules).
:- consult(llm).
:- consult(cache).
:- consult(fallback).
:- consult(inject).
:- consult(semwebs).
:- consult(triple_facts).
:- consult(ontology_utils).

% Examples:
% ?- resolve_with_llm(parent(alice, bob)).
% ?- cached_llm_query("What's the capital of France?", X).
% ?- inject_fact_from_llm("Add a fact that john is the father of mark.").


say_hello :- writeln("Hello from main!").
