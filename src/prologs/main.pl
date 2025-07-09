:- use_module(library(semweb/rdf11)).     % RDF + OWL support

:- consult(llm).
:- consult(cache).
:- consult(semwebs).
:- consult(ontology_utils).
:- consult(my_parser).

% Register prefixes
:- rdf_register_prefix(dul, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#').

% Load the actual file and associate with the DUL URI
%:- rdf_load('../ontology/DUL.owl',
%            [ graph(user),
%              register_namespaces(true),
%              base_uri('http://www.ontologydesignpatterns.org/ont/dul/DUL.owl')
%            ]).

:- rdf_retractall(_,_,_,_).

:- rdf_load('../ontology/SOMA.owl', [imports(true), graph(user)]).
%:- rdf_load('up1.owl', [imports(true), graph(user)]).

say_hello :- writeln("Hello from main!").



% Redundant
%:- consult(facts).
%:- consult(rules).
%:- consult(fallback).
%:- consult(inject).
%:- consult(triple_facts).
% Examples:
% ?- resolve_with_llm(parent(alice, bob)).
% ?- cached_llm_query("What's the capital of France?", X).
% ?- inject_fact_from_llm("Add a fact that john is the father of mark.").