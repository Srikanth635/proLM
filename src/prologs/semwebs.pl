:- module(action_to_rdf, [parse_and_store_action/1]).

:- use_module(library(semweb/rdf11)).     % RDF + OWL support
:- use_module(library(semweb/rdfs)).      % RDFS/OWL reasoning
:- use_module(library(http/json)).        % JSON parsing
:- use_module(library(gensym)).           % Unique node generation
:- use_module(ontology_utils).            % For ensure_property/3

% Register ontology prefixes
:- rdf_register_prefix(rdf,  'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_prefix(owl,  'http://www.w3.org/2002/07/owl#').
:- rdf_register_prefix(xsd,  'http://www.w3.org/2001/XMLSchema#').
:- rdf_register_prefix(foaf, 'http://xmlns.com/foaf/0.1/').
:- rdf_register_prefix(soma, 'http://www.ease-crc.org/ont/SOMA.owl#').
:- rdf_register_prefix(dul, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#').

%% parse_and_store_action(+JSONString:atom)
% Parses a JSON string representing an action and asserts RDF triples into the user graph.
parse_and_store_action(JSONString) :-
    atom_json_dict(JSONString, Dict, []),
    store_action_dict(Dict).

store_action_dict(Dict) :-
    Graph = user,

    % Create a new Action individual
    gensym(action, Action),
    atom_string(ActionType, Dict.action),
    rdf_global_id(soma:'Action', ActionClass),
    rdf_assert(Action, rdf:type, ActionClass, Graph),
    rdf_global_id(soma:'actionType', ActionTypeProp),
    rdf_assert(Action, ActionTypeProp, literal(ActionType), Graph),

    % Handle object
    (   get_dict(object, Dict, ObjDict)
    ->  gensym(object, Obj),
        rdf_global_id(soma:'hasObject', HasObjectProp),
        rdf_assert(Action, HasObjectProp, Obj, Graph),
        rdf_global_id(soma:'Object', ObjectClass),
        rdf_assert(Obj, rdf:type, ObjectClass, Graph),
        dict_pairs(ObjDict, _, ObjPairs),
        forall(member(Key-Value, ObjPairs),
               (   atom_string(AtomValue, Value),
                   rdf_global_id(soma:Key, Predicate),
                   rdf_global_id(soma:'Object', Domain),
                   rdf_global_id(xsd:string, Range),
                   ensure_property(Predicate, Domain, Range),
                   rdf_assert(Obj, Predicate, literal(AtomValue), Graph)
               ))
    ;   true
    ),

    % Handle source
    (   get_dict(source, Dict, SrcDict)
    ->  gensym(location, Loc),
        rdf_global_id(soma:'hasSource', HasSourceProp),
        rdf_assert(Action, HasSourceProp, Loc, Graph),
        rdf_global_id(soma:'Location', LocClass),
        rdf_assert(Loc, rdf:type, LocClass, Graph),
        dict_pairs(SrcDict, _, SrcPairs),
        forall(member(Key-Value, SrcPairs),
               (   atom_string(AtomValue, Value),
                   rdf_global_id(soma:Key, Predicate),
                   rdf_global_id(soma:'Location', Domain),
                   rdf_global_id(xsd:string, Range),
                   ensure_property(Predicate, Domain, Range),
                   rdf_assert(Loc, Predicate, literal(AtomValue), Graph)
               ))
    ;   true
    ).

