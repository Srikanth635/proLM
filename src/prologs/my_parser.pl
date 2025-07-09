% Required library imports for RDF functionality
:- use_module(library(semweb/rdf11)).
%:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

% Register RDF namespaces
:- rdf_register_prefix(soma, 'http://www.ease-crc.org/ont/SOMA.owl#').
:- rdf_register_prefix(dul, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#').
:- rdf_register_prefix(xsd, 'http://www.w3.org/2001/XMLSchema#').

% Main predicate to parse S-expression string and store as RDF
parse_and_store_sexp(SexpString) :-
    atom_string(SexpAtom, SexpString),
    tokenize_atom(SexpAtom, Tokens),
    parse_sexp(Tokens, ParsedSexp, []),
    store_sexp_action(ParsedSexp).

% Tokenizer - converts atom to list of tokens
tokenize_atom(Atom, Tokens) :-
    atom_chars(Atom, Chars),
    tokenize_chars(Chars, Tokens).

tokenize_chars([], []).
tokenize_chars([' '|Rest], Tokens) :-
    !,
    tokenize_chars(Rest, Tokens).
tokenize_chars(['\t'|Rest], Tokens) :-
    !,
    tokenize_chars(Rest, Tokens).
tokenize_chars(['\n'|Rest], Tokens) :-
    !,
    tokenize_chars(Rest, Tokens).
tokenize_chars(['('|Rest], ['('|Tokens]) :-
    !,
    tokenize_chars(Rest, Tokens).
tokenize_chars([')'|Rest], [')'|Tokens]) :-
    !,
    tokenize_chars(Rest, Tokens).
tokenize_chars(Chars, [Token|Tokens]) :-
    take_word(Chars, WordChars, Rest),
    atom_chars(Token, WordChars),
    tokenize_chars(Rest, Tokens).

% Extract word characters until whitespace or parentheses
take_word([], [], []).
take_word([' '|Rest], [], Rest) :- !.
take_word(['\t'|Rest], [], Rest) :- !.
take_word(['\n'|Rest], [], Rest) :- !.
take_word(['('|Rest], [], ['('|Rest]) :- !.
take_word([')'|Rest], [], [')'|Rest]) :- !.
take_word([C|Rest], [C|WordChars], RestChars) :-
    take_word(Rest, WordChars, RestChars).

% S-expression parser using DCG
parse_sexp(Tokens, Sexp, Rest) :-
    phrase(sexp(Sexp), Tokens, Rest).

sexp(Sexp) --> ['('], sexp_elements(Elements), [')'],
    { Sexp =.. [list|Elements] }.
sexp(Atom) --> [Atom], { \+ member(Atom, ['(', ')']) }.

sexp_elements([]) --> [].
sexp_elements([H|T]) --> sexp(H), sexp_elements(T).

% Store the parsed S-expression as RDF triples
store_sexp_action(ParsedSexp) :-
    Graph = user,
    ParsedSexp =.. [list|Elements],
    extract_action_info(Elements, ActionType, ObjectInfo, SourceInfo),

    % Create action individual
    gensym(action, Action),
    rdf_global_id(dul:'Action', ActionClass),
    rdf_assert(Action, rdf:type, ActionClass, Graph),

    % Set action type
    rdf_current_prefix(soma, SomaPrefixIRI),
    atom_concat(SomaPrefixIRI, ActionType, ActionClassTypeIRI),
    rdf_global_id(soma:'actionType', ActionTypeProp),
    rdf_assert(Action, ActionTypeProp, ActionClassTypeIRI, Graph),

    % Handle object if present
    ( ObjectInfo \= none ->
        create_object_triples(Action, ObjectInfo, Graph)
    ; true
    ),

    % Handle source if present
    ( SourceInfo \= none ->
        create_source_triples(Action, SourceInfo, Graph)
    ; true
    ).

% Extract action type, object info, and source info from parsed elements
extract_action_info(Elements, ActionType, ObjectInfo, SourceInfo) :-
    extract_action_type(Elements, ActionType),
    extract_object_info(Elements, ObjectInfo),
    extract_source_info(Elements, SourceInfo).

extract_action_type(Elements, ActionType) :-
    member(TypeSexp, Elements),
    TypeSexp =.. [list, type, ActionType], !.

extract_object_info(Elements, ObjectInfo) :-
    find_object_sexp(Elements, ObjectSexp),
    ObjectSexp =.. [list, an, object | ObjectElements],
    extract_property_pairs(ObjectElements, ObjectInfo), !.
extract_object_info(_, none).

extract_source_info(Elements, SourceInfo) :-
    find_source_sexp(Elements, SourceSexp),
    SourceSexp =.. [list, source | SourceElements],
    extract_location_properties(SourceElements, SourceInfo), !.
extract_source_info(_, none).

% Find object S-expression in elements
find_object_sexp([Element|_], Element) :-
    Element =.. [list, an, object | _], !.
find_object_sexp([_|Rest], ObjectSexp) :-
    find_object_sexp(Rest, ObjectSexp).

% Find source S-expression in elements
find_source_sexp([Element|_], Element) :-
    Element =.. [list, source | _], !.
find_source_sexp([_|Rest], SourceSexp) :-
    find_source_sexp(Rest, SourceSexp).

% Extract location properties, handling "a location" wrapper
extract_location_properties([LocationSexp|Rest], Properties) :-
    LocationSexp =.. [list, a, location | LocationElements],
    extract_property_pairs(LocationElements, LocationProps),
    extract_property_pairs(Rest, RestProps),
    append(LocationProps, RestProps, Properties), !.
extract_location_properties(Elements, Properties) :-
    extract_property_pairs(Elements, Properties).

% Extract property-value pairs from elements
extract_property_pairs([], []).
extract_property_pairs([PropSexp|Rest], [Prop-Value|Props]) :-
    PropSexp =.. [list, Prop, Value],
    !,
    extract_property_pairs(Rest, Props).
extract_property_pairs([_|Rest], Props) :-
    extract_property_pairs(Rest, Props).

% Create object triples
create_object_triples(Action, ObjectInfo, Graph) :-
    gensym(object, Obj),
    rdf_global_id(soma:'hasObject', HasObjectProp),
    rdf_assert(Action, HasObjectProp, Obj, Graph),
    rdf_global_id(soma:'Object', ObjectClass),
    rdf_assert(Obj, rdf:type, ObjectClass, Graph),

    % Assert object properties
    forall(member(Key-Value, ObjectInfo),
        ( rdf_global_id(soma:Key, Predicate),
          rdf_global_id(soma:'Object', Domain),
          rdf_global_id(xsd:string, Range),
          ensure_property(Predicate, Domain, Range),
          rdf_assert(Obj, Predicate, literal(Value), Graph)
        )).

% Create source/location triples
create_source_triples(Action, SourceInfo, Graph) :-
    gensym(location, Loc),
    rdf_global_id(soma:'hasSource', HasSourceProp),
    rdf_assert(Action, HasSourceProp, Loc, Graph),
    rdf_global_id(soma:'Location', LocClass),
    rdf_assert(Loc, rdf:type, LocClass, Graph),

    % Assert location properties
    forall(member(Key-Value, SourceInfo),
        ( rdf_global_id(soma:Key, Predicate),
          rdf_global_id(soma:'Location', Domain),
          rdf_global_id(xsd:string, Range),
          ensure_property(Predicate, Domain, Range),
          rdf_assert(Loc, Predicate, literal(Value), Graph)
        )).

% Debugging helper to see parsed structure
debug_parse_sexp(SexpString) :-
    atom_string(SexpAtom, SexpString),
    tokenize_atom(SexpAtom, Tokens),
    format('Tokens: ~w~n', [Tokens]),
    parse_sexp(Tokens, ParsedSexp, []),
    format('Parsed: ~w~n', [ParsedSexp]),
    ParsedSexp =.. [list|Elements],
    format('Elements: ~w~n', [Elements]),
    extract_action_info(Elements, ActionType, ObjectInfo, SourceInfo),
    format('ActionType: ~w~n', [ActionType]),
    format('ObjectInfo: ~w~n', [ObjectInfo]),
    format('SourceInfo: ~w~n', [SourceInfo]).

% Simple implementation of ensure_property/3
% This could be expanded to actually check/create property definitions
ensure_property(_, _, _).

% Example usage:
% ?- parse_and_store_sexp("(an action (type PickingUp) (an object (type cooking pan) (material metal) (shape cylindrical) (handle present) (grip textured)) (source (a location (on wooden drawer) (material wood) (size medium) (shape rectangular))))").