:- module(ontology_utils, [ensure_property/3, literal_cleaned_value/2,
                           add_import_header/1, export_ontology/1,
                           show_subclass_axioms/1]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- rdf_register_prefix(soma, 'http://www.ease-crc.org/ont/SOMA.owl#').
:- rdf_register_prefix(xsd,  'http://www.w3.org/2001/XMLSchema#').
:- use_module(library(semweb/rdf_prefixes)).

%% ensure_property(+PropertyIRI, +DomainIRI, +RangeIRI)
%  Ensures the given property is defined in the RDF graph.
ensure_property(P, Domain, Range) :-
    (   rdf(P, rdf:type, rdf:'Property')
    ->  true  % already defined
    ;   rdf_assert(P, rdf:type, rdf:'Property'),
        rdf_assert(P, rdfs:domain, Domain),
        rdf_assert(P, rdfs:range, Range)
    ).


literal_cleaned_value(Value, Cleaned) :-
    (   Value = ^^(Cleaned, _)                  % typed literal
    ;   Value = literal(Cleaned)                % untyped literal
    ;   Value = literal(type(_, Cleaned))       % expanded typed
    ).

add_import_header(Graph) :-
    rdf_create_bnode(Ontology),
    rdf_assert(Ontology, rdf:type, owl:'Ontology', Graph),
    rdf_assert(Ontology, owl:imports, 'http://www.ease-crc.org/ont/SOMA.owl', Graph).

export_ontology(File) :-
    Graph = user,  % or a custom graph if you're using one
    add_import_header(Graph),
    rdf_save(File, [format(xml), graph(Graph)]).


%% show_subclass_axioms(+Class)
%  Lists all rdfs:subClassOf statements for a class,
%  including restrictions (blank nodes).
show_subclass_axioms(Class) :-
    format("SubClass axioms for ~w:~n", [Class]),
    forall(
        rdf(Class, rdfs:subClassOf, Super),
        (
            (   rdf_is_bnode(Super)
            ->  format("  Restriction (blank node): ~w~n", [Super]),
                show_bnode_details(Super)
            ;   format("  Subclass of: ~w~n", [Super])
            )
        )
    ).

%% show_bnode_details(+BNode)
%  Prints the details of a restriction node.
show_bnode_details(BNode) :-
    forall(
        rdf(BNode, P, O),
        format("    ~w ~w~n", [P, O])
    ).