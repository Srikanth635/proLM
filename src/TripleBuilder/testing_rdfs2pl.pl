:- ensure_loaded( library(rdfs2pl) ).
:- use_module(library(semweb/rdf_db)).

gen :-
        rdf_load('SOMA.owl'),
        tell('SOMA.pl'),
        write_schema(soma,'http://www.ease-crc.org/ont/SOMA.owl#',[use_labels(true)]),
        told.