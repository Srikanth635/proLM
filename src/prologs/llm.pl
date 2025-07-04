:- module(llm, [llm_query_json/2]).
:- use_module(library(process)).
:- use_module(library(http/json)).

llm_query_json(Prompt, Answer) :-
    process_create(path(python3),
                   ['py_bridge.py', Prompt],
                   [stdout(pipe(Out))]),
    json_read_dict(Out, Dict),
    close(Out),
    Answer = Dict.answer.

