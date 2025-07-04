:- module(fallback, [resolve_with_llm/1]).
:- use_module(llm).
:- use_module(library(readutil)).
:- meta_predicate resolve_with_llm(0).

resolve_with_llm(Goal) :-
    call(Goal), !.
resolve_with_llm(Goal) :-
    term_string(Goal, GoalStr),
    load_code_context("facts.pl", Facts),
    load_code_context("rules.pl", Rules),
    format(string(Prompt),
           "Given the following Prolog facts and rules:\n\nFacts:\n~s\n\nRules:\n~s\n\nWhat is the result of the goal: ~w?\nRespond with JSON: {\"answer\": <your answer>}",
           [Facts, Rules, GoalStr]),
    llm:llm_query_json(Prompt, Answer),
    format('LLM suggestion for ~w: ~w~n', [Goal, Answer]).

load_code_context(File, Content) :-
    read_file_to_string(File, Content, []).

