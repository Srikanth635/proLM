:- module(cache, [
    cached_llm_query/2,
    list_cached/0,
    save_cache_to_file/1,
    load_cache_from_file/1,
    clear_cache/0,
    find_cached_by_keyword/1,
    inject_cached_fact/1
]).

:- dynamic cached_answer/2.
:- use_module(llm).

%% Main entry point with memoization
cached_llm_query(Prompt, Response) :-
    cached_answer(Prompt, Response), !.
cached_llm_query(Prompt, Response) :-
    llm:llm_query_json(Prompt, Response),
    asserta(cached_answer(Prompt, Response)).

%% List all cached entries
list_cached :-
    forall(cached_answer(Prompt, Response),
           format('~nPrompt: ~w~nResponse: ~w~n', [Prompt, Response])).

%% Save to file
save_cache_to_file(File) :-
    open(File, write, Stream),
    forall(cached_answer(Prompt, Response),
           write_term(Stream, cached_answer(Prompt, Response), [fullstop(true), nl(true), quoted(true)])),
    close(Stream).


%% Load from file
load_cache_from_file(File) :-
    consult(File).

%% Clear the cache (in memory only)
clear_cache :-
    retractall(cached_answer(_, _)).

%% Find by keyword
find_cached_by_keyword(Keyword) :-
    cached_answer(Prompt, Response),
    sub_string(Prompt, _, _, _, Keyword),
    format('~nPrompt: ~w~nResponse: ~w~n', [Prompt, Response]),
    fail.
find_cached_by_keyword(_).


inject_cached_fact(Prompt) :-
    cache:cached_llm_query(Prompt, FactString),
    term_string(Fact, FactString),
    assertz(Fact).

