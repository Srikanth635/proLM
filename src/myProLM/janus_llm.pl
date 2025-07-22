:- module(janus_llm, [enquire_designator/3, gen_action_designator/2, movie/4, recommendation_by_director/2]).
:- use_module(library(janus)).
%:- py_setenv('PYTHON', '/home/malineni/envs/hometesting/bin/python').

:- discontiguous janus_llm:name/2.
vocab:-
    writeln("gen_action_designator/2"),
    writeln("enquire_designator/3").

% --- Knowledge Base ---
movie(interstellar, nolan, scifi, 2014).
movie(inception, nolan, scifi, 2010).
movie(the_prestige, nolan, thriller, 2006).
movie(pulp_fiction, tarantino, crime, 1994).
movie(once_upon_a_time, tarantino, drama, 2019).

% --- Recommendation Logic ---
% Recommend another movie by the same director.
recommendation_by_director(LikedMovie, NewMovie) :-
    movie(LikedMovie, Director, _, _), % Find the director of the liked movie
    movie(NewMovie, Director, _, _),   % Find another movie by that same director
    LikedMovie \= NewMovie.            % Ensure it's not the same movie


%% gen_action_designator(+Prompt, -Designator)
%  Sends Prompt to Python gen_action_designator function via janus.
%  Python returns dict containing "cram_plan_response",
%  which we bind to Designator.
gen_action_designator(Prompt, Designator) :-
    py_call('janus_ad_translation':gen_action_designator(Prompt), Dict),
    (   _{cram_plan_response:Designator} :< Dict
    ->  (
            % Store in global variable (fast access)
            nb_setval(last_designator, Designator),

            % Store as asserted fact (backtrackable, queryable)
            retractall(last_designator(_)),
            asserta(last_designator(Designator))

            % Store in KB file (persistent across sessions)
            %(   _{kb_file:KBPath} :< Dict
            %->  append_designator_fact(KBPath, Designator)   % handles append & reload
            %;   true
            %)
        )
    ;   (   _{error:ErrorMsg} :< Dict
        ->  throw(error(python_error(ErrorMsg), _))
        ;   throw(error(cram_plan_response_not_found, _))
        )
    ).

% helper to append the designator fact to a file and reload it
append_designator_fact(KBPath, Designator) :-
    setup_call_cleanup(
        open(KBPath, append, Stream, [encoding(utf8)]),
        format(Stream, 'designator(~q).~n', [Designator]),
        close(Stream)
    ),
    % Force reload so new fact is visible immediately
    load_files(KBPath, [reload(true)]).

% Helper predicates for different access methods
% Fast access via global variable
get_last_designator_fast(Designator) :-
    nb_getval(last_designator, Designator).

% Queryable access via asserted fact
get_last_designator_fact(Designator) :-
    last_designator(Designator).

% Check if designator exists (either method)
has_last_designator :-
    (nb_current(last_designator, _) ; last_designator(_)), !.

% Clear all stored designators
clear_last_designator :-
    (nb_current(last_designator, _) -> nb_delete(last_designator) ; true),
    retractall(last_designator(_)).

% Get designator with fallback priority: global var -> fact -> fail
get_last_designator(Designator) :-
    (   nb_current(last_designator, _)
    ->  nb_getval(last_designator, Designator)
    ;   last_designator(Designator)
    ).

%% enquire_designator(+Designator, +Prompt, -Answer)
% Sends Designator and Prompt to Python enquire_designator function via janus.

enquire_designator(Designator, Prompt, Answer) :-

    Data = _{designator: Designator, prompt: Prompt},

    py_call('janus_ad_translation':enquire_designator(Data), Result),
    (   _{prolog_query:PQuery} :< Result
    ->  writeln(PQuery)
    ;   (   _{error:ErrorMsg} :< Result
        ->  throw(error(python_error(ErrorMsg), _))
        ;   throw(error(prolog_query_not_found, _))
        )
    ),

    writeln(PQuery),

    KB_PATH = "",
    py_call('janus_ad_translation':run_prolog_query(KB_PATH, PQuery), FinalResult),
    (   _{answer:Answer} :< FinalResult
    -> writeln(Answer)
    ; (   _{error:ErrorMsg} :< FinalResult
        ->  throw(error(python_error(ErrorMsg), _))
        ;   throw(error(answer_not_found, _))
        )
    ).

% No longer needed with janus (kept for reference)
% prolog_to_json(Dict, JsonAtom) :-
%     with_output_to(string(S), json_write(current_output, Dict)),
%     atom_codes(JsonAtom, S).


test_gen_action_designator(Prompt, Designator) :-
    py_call('janus_querying':test_gen_action_designator(Prompt), Dict),
    (   _{cram_plan_response:Designator} :< Dict
    ->  (
            % Store in global variable (fast access)
            nb_setval(last_designator, Designator),

            % Store as asserted fact (backtrackable, queryable)
            retractall(last_designator(_)),
            asserta(last_designator(Designator)),

            % Store in KB file (persistent across sessions)
            (   _{kb_file:KBPath} :< Dict
            ->  append_designator_fact(KBPath, Designator)   % handles append & reload
            ;   true
            )
        )
    ;   (   _{error:ErrorMsg} :< Dict
        ->  throw(error(python_error(ErrorMsg), _))
        ;   throw(error(cram_plan_response_not_found, _))
        )
    ).