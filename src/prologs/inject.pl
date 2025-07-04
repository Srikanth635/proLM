:- module(inject, [inject_fact_from_llm/1]).
:- use_module(llm).
:- use_module(library(http/json)).
:- dynamic capital/2.  % declare expected predicate for dynamic injection

%% inject_fact_from_llm(+Prompt)
inject_fact_from_llm(Prompt) :-
    format(string(LLMPrompt),
           "Convert the following instruction into a valid Prolog fact.\nRespond with only the fact and a period, no explanations.\nUse only lowercase atoms. Allowed predicate: capital/2.\nInstruction: ~s",
           [Prompt]),

    % Call LLM, get JSON string with {"answer": "..."}
    llm:llm_query_json(LLMPrompt, JsonString),

    % Parse JSON and extract "answer" field
    atom_json_dict(JsonString, Dict, []),
    FactText = Dict.answer,

    % Convert string to Prolog term
    catch(term_string(Fact, FactText),
          _Error,
          (format("⚠️ Invalid fact syntax: ~w~n", [FactText]), fail)),

    % Validate allowed structure (only capital/2)
    ( Fact =.. [capital, _, _] ->
        % Check for duplicates
        ( call(Fact) ->
            format("⚠️ Fact already exists: ~w~n", [Fact])
        ;
            % Inject and log
            assertz(Fact),
            log_fact_to_file(Fact),
            format("✅ Injected fact: ~w~n", [Fact])
        )
    ;
        format("❌ Rejected fact: ~w (only capital/2 is allowed)~n", [Fact]),
        fail
    ).

%% log_fact_to_file(+Fact)
log_fact_to_file(Fact) :-
    open('injected_facts.pl', append, Stream),
    write_term(Stream, Fact, [quoted(true), fullstop(true), nl(true)]),
    close(Stream).
