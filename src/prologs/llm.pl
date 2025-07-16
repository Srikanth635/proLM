:- module(llm, [llm_query_json/2]).
:- use_module(library(process)).
:- use_module(library(http/json)).

% Ensure UTF-8 is used everywhere
:- set_prolog_flag(encoding, utf8).

llm_query_json(Prompt, Answer) :-
    process_create(path(python3),
                   ['py_bridge.py', Prompt],
                   [stdout(pipe(Out))]),
    json_read_dict(Out, Dict),
    close(Out),
    Answer = Dict.answer.

%% llm_query_json(+Prompt, -Answer)
%  Prompt is a string or atom containing your question.
%  Answer is the parsed JSON field "answer".
llm_query_json_std(Prompt, Answer) :-
    process_create(path(python3),
                   ['py_bridge.py'],        % call python script
                   [stdin(pipe(In)), stdout(pipe(Out))]),
    % Send the prompt to Python
    format(In, '~w~n', [Prompt]),
    close(In),
    % Read JSON response
    json_read_dict(Out, Dict),
    close(Out),
    Answer = Dict.answer.


%% ad_translation_run(+Instruction)
%  Instruction is a string that will be sent to Python.
%  Python script handles the rest of the logic internally.
ad_translation_run(Instruction) :-
    process_create(path(python3),
                   ['ad_translation.py'],           % Python script
                   [stdin(pipe(In)), stdout(pipe(Out))]),
    % Send the instruction to Python
    format(In, '~w~n', [Instruction]),
    close(In),
    % Read all Python output and print to Prolog console
    read_stream_to_codes(Out, Codes),
    close(Out),
    atom_codes(AtomOutput, Codes),
    format('Python output:\n~s\n', [AtomOutput]).