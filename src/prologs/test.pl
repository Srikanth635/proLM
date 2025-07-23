my_directive(Arg1, Arg2) :-
    ( var(Arg1) ->
        write('Argument 1 is unbound'), nl
    ; nonvar(Arg1) ->
        write('Argument 1 is bound to: '), write(Arg1), nl
    ),
    ( var(Arg2) ->
        write('Argument 2 is unbound'), nl
    ; nonvar(Arg2) ->
        write('Argument 2 is bound to: '), write(Arg2), nl
    ).

test_argument_types(Arg) :-
    ( var(Arg) ->
        py_call(builtins:print("python unbound variable"))
        %write('Unbound variable')
    ; atom(Arg) ->
        write('Atom: '), write(Arg)
    ; integer(Arg) ->
        write('Integer: '), write(Arg)
    ; float(Arg) ->
        write('Float: '), write(Arg)
    ; string(Arg) ->
        write('String: '), write(Arg)
    ; is_list(Arg) ->
        write('List: '), write(Arg)
    ; is_dict(Arg) ->
        write('Dictionary: '), write(Arg)
    ; compound(Arg) ->
        write('Compound term: '), write(Arg)
    ; ground(Arg) ->
        write('Ground term: '), write(Arg)
    ;   write('Unknown type: '), write(Arg)
    ).
