{
    'cond_true': if true then 'if true works!',
    'cond_null': if false then 'unreachable!',
    'cond_else_true': if true then 'then branch' else 'else branch',
    'cond_else_false': if false then 'then branch' else 'else branch',
    'cond_else_expr': if true then 10 + 5 else 20 + 5,
    'cond_nested_object': {
        result: if false then 'not this' else 'this one'
    },
    'cond_nested_chain':
        if true then
            if false then 0
            else
                if true then 42
                else 1
        else 2
}