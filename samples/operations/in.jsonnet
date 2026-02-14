{
    has_field: 'foo' in { foo: 1 },
    has_no_field: 'xyz' in { foo: 1 },

    local field_name = 'bar',
    has_field_bar: field_name in { bar: 2 },
    has_no_field_bar: field_name in { baz: 3 },
}
