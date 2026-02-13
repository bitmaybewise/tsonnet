{
    eq_number_number: 1 == 1,
    dif_number_number: 1 == 2,
    dif_number_str: "1" == 1,

    eq_str_str: "42" == "42",
    dif_str_str: "42" == "0",

    eq_array_array: [1,2,3] == [1,2,3],
    dif_array_array: [1,2,3] == [3,2,1],

    eq_obj_obj: {x: 1, y: 2, z: 3} == {z: 3, x: 1, y: 2},
    dif_obj_obj: {a: 1, b: 2} == {b: 2, c: 3},

    eq_complex_obj_complex_obj: [{}, { x: 3 - 1 }] == [{}, { x: 2 }],
    dif_complex_obj_complex_obj: [{ a: 1 }, { b: 2 }] == [{ b: 2 }, { a: 1 }],
}

