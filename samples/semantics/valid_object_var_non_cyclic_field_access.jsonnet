local obj = {
    // This is a very weird Jsonnet valid reference.
    // However, it works because of laziness.
    a: obj.b,
    b: 1,
};
obj.a
