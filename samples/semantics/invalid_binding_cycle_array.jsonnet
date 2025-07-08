local a = b,
    b = c,
    c = (local d = 1, e = 2; [a, b, c, d, e]);
b
