{
    // Two strings can be compared with `<` (unicode codepoint order).
    gt_true: "é" > "e",
    gte_true: "é" >= "e",
    lt_true: "e" < "é",
    lte_true: "e" <= "é",

    gt_false: "é" < "e",
    gte_false: "é" <= "e",
    lt_false: "e" > "é",
    lte_false: "e" >= "é",

    ordered: std.sort(["b", "a", "é", "A"])
}