  $ tsonnet ../../samples/tutorials/syntax.jsonnet
  {
    "cocktails": {
      "Manhattan": {
        "description": "A clear \\ red drink.",
        "garnish": "Maraschino Cherry",
        "ingredients": [
          { "kind": "Rye", "qty": 2.5 },
          { "kind": "Sweet Red Vermouth", "qty": 1 },
          { "kind": "Angostura", "qty": "dash" }
        ],
        "served": "Straight Up"
      },
      "Tom Collins": {
        "description": "The Tom Collins is essentially gin and\nlemonade.  The bitters add complexity.\n",
        "garnish": "Maraschino Cherry",
        "ingredients": [
          { "kind": "Farmer's Gin", "qty": 1.5 },
          { "kind": "Lemon", "qty": 1 },
          { "kind": "Simple Syrup", "qty": 0.5 },
          { "kind": "Soda", "qty": 2 },
          { "kind": "Angostura", "qty": "dash" }
        ],
        "served": "Tall"
      }
    }
  }

  $ tsonnet ../../samples/tutorials/variables.jsonnet
  {
    "Daiquiri": {
      "ingredients": [
        { "kind": "Banks Rum", "qty": 1.5 },
        { "kind": "Lime", "qty": 1 },
        { "kind": "Simple Syrup", "qty": 0.5 }
      ],
      "served": "Straight Up"
    },
    "Mojito": {
      "garnish": "Lime wedge",
      "ingredients": [
        { "action": "muddle", "kind": "Mint", "qty": 6, "unit": "leaves" },
        { "kind": "Banks Rum", "qty": 1.5 },
        { "kind": "Lime", "qty": 0.5 },
        { "kind": "Simple Syrup", "qty": 0.5 },
        { "kind": "Soda", "qty": 3 }
      ],
      "served": "Over crushed ice"
    }
  }

  $ tsonnet ../../samples/tutorials/references.jsonnet
  {
    "Gin Martini": {
      "garnish": "Olive",
      "ingredients": [
        { "kind": "Farmer's Gin", "qty": 2 },
        { "kind": "Dry White Vermouth", "qty": 1 }
      ],
      "served": "Straight Up"
    },
    "Martini": {
      "garnish": "Olive",
      "ingredients": [
        { "kind": "Farmer's Gin", "qty": 2 },
        { "kind": "Dry White Vermouth", "qty": 1 }
      ],
      "served": "Straight Up"
    },
    "Tom Collins": {
      "garnish": "Maraschino Cherry",
      "ingredients": [
        { "kind": "Farmer's Gin", "qty": 1.5 },
        { "kind": "Lemon", "qty": 1 },
        { "kind": "Simple Syrup", "qty": 0.5 },
        { "kind": "Soda", "qty": 2 },
        { "kind": "Angostura", "qty": "dash" }
      ],
      "served": "Tall"
    }
  }

  $ tsonnet ../../samples/tutorials/inner-reference.jsonnet
  {
    "Martini": {
      "garnish": "Olive",
      "ingredients": [
        { "kind": "Farmer's Gin", "qty": 1 },
        { "kind": "Dry White Vermouth", "qty": 1 }
      ],
      "served": "Straight Up"
    }
  }

  $ tsonnet ../../samples/tutorials/arith.jsonnet
  {
    "concat_array": [ 1, 2, 3, 4 ],
    "concat_string": "1234",
    "equality1": false,
    "equality2": true,
    "ex1": 1.6666666666666665,
    "ex2": 3,
    "ex3": 1.6666666666666665,
    "ex4": true,
    "obj": { "a": 1, "b": 3, "c": 4 },
    "obj_member": true
  }

  $ tsonnet ../../samples/tutorials/functions.jsonnet
  {
    "call": 12,
    "call_inline_function": 25,
    "call_method1": 9,
    "call_multiline_function": [ 8, 9 ],
    "named_params": 12,
    "named_params2": 5
  }
