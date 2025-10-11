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
