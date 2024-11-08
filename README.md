# tsonnet

Like [Jsonnet](https://jsonnet.org/), but [gradually typed](https://en.wikipedia.org/wiki/Gradual_typing).

"How hard could it be?" -- Myself speaking in an arrogant tone before realizing what I'm getting into XD

## Goal

Re-implement the [Jsonnet specification](https://jsonnet.org/ref/spec.html) following the [language design rationale](https://jsonnet.org/articles/design.html), while adding minimal and optional type definition features. It aims to be as compatible as possible with Jsonnet, but not fully compatible to the point that it hurts [type soundness](https://en.wikipedia.org/wiki/Type_safety).

Ideally, tsonnet and jsonnet could be used interchangeably given the jsonnet could be fully parsed into a valid tsonnet AST.
