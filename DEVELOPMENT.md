# Development

## Requirements

Install [mise](https://mise.jdx.dev/) and run `make prepare-dev-setup`.

## Build Commands

```bash
make prepare-dev-setup  # Install dev dependencies (requires mise)
make explain            # Generate parser explanation from menhir
make build              # Build the project (dune build)
make test               # Run all tests (dune runtest)
make coverage           # Generate code coverage reports
```

## CLI Usage

```bash
dune exec tsonnet -- <file.jsonnet>          # Execute and output JSON
dune exec tsonnet -- --debug-ast <file>      # Print parsed AST
dune exec tsonnet -- --skip-typecheck <file> # Bypass type checking
```
