# -*- mode: just -*-

# Test project
test:
    uv run pytest -v

# Check formatting with blue style
fmt:
    # See also configuration for pre-commit:
    #   github.com/jakewilliami/ispunct-py/blob/05934d5c/.pre-commit-config.yaml
    #
    # And for Ruff:
    #   github.com/jakewilliami/ispunct-py/blob/05934d5c/pyproject.toml#L23-L44
    uv run pre-commit run --all-files
    # Alternatively:
    #   uv run ruff check --fix  # linter
    #   uv run ruff format       # formatter

# Audit project dependencies
audit:
    # For alternative, see commit 390c877b
    uv tree
    uv pip compile pyproject.toml -o requirements.txt
    uvx pip-audit -r requirements.txt --fix

# Benchmark performance
# bench:
#     TODO
