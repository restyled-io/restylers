#!/usr/bin/env bash
set -euo pipefail

if [[ ! -d .git ]]; then
  cat >&2 <<'EOM'
[WARNING] Auto-initializing current directory as a git repository

This is required for pre-commit to function. If this is not a test system, where
auto-initializing like this is expected, something may be wrong.

EOM
  git init --quiet
fi

validate-pre-commit
exec "$@"
