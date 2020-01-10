#!/usr/bin/env bash
git ls-files . | grep -e '.*\.hs' | xargs hlint "$@"
