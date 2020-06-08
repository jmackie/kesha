#!/usr/bin/env nix-shell
#! nix-shell -i bash

set -euo pipefail
shopt -s inherit_errexit

print_versions() { (
  set -x
  ghc --version
  cabal --version
  hlint --version
  ormolu --version
); }

build_and_test() {
  cabal new-update
  cabal new-build
  cabal new-test --test-show-details=streaming
}

lint() {
  hlint --git

  local exit=0
  for f in $(git ls-files | grep -e '\.hs'); do
    if ! ormolu --mode check "$f"; then
      echo 2>&1 "$f isn't formatted"
      exit=1
    fi
  done

  return $exit
}

main() {
  print_versions
  build_and_test
  lint
}

main "$@"
