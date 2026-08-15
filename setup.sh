#!/usr/bin/env bash

set -euo pipefail

root_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
extension_dir="$root_dir/vscode"
tongue_dir="${TUNG_TONGUE:-$root_dir/tongue}"
cache_dir="$extension_dir/.npm-cache"
output_dir="$extension_dir/dist"
vsix="$output_dir/tung-vscode.vsix"

printf 'building tung compiler\n'
(cd -- "$tongue_dir" && cabal build exe:tung)

printf 'preparing vscode client and language server\n'
mkdir -p "$cache_dir" "$output_dir"
(
  cd -- "$extension_dir"
  npm install --cache "$cache_dir"
  npm run build
  npm run check
  npm test
  npm exec -- vsce package --allow-missing-repository --skip-license --out "$vsix"
)

printf 'installing or updating vscode extension\n'
code --install-extension "$vsix" --force

printf 'ready: tung.tung-vscode@0.1.0\n'
printf 'reload vscode, then open a .tung file\n'
