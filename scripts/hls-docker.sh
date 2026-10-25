#!/usr/bin/env bash
# （実験的）Docker イメージ内の haskell-language-server を、ホストと同じ絶対パスで
# リポジトリをマウントして起動するラッパー。IntelliJ の Haskell LSP プラグインなどで
# 「HLS のパス」にこのスクリプトを指定すると、ホストに GHC/HLS を入れずに済む。
#   事前に: docker compose build
#   動作確認: scripts/hls-docker.sh --probe
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
image=${ATCODER_HASKELL_IMAGE:-atcoder-haskell:ghc-9.8.4}
if [ "${1:-}" = "--probe" ]; then
  exec docker run --rm -v "$root:$root" -w "$root" "$image" haskell-language-server-wrapper --version
fi
# stdout は LSP の通信に使うので、準備処理の出力は stderr に回す
exec docker run --rm -i -v "$root:$root" -w "$root" "$image" \
  bash -c 'scripts/setup-hls.sh 1>&2 && exec haskell-language-server-wrapper "$@"' hls "$@"
