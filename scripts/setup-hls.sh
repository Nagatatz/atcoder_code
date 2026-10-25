#!/usr/bin/env bash
# haskell-language-server (HLS) 用の準備。
# 解答ファイルは cabal のコンポーネントに属さないので、hie.yaml の direct cradle で
# 単体ファイルとして読ませる。その際に lens / vector などジャッジと同じ依存を
# 見えるようにするため、GHC の環境ファイル (.ghc.environment.*) を生成する。
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"
mkdir -p .solution
[ -f .solution/Main.hs ] || cp template/Main.hs .solution/Main.hs
cabal build -v1 --flags=+atcoder --only-dependencies --write-ghc-environment-files=always main
ls .ghc.environment.* >/dev/null
echo "HLS の準備完了: $(ls .ghc.environment.*)"
