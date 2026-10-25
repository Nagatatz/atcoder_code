#!/usr/bin/env bash
# 1 つの解答を AtCoder ジャッジと同じ設定 (atcoder.cabal / cabal.project / freeze) でビルドして実行する。
#   使い方: scripts/run.sh contests/abc/117/C.hs [入力ファイル]
#   入力ファイルを省略すると標準入力から読む。
set -euo pipefail
if [ $# -lt 1 ]; then
  echo "usage: $0 <solution.hs> [input-file]" >&2
  exit 1
fi
src=$1
root=$(cd "$(dirname "$0")/.." && pwd)
src_abs=$(cd "$(dirname "$src")" && pwd)/$(basename "$src")
mkdir -p "$root/.solution"
# 変更がなければコピーしない（不要な再ビルドを避ける）
cmp -s "$src_abs" "$root/.solution/Main.hs" || cp "$src_abs" "$root/.solution/Main.hs"
cd "$root"
cabal build -v0 --flags=+atcoder main
bin=$(cabal list-bin -v0 --flags=+atcoder main)
if [ $# -ge 2 ]; then
  "$bin" < "$2"
else
  "$bin"
fi
