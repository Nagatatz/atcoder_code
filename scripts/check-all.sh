#!/usr/bin/env bash
# すべての解答 (contests/, books/) と template/ を GHC 9.8.4 で型検査する（-fno-code）。
# 依存パッケージは cabal.project.freeze どおりにビルド済みである必要がある:
#   cabal build --only-dependencies main
# 失敗したファイルは check-all.log に出力される。
set -uo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"
log=check-all.log
: > "$log"
pass=0; fail=0
while IFS= read -r f; do
  if out=$(cabal exec -v0 -- ghc -fno-code -XGHC2021 -v0 "$f" 2>&1); then
    pass=$((pass + 1))
  else
    fail=$((fail + 1))
    printf '=== %s\n%s\n' "$f" "$out" >> "$log"
    echo "FAIL $f"
  fi
done < <(find contests books template -name '*.hs' | sort)
echo "pass=$pass fail=$fail (詳細: $log)"
[ "$fail" -eq 0 ]
