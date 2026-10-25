# atcoder_code

[AtCoder](https://atcoder.jp/) の Haskell 解答集です。
ビルド環境は **AtCoder 2025 言語アップデート後のジャッジ（Haskell GHC 9.8.4）** と同じ構成にしてあります。

## ディレクトリ構成

```
.
├── contests/
│   ├── abc/<3桁の回>/A.hs …   AtCoder Beginner Contest   (例: contests/abc/117/C.hs)
│   ├── arc/<3桁の回>/…        AtCoder Regular Contest
│   ├── agc/<3桁の回>/…        AtCoder Grand Contest
│   └── others/<コンテストID>/… 企業コンテストなど。ID は URL の atcoder.jp/contests/<ID> と同じ
│                               (例: contests/others/code-festival-2016-qualb/B.hs)
├── books/
│   └── tessoku-book/          「競技プログラミングの鉄則」演習問題 (A01.hs, B01.hs …)
├── template/Main.hs           提出用テンプレート（ByteString ベースの入力ヘルパー入り）
├── scripts/
│   ├── run.sh                 1 つの解答をジャッジ設定でビルドして実行
│   ├── check-all.sh           全解答を型検査
│   └── setup-hls.sh           haskell-language-server 用の準備
├── atcoder.cabal              ジャッジの submission.cabal と同じ依存・言語設定
├── cabal.project              ジャッジの cabal.project（-O2 / -fllvm など）
├── cabal.project.freeze       ジャッジの freeze ファイル（依存バージョンを完全固定）
├── hie.yaml                   HLS 設定
├── Dockerfile / docker-compose.yml / .devcontainer/   ジャッジ相当の環境
└── LICENSE
```

- 1 問 1 ファイル（`Main` モジュール）です。ファイル名は問題記号（`A.hs`, `B.hs`, …）。
- 同じ問題の別解は `D2.hs`, `D3.hs`（コンテスト）や `A08-2.hs`（鉄則本）のように番号を付けています。
  命名は今後 `D.alt1.hs` のような形に統一するかもしれません。

## ジャッジ環境との対応

| 項目 | 値 |
| --- | --- |
| 言語 | Haskell (GHC 9.8.4) — AtCoder 2025 言語アップデート（ABC428 以降） |
| ビルドツール | cabal 3.14.2.0 |
| 既定の言語 | `GHC2021` |
| 最適化 | `optimization: 2`, `ghc-options: -fllvm -Wall`（LLVM 15） |
| フラグ | `+atcoder`（`-DATCODER` が定義される） |
| 依存ライブラリ | ac-library-hs, containers 0.8, vector 0.13, lens, massiv, hmatrix など 89 パッケージ（`atcoder.cabal` 参照） |

`atcoder.cabal` / `cabal.project` / `cabal.project.freeze` は公式のインストールスクリプト
（[052-ghc_9-8-3.toml](https://img.atcoder.jp/file/language-update/2025-10/052-ghc_9-8-3.toml)、
[言語一覧](https://img.atcoder.jp/file/language-update/2025-10/language-list.html)）から取っています。
ジャッジ側の設定が更新されたらこの 3 ファイルを差し替えてください。

## 解答の実行

```sh
# 標準入力から読む
scripts/run.sh contests/abc/117/C.hs

# 入力ファイルを渡す
scripts/run.sh contests/abc/117/C.hs input.txt
```

`run.sh` は対象ファイルを `.solution/Main.hs`（git 管理外）にコピーし、
`atcoder.cabal` の実行ファイル `main` として **ジャッジと同じ設定（-O2 -fllvm、freeze 済み依存）** でビルドして実行します。

全解答の型検査:

```sh
cabal build --flags=+atcoder --only-dependencies main   # 初回のみ（依存ライブラリのビルド）
scripts/check-all.sh                                     # 失敗したファイルは check-all.log に出る
```

## テンプレート

`template/Main.hs` は既存の解答でよく使っていた入力ヘルパーをまとめたものです。

| 関数 | 内容 |
| --- | --- |
| `getInt` / `getInteger` | 1 行に整数 1 つ |
| `getIntList` | 1 行に空白区切りの整数 |
| `getInt2` / `getInt3` | 1 行に整数 2 つ / 3 つ（タプルで返す） |
| `getNInt n` / `getNIntList n` | n 行 |
| `getString` / `getStringList` | 1 行の文字列 / 空白区切りの文字列 |
| `printList`, `yesNo` | 出力用 |

新しい問題を始めるときはコピーして使います。

```sh
mkdir -p contests/abc/430 && cp template/Main.hs contests/abc/430/A.hs
```

[atcoder-cli (acc)](https://github.com/Tatamo/atcoder-cli) を使う場合は、テンプレートディレクトリを作って `acc` に登録できます。

```sh
mkdir -p "$(acc config-dir)/haskell"
cp template/Main.hs "$(acc config-dir)/haskell/Main.hs"
cat > "$(acc config-dir)/haskell/template.json" <<'JSON'
{ "task": { "program": ["Main.hs"], "submit": "Main.hs" } }
JSON
acc config default-template haskell
```

（`*.in` / `*.out` / `contest.acc.json` は `.gitignore` 済みです。）

## 環境構築

次の 3 通りがあります。普段は (a) か (b) を使い、どうしても必要なときだけ (c) にしてください。

### (a) Docker だけで使う（ホストに GHC 不要）

必要なのは Docker（Docker Desktop など）だけです。

```sh
# 初回: イメージ作成。公式スクリプトと同じ手順で GHC 9.8.4 / cabal 3.14.2.0 を入れ、
# 依存 89 パッケージを -O2 -fllvm で事前ビルドするため、かなり時間がかかります（数十分程度）
docker compose build

# 解答を実行（対話入力）
docker compose run --rm atcoder scripts/run.sh contests/abc/117/C.hs

# ファイルを標準入力に流す（-T で疑似 TTY を無効にする）
docker compose run --rm -T atcoder scripts/run.sh contests/abc/117/C.hs < input.txt

# コンテナ内でシェルを開いて作業する
docker compose run --rm atcoder
#   $ scripts/run.sh contests/abc/117/C.hs
#   $ scripts/check-all.sh
```

リポジトリはコンテナの `/workspace` にマウントされます。ビルド結果（`dist-newstyle/`）もリポジトリ内に残るので、2 回目以降は速くなります。

### (b) IntelliJ IDEA で使う

2026 年 10 月時点の状況（詳細は末尾の「参考」）:

- **IntelliJ-Haskell（rikvdkleij）**: stack 専用で、2024-11-30 にリポジトリがアーカイブ（開発終了）。cabal 構成のこのリポジトリでは使いません。
- **Haskell LSP（rockofox/intellij-haskell-lsp, plugin ID `boo.fox.haskelllsp`）**: 現在メンテされている選択肢です。[LSP4IJ](https://plugins.jetbrains.com/plugin/23257-lsp4ij) の上で haskell-language-server (HLS) を動かします。2026-08 に 1.5.x、IntelliJ 2025.1 以降が対象です。無料版の IntelliJ IDEA でも動きます。
- **IntelliJ IDEA の版**: 2025.3 から Community / Ultimate が 1 つの配布物に統合され、無料版と Ultimate サブスクリプションの区別になりました。**Dev Containers は Ultimate の機能**です。
- **Dev Container のネイティブモード**（2026.1 から既定。IDE はホストで動き、コンテナ内のエージェント経由でファイル操作やプロセス実行をする）は
  プラットフォーム（EelApi）対応のプラグインでないとコンテナ内のツールを使えません。Haskell LSP プラグインは今のところ
  HLS をホスト側のパスから探して起動する実装なので（`java.io.File` / `SystemInfo` を使っています）、**ネイティブモードではコンテナ内の HLS は使われません**。

おすすめの手順は手元の環境によって次のどれかです。

**b-1. Ultimate がある場合: Dev Container（リモート開発モード）**

1. Docker を起動しておく。
2. IntelliJ IDEA の Settings → Advanced Settings → Dev Containers の **「Open devcontainer projects natively」をオフ** にする（IDE のバックエンドをコンテナ内で動かすモードにするため）。
3. `.devcontainer/devcontainer.json` を開き、ガターの Dev Container アイコンから **Create Dev Container and Mount Sources…** を選ぶ。
   イメージのビルドが終わると JetBrains Client が開きます。
4. `devcontainer.json` の `customizations.jetbrains.plugins` に LSP4IJ と Haskell LSP を指定してあるので、コンテナ側の IDE バックエンドに自動で入ります。
   入っていなければ Client の Settings → Plugins（Host 側）から入れてください。
5. `postCreateCommand` の `scripts/setup-hls.sh` が GHC 環境ファイルを作るので、HLS（コンテナ内の `haskell-language-server-wrapper`）が `lens` や `vector` などの import を解決できます。
6. 実行はコンテナ内ターミナルで `scripts/run.sh contests/abc/117/C.hs`。

この方法なら **ホストに GHC / HLS を入れる必要はありません**。

**b-2. 無料版 IntelliJ IDEA の場合: エディタ機能だけネイティブに入れる**

Dev Containers が使えないので、HLS はホストで動かす必要があります。ビルドと実行は (a) の Docker に任せ、
**HLS 用の GHC だけ** ghcup でホストに入れるのが一番簡単です（下の (c)）。そのうえで:

1. Settings → Plugins → Marketplace で **Haskell LSP** をインストール（LSP4IJ も一緒に入ります）。
2. Settings → Tools → Haskell LSP で HLS のパス（`~/.ghcup/bin/haskell-language-server-wrapper`）を指定。PATH 上にあれば自動で見つかります。
3. このリポジトリを開く前に一度 `scripts/setup-hls.sh` を実行（依存をビルドして `.ghc.environment.*` を作る）。

**b-3. （実験的）無料版でホストに GHC を入れずに済ませたい場合**

`scripts/hls-docker.sh` は、リポジトリを **ホストと同じ絶対パス** でマウントしたコンテナ内で HLS を起動するラッパーです。
Haskell LSP プラグインの HLS パスにこのスクリプトを指定すると、ホストに GHC が無くても補完や型表示が使えます（Linux / macOS 向け）。
LSP4IJ がファイル URI をそのまま渡すためにパスを揃えているだけなので、うまく動かない場合は b-2 にしてください。

```sh
docker compose build          # イメージ atcoder-haskell:ghc-9.8.4 を作る
scripts/hls-docker.sh --probe # 動作確認（コンテナ内の HLS のバージョンを表示）
```

### (c) ネイティブ環境（ghcup）

公式スクリプトと同じ手順です（Ubuntu / Debian の例）。

```sh
sudo apt-get install -y build-essential curl libffi-dev libgmp-dev libncurses-dev libtinfo6 \
  llvm-15 libgsl0-dev liblapack-dev libatlas-base-dev libglpk-dev
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
source ~/.ghcup/env
ghcup install cabal 3.14.2.0 && ghcup set cabal 3.14.2.0
ghcup install ghc 9.8.4 && ghcup set ghc 9.8.4
ghcup install hls 2.14.0.0 && ghcup set hls 2.14.0.0   # エディタ連携用

cabal update
cabal build --flags=+atcoder --only-dependencies main
```

- `-fllvm` のため LLVM 15 の `opt` / `llc` が PATH に必要です（Ubuntu なら `/usr/lib/llvm-15/bin`）。
  llvm-15 が apt に無いディストリビューション（Debian 13 など）では
  [LLVM 15 のリリースバイナリ](https://github.com/llvm/llvm-project/releases/tag/llvmorg-15.0.6) を展開して PATH に足してください。
- macOS は `brew install llvm@15 gsl glpk` などで代わりにできますが、ジャッジと完全に同じにはなりません。
- `libatlas-base-dev` が無い環境では `libblas-dev` で代わりになります。
- `cabal update` で `http://hackage.haskell.org` に繋がらない環境では `~/.config/cabal/config` の `url:` を `https://` にしてください。

### 参考

- AtCoder 2025 言語アップデート: [言語一覧](https://img.atcoder.jp/file/language-update/2025-10/language-list.html) / [Haskell のインストールスクリプト](https://img.atcoder.jp/file/language-update/2025-10/052-ghc_9-8-3.toml)
- [rikvdkleij/intellij-haskell](https://github.com/rikvdkleij/intellij-haskell)（2024-11-30 アーカイブ）
- [rockofox/intellij-haskell-lsp](https://github.com/rockofox/intellij-haskell-lsp) / [JetBrains Marketplace: Haskell LSP](https://plugins.jetbrains.com/plugin/24123-haskell-lsp) / [LSP4IJ](https://github.com/redhat-developer/lsp4ij)
- [IntelliJ IDEA Moves to the Unified Distribution](https://blog.jetbrains.com/idea/2025/07/intellij-idea-unified-distribution-plan/) / [IntelliJ IDEA as a unified product](https://www.jetbrains.com/help/idea/intellij-idea-single-distribution.html)
- [What's New in IntelliJ IDEA 2026.1](https://www.jetbrains.com/idea/whatsnew/2026-1/)（Dev Container のネイティブ対応は Ultimate）
- [Open a project with Dev Container natively](https://www.jetbrains.com/help/idea/open-project-with-dev-container-natively.html) / [The Dev Containers Story: Introducing EelApi for Plugin Authors](https://blog.jetbrains.com/platform/2026/06/the-dev-containers-story-introducing-eelapi-for-plugin-authors/)
- [Remote Development FAQ](https://www.jetbrains.com/remote-development/)（リモート開発は有料サブスクリプションが必要）

## ライセンス

[LICENSE](LICENSE)（BSD-3-Clause）
