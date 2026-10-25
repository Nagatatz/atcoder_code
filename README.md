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

次の 3 通りがあります。どれでもジャッジと同じ GHC 9.8.4 / cabal 3.14.2.0 / 依存ライブラリ（freeze 済み）でビルドされます。

| 方法 | ホストに入れるもの | 向いている場合 |
| --- | --- | --- |
| (a) Docker だけ | Docker | ターミナル中心で使う。手元を汚したくない |
| (b) IntelliJ IDEA + Dev Container | Docker（+ IntelliJ IDEA Ultimate） | Ultimate があり、ホストに GHC を入れたくない |
| (c) ネイティブ（手元に直接入れる） | ghcup で GHC / cabal / HLS、LLVM 15 など | ビルドを速くしたい。無料版 IntelliJ や他のエディタで使う |

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
  HLS をホスト側のパスから探して起動する実装なので（`java.io.File` / `SystemInfo` を使っています）、**ネイティブモードではコンテナ内の HLS は使われないと考えられます**（プラグインのコードからの判断で、実機では未確認）。

使い方は次のどれかです。

**b-1. Ultimate: Dev Container（リモート開発モード）で使う — ホストに GHC 不要**

1. Docker を起動しておく。
2. IntelliJ IDEA の Settings → Advanced Settings → Dev Containers の **「Open devcontainer projects natively」をオフ** にする（IDE のバックエンドをコンテナ内で動かすモードにするため）。
3. `.devcontainer/devcontainer.json` を開き、ガターの Dev Container アイコンから **Create Dev Container and Mount Sources…** を選ぶ。
   イメージのビルドが終わると JetBrains Client が開きます。
4. `devcontainer.json` の `customizations.jetbrains.plugins` に LSP4IJ と Haskell LSP を指定してあるので、コンテナ側の IDE バックエンドに自動で入ります。
   入っていなければ Client の Settings → Plugins（Host 側）から入れてください。
5. `postCreateCommand` の `scripts/setup-hls.sh` が GHC 環境ファイルを作るので、HLS（コンテナ内の `haskell-language-server-wrapper`）が `lens` や `vector` などの import を解決できます。
6. 実行はコンテナ内ターミナルで `scripts/run.sh contests/abc/117/C.hs`。

**b-2. ネイティブに入れた環境で使う（Ultimate / 無料版どちらでも）→ (c) を参照**

ホストに GHC / HLS を入れる方法です。無料版の IntelliJ IDEA では Dev Container が使えないので、こちらになります。
手順は **(c) ネイティブ環境** の c-1〜c-3 で GHC などを入れ、**c-4 の IntelliJ IDEA の設定** をしてください。

**b-3. （実験的）無料版でホストに GHC を入れずに済ませたい場合**

`scripts/hls-docker.sh` は、リポジトリを **ホストと同じ絶対パス** でマウントしたコンテナ内で HLS を起動するラッパーです。
Haskell LSP プラグインの HLS パスにこのスクリプトを指定すると、ホストに GHC が無くても補完や型表示が使えます（Linux / macOS 向け）。
LSP4IJ がファイル URI をそのまま渡すためにパスを揃えているだけなので、うまく動かない場合は b-2 にしてください。

```sh
docker compose build          # イメージ atcoder-haskell:ghc-9.8.4 を作る
scripts/hls-docker.sh --probe # 動作確認（コンテナ内の HLS のバージョンを表示）
```

### (c) ネイティブ環境（手元に直接入れる）

[ghcup](https://www.haskell.org/ghcup/) で GHC / cabal / HLS を入れ、ジャッジと同じ依存ライブラリを手元でビルドします。
流れは c-1（OS ごとの準備）→ c-2（ghcup で GHC 等を入れる）→ c-3（依存のビルドと動作確認）→ c-4（IntelliJ IDEA の設定）です。

入れるバージョン:

| ツール | バージョン | 備考 |
| --- | --- | --- |
| GHC | 9.8.4 | ジャッジと同じ |
| cabal-install | 3.14.2.0 | ジャッジと同じ |
| HLS | 2.14.0.0 | GHC 9.8.4 用のバイナリを含む最新版（エディタ用） |
| LLVM | 15 | `-fllvm` 用。GHC 9.8 が対応するのは LLVM 11〜15 で、ジャッジは llvm-15 |

#### c-1. OS ごとの準備

**macOS（Apple Silicon / Intel）**

1. Xcode Command Line Tools を入れる（C コンパイラ。ghcup のインストール中にダイアログが出ることもあります）。

   ```sh
   xcode-select --install
   ```

2. [Homebrew](https://brew.sh/) で LLVM 15 と C ライブラリを入れる。

   ```sh
   brew install llvm@15 gmp gsl glpk pkgconf
   ```

   - `llvm@15`（15.0.7、ジャッジと同じ版）は 2026 年 10 月時点で Homebrew にあり、非推奨にもなっていません。
     バイナリ（bottle）は macOS 14 Sonoma 用までですが、Homebrew は新しい macOS でも古い macOS 用の bottle を使うので、通常はソースからのビルドにはなりません。
   - `llvm@15` は keg-only（`/opt/homebrew/bin` にリンクされない）なので、`opt` / `llc` を PATH に足します。
     **PATH の先頭に置くと Apple の clang が LLVM 15 の clang に置き換わる**ので、末尾に足してください。

     ```sh
     # ~/.zshrc に追記
     export PATH="$PATH:$(brew --prefix llvm@15)/bin"
     ```

   - LAPACK / BLAS は macOS 標準の Accelerate フレームワークを使うので（`hmatrix` の macOS 向け設定）、別途入れる必要はありません。

3. **Apple Silicon の場合**: Homebrew は `/opt/homebrew` に入りますが、`hmatrix-gsl` / `hmatrix-glpk` が探すのは `/usr/local` と `/opt/local` だけです。
   リポジトリ直下に `cabal.project.local`（git 管理外）を作って場所を教えてください。

   ```sh
   cat > cabal.project.local <<'EOF2'
   package *
       extra-include-dirs: /opt/homebrew/include
       extra-lib-dirs: /opt/homebrew/lib
   EOF2
   ```

   - GHC 9.8.4 / cabal 3.14.2.0 / HLS 2.14.0.0 には ghcup で Apple Silicon（aarch64-apple-darwin）用のバイナリがあります。Rosetta は不要です。
   - ghcup の公式表では macOS aarch64 の HLS は「実験的（⚠️）」扱いです。動かないときは b-1（Dev Container）か b-3 を使ってください。
   - Intel 版 Homebrew（`/usr/local`）と Apple Silicon 版（`/opt/homebrew`）を混ぜないでください。
   - Intel Mac は Homebrew が `/usr/local` にあるので `cabal.project.local` は不要です（Homebrew は Intel 向け bottle の新規ビルドを終えており、ソースビルドになる場合があります）。

**Ubuntu / Debian**

公式のインストールスクリプトと同じパッケージです（Ubuntu 24.04 で確認）。

```sh
sudo apt-get update
sudo apt-get install -y build-essential curl git libffi-dev libgmp-dev libncurses-dev libtinfo6 zlib1g-dev pkg-config \
  llvm-15 libgsl0-dev liblapack-dev libatlas-base-dev libglpk-dev
echo 'export PATH="$PATH:/usr/lib/llvm-15/bin"' >> ~/.bashrc   # opt / llc を PATH に
```

- llvm-15 が apt に無いディストリビューション（Debian 13 など）では
  [LLVM 15 のリリースバイナリ](https://github.com/llvm/llvm-project/releases/tag/llvmorg-15.0.6) を展開して `bin` を PATH に足してください
  （15.0.7 には汎用 Linux 向けバイナリがないため 15.0.6）。
- `libatlas-base-dev` が無い環境では `libblas-dev` で代わりになります。

**Windows: WSL2 + Ubuntu を推奨**

ネイティブ Windows でも ghcup は動きますが、LLVM 15 や GSL / GLPK / LAPACK をそろえるのが大変なので、WSL2 の Ubuntu を使ってください。

1. PowerShell（管理者）で WSL2 と Ubuntu 24.04 を入れて再起動する。

   ```powershell
   wsl --install -d Ubuntu-24.04
   ```

2. Ubuntu を起動し、上の **Ubuntu / Debian** の手順を実行する。
3. リポジトリは **WSL 側のホーム（例: `~/atcoder_code`）に clone** する。`/mnt/c/...` 上だとビルドがかなり遅くなります。

   ```sh
   git clone https://github.com/Nagatatz/atcoder_code.git ~/atcoder_code
   ```

4. 以降の c-2 / c-3 は WSL の Ubuntu の中で実行します。IntelliJ IDEA の設定は c-4 の「Windows（WSL2）の場合」を参照。

#### c-2. ghcup で GHC 9.8.4 / cabal 3.14.2.0 / HLS 2.14.0.0 を入れる

```sh
# ghcup 本体だけを入れる（シェルの rc ファイルに PATH 設定を追記）
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 BOOTSTRAP_HASKELL_ADJUST_BASHRC=1 sh
source ~/.ghcup/env

ghcup install ghc 9.8.4        && ghcup set ghc 9.8.4
ghcup install cabal 3.14.2.0   && ghcup set cabal 3.14.2.0
ghcup install hls 2.14.0.0     && ghcup set hls 2.14.0.0

ghc --version     # The Glorious Glasgow Haskell Compilation System, version 9.8.4
cabal --version   # cabal-install version 3.14.2.0
haskell-language-server-wrapper --version   # 2.14.0.0
which opt llc     # LLVM 15 の opt / llc が見えること
```

対話形式で入れたい場合は `BOOTSTRAP_HASKELL_...` を付けずに実行し、質問に答えてください。
すでに別のバージョンの GHC を使っている場合も、`ghcup set ghc 9.8.4` で 9.8.4 を既定にしておくと HLS が迷いません。

#### c-3. 依存ライブラリのビルドと動作確認

```sh
cd atcoder_code
cabal update

# freeze（cabal.project.freeze）どおりに依存 89 パッケージ（とその依存）を -O2 -fllvm でビルド。
# 初回は数十分かかります（こちらの Linux 環境で約 40 分）。2 回目以降は cabal のストアから使われます。
scripts/setup-hls.sh
#  ↑ 中身は cabal build --flags=+atcoder --only-dependencies --write-ghc-environment-files=always main
#    で、HLS 用の .ghc.environment.* も作ります

# 動作確認: テンプレートの main は 1 行目の n と 2 行目の数の合計を出力する
printf '3\n1 2 3\n' | scripts/run.sh template/Main.hs    # → 9 (= 3 + 1 + 2 + 3)
scripts/run.sh contests/abc/117/C.hs                       # 標準入力から読む

# 全解答の型チェック（失敗したファイルは check-all.log に出る）
scripts/check-all.sh
```

`scripts/check-all.sh` は 2026-10 時点で 282 ファイル中 6 ファイルが失敗します（書きかけ・バグのある解答で、環境の問題ではありません）。

#### c-4. IntelliJ IDEA の設定（Ultimate / 無料版共通）

1. **プラグイン**: Settings → Plugins → Marketplace で **Haskell LSP** をインストールし、IDE を再起動（依存する **LSP4IJ** も一緒に入ります）。
2. **HLS のパス**: Settings → Tools → **Haskell LSP** で `haskell-language-server-wrapper` のパスを指定します。
   PATH 上にあれば自動で見つかりますが、IDE を Dock / デスクトップから起動すると PATH が通っていないことがあるので、絶対パスで指定するのが確実です。
   - macOS: `/Users/<ユーザー名>/.ghcup/bin/haskell-language-server-wrapper`
   - Linux: `/home/<ユーザー名>/.ghcup/bin/haskell-language-server-wrapper`
3. **HLS 用の環境ファイル**: プロジェクトを開く前に（または依存を変えたら）ターミナルで `scripts/setup-hls.sh` を実行します。
   これで `.ghc.environment.*` ができ、`hie.yaml`（direct cradle）経由で HLS が `vector` や `ac-library-hs` などの import を解決できます。
   すでに IDE を開いていた場合は、LSP4IJ の Language Servers ビューから HLS を再起動してください。
4. **解答の実行**: 次のどれかです。
   - **ターミナル**（IDE 下部の Terminal）: `scripts/run.sh contests/abc/117/C.hs`。対話的に入力できるのはこれだけです。
   - **External Tools（開いているファイルを実行）**: Settings → Tools → External Tools → `+` で次のように登録し、
     Settings → Keymap → External Tools でショートカットを割り当てると便利です。
     入力は解答と同じディレクトリの同名 `.in` ファイル（例: `C.hs` → `C.in`、`.gitignore` 済み）から読みます。

     | 項目 | 値 |
     | --- | --- |
     | Name | `AtCoder run` |
     | Program | `$ProjectFileDir$/scripts/run.sh` |
     | Arguments | `$FilePathRelativeToProjectRoot$ $FileDir$/$FileNameWithoutExtension$.in` |
     | Working directory | `$ProjectFileDir$` |

   - **Shell Script の Run Configuration（特定のファイル用）**: Run → Edit Configurations → `+` → **Shell Script** →
     Execute: Script file、Script path: `scripts/run.sh`、Script options: `contests/abc/117/C.hs input.txt`、
     Working directory: `$PROJECT_DIR$`。Script path / Script options では「今開いているファイル」のマクロは使えないので、
     開いているファイルを実行したい場合は上の External Tools を使ってください。

**Windows（WSL2）の場合**

- プロジェクトは `\\wsl.localhost\Ubuntu-24.04\home\<ユーザー名>\atcoder_code` として開きます。
  Haskell LSP プラグインはプロジェクトが WSL のパスにあると、HLS を `wsl.exe -d <ディストリビューション> -- bash -lc` で WSL 内で起動し、ファイルパスを相互に変換します。
- その際 `~/.bashrc` は読まれない（ログインシェルかつ非対話のため）ので、HLS のパスには **WSL 内の絶対パス**
  `/home/<ユーザー名>/.ghcup/bin/haskell-language-server-wrapper` を指定してください。
- `scripts/setup-hls.sh` や `scripts/run.sh` は WSL のターミナル（IntelliJ の Terminal で Ubuntu のシェルを選ぶ）で実行します。
- WSL 対応はプラグインの機能説明とコードに基づく手順で、実機では未確認です。

#### c-5. うまくいかないとき

- **`could not execute: opt` / `llc` が見つからない、LLVM のバージョンが違う**:
  `-fllvm` のため LLVM の `opt` / `llc` が PATH に必要です。`which opt llc` と `opt --version` で 15 系が見えるか確認してください
  （macOS: `$(brew --prefix llvm@15)/bin`、Ubuntu: `/usr/lib/llvm-15/bin`）。GHC 9.8 は LLVM 16 以降に対応していません。
- **HLS が「GHC のバージョンが合わない」「haskell-language-server-9.x.x が無い」と言う**:
  `ghc --version` が 9.8.4 か確認し、違えば `ghcup set ghc 9.8.4`。HLS は 2.14.0.0 を使ってください（`ghcup list -t hls` で確認）。
  その後 `scripts/setup-hls.sh` をやり直して HLS を再起動します。
- **HLS で import が解決できない（`Could not find module`）**: `scripts/setup-hls.sh` を実行して `.ghc.environment.*` を作り直してください。
- **`cabal update` が失敗する**: `http://hackage.haskell.org` に繋がらない環境では、cabal の設定ファイル
  （`~/.config/cabal/config`。`~/.cabal` がある場合は `~/.cabal/config`）の `repository hackage.haskell.org` の `url:` を `https://hackage.haskell.org/` にしてください。
- **macOS で `cannot find -lgsl` / `-lglpk`、`gsl/gsl_*.h: No such file`**: c-1 の `cabal.project.local`（Apple Silicon）を確認してください。
- **Linux で `cannot find -lgmp` などのリンクエラー**: c-1 の apt パッケージが入っているか確認してください。

### 参考

- AtCoder 2025 言語アップデート: [言語一覧](https://img.atcoder.jp/file/language-update/2025-10/language-list.html) / [Haskell のインストールスクリプト](https://img.atcoder.jp/file/language-update/2025-10/052-ghc_9-8-3.toml)
- [rikvdkleij/intellij-haskell](https://github.com/rikvdkleij/intellij-haskell)（2024-11-30 アーカイブ）
- [rockofox/intellij-haskell-lsp](https://github.com/rockofox/intellij-haskell-lsp) / [JetBrains Marketplace: Haskell LSP](https://plugins.jetbrains.com/plugin/24123-haskell-lsp) / [LSP4IJ](https://github.com/redhat-developer/lsp4ij)
- [IntelliJ IDEA Moves to the Unified Distribution](https://blog.jetbrains.com/idea/2025/07/intellij-idea-unified-distribution-plan/) / [IntelliJ IDEA as a unified product](https://www.jetbrains.com/help/idea/intellij-idea-single-distribution.html)
- [What's New in IntelliJ IDEA 2026.1](https://www.jetbrains.com/idea/whatsnew/2026-1/)（Dev Container のネイティブ対応は Ultimate）
- [Open a project with Dev Container natively](https://www.jetbrains.com/help/idea/open-project-with-dev-container-natively.html) / [The Dev Containers Story: Introducing EelApi for Plugin Authors](https://blog.jetbrains.com/platform/2026/06/the-dev-containers-story-introducing-eelapi-for-plugin-authors/)
- [Remote Development FAQ](https://www.jetbrains.com/remote-development/)（リモート開発は有料サブスクリプションが必要）
- ネイティブ環境: [GHCup](https://www.haskell.org/ghcup/) / [GHCup の対応プラットフォーム](https://www.haskell.org/ghcup/install/support/)（macOS aarch64 の HLS は実験的扱い） / [HLS 2.14.0.0 リリース](https://github.com/haskell/haskell-language-server/releases/tag/2.14.0.0)（GHC 9.8.4 用バイナリを含む）
- [Homebrew: llvm@15](https://formulae.brew.sh/formula/llvm@15) / [Homebrew の Bottles](https://docs.brew.sh/Bottles) / [GHC 9.8 の対応 LLVM（configure.ac: 11 以上 16 未満）](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.4-release/configure.ac)
- IntelliJ IDEA: [External Tools](https://www.jetbrains.com/help/idea/settings-tools-external-tools.html) / [組み込みマクロ](https://www.jetbrains.com/help/idea/built-in-macros.html) / [Shell Script の Run Configuration](https://www.jetbrains.com/help/idea/run-debug-configuration-shell-script.html)
- [WSL のインストール](https://learn.microsoft.com/ja-jp/windows/wsl/install)

## ライセンス

[LICENSE](LICENSE)（BSD-3-Clause）
