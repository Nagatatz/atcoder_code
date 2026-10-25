# AtCoder 2025 言語アップデート後の Haskell (GHC 9.8.4) ジャッジ環境を再現するイメージ。
# 公式インストールスクリプト
#   https://img.atcoder.jp/file/language-update/2025-10/052-ghc_9-8-3.toml
# と同じ手順（Ubuntu 24.04 / llvm-15 / ghcup で cabal 3.14.2.0 と GHC 9.8.4）を踏み、
# このリポジトリの atcoder.cabal / cabal.project / cabal.project.freeze で依存ライブラリを事前ビルドする。
# エディタ連携用に GHC 9.8.4 対応の haskell-language-server も入れる。
#
#   docker compose build
#   docker compose run --rm atcoder scripts/run.sh contests/abc/117/C.hs
FROM ubuntu:24.04

ARG CABAL_VERSION=3.14.2.0
ARG GHC_VERSION=9.8.4
# GHC 9.8.4 のバイナリを同梱している HLS（ghcup の recommended, 2026-10 時点）
ARG HLS_VERSION=2.14.0.0

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

# 公式スクリプトと同じパッケージ（+ git / sudo / pkg-config / unzip / procps は開発用）
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      build-essential ca-certificates curl \
      libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses6 libtinfo6 \
      llvm-15 \
      libgsl0-dev liblapack-dev libatlas-base-dev libglpk-dev \
      git sudo pkg-config unzip procps less \
 && rm -rf /var/lib/apt/lists/* \
 && echo 'ubuntu ALL=(ALL) NOPASSWD:ALL' > /etc/sudoers.d/ubuntu

# ubuntu:24.04 に最初からある UID 1000 のユーザー ubuntu で作業する（bind mount の権限対策）
USER ubuntu
WORKDIR /home/ubuntu
ENV PATH=/home/ubuntu/.ghcup/bin:/home/ubuntu/.cabal/bin:/home/ubuntu/.local/bin:/usr/lib/llvm-15/bin:$PATH

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
      | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh \
 && mkdir -p ~/.cabal \
 && ghcup install cabal ${CABAL_VERSION} && ghcup set cabal ${CABAL_VERSION} \
 && ghcup install ghc ${GHC_VERSION} && ghcup set ghc ${GHC_VERSION} \
 && ghcup install hls ${HLS_VERSION} && ghcup set hls ${HLS_VERSION} \
 && ghcup gc --cache --tmpdirs \
 && cabal user-config init \
 && sed -i 's|url: http://hackage.haskell.org/|url: https://hackage.haskell.org/|' \
      $(ls ~/.cabal/config ~/.config/cabal/config 2>/dev/null)

# 依存ライブラリの事前ビルド（ジャッジの `cabal v2-build --only-dependencies` 相当）。
# cabal の store（公式スクリプトと同じく ~/.cabal があるので ~/.cabal/store）に入るので、リポジトリを mount した後も再利用される。
COPY --chown=ubuntu:ubuntu atcoder.cabal cabal.project cabal.project.freeze LICENSE /home/ubuntu/deps/
RUN cd /home/ubuntu/deps \
 && mkdir -p .solution && echo "main = return () :: IO ()" > .solution/Main.hs \
 && cabal v2-update \
 && cabal v2-build --flags=+atcoder --only-dependencies main \
 && cd && rm -rf /home/ubuntu/deps ~/.cabal/packages/*/*/*/*.tar.gz ~/.cache/cabal/packages/*/*/*/*.tar.gz

WORKDIR /workspace
CMD ["bash"]
