#!/bin/bash
set -euxo pipefail

# Setup directories
readonly PREFIX=$HOME/.local
readonly BINDIR=$PREFIX/bin
mkdir -p $BINDIR

# Get stack
if [[ $TRAVIS_OS_NAME == "osx" ]]; then
  readonly WILDCARDS=
else
  readonly WILDCARDS=--wildcards
fi
curl -L https://get.haskellstack.org/stable/$TRAVIS_OS_NAME-x86_64.tar.gz | tar xz --strip-components=1 $WILDCARDS -C $BINDIR '*/stack'

# Get NASM
readonly NASMVER=2.14
readonly SRCDIR=$HOME/src
readonly NASMDIR=$SRCDIR/nasm-$NASMVER

mkdir -p $SRCDIR
pushd $SRCDIR

case $TRAVIS_OS_NAME in
  linux)
    curl -L https://www.nasm.us/pub/nasm/releasebuilds/$NASMVER/nasm-$NASMVER.tar.gz | tar xz
    pushd $NASMDIR
    ./configure --prefix=$PREFIX
    make install
    popd
    ;;
  osx)
    curl -O -L https://www.nasm.us/pub/nasm/releasebuilds/$NASMVER/macosx/nasm-$NASMVER-macosx.zip
    unzip nasm-$NASMVER-macosx.zip
    cp $NASMDIR/nasm $BINDIR
    ;;
  *)
    echo "Unsupported OS: $TRAVIS_OS_NAME"
    exit 1
    ;;
esac

popd
