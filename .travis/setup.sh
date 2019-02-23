#!/bin/bash
set -euxo pipefail

# Setup directories
readonly PREFIX=$HOME/.local
readonly BINDIR=$PREFIX/bin
mkdir -p $BINDIR

# We install our dependencies on MacOS using brew.
if [[ $TRAVIS_OS_NAME == "linux" ]]; then
    # Get stack
    curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --strip-components=1 --wildcards -C $BINDIR '*/stack'

    # Get NASM
    readonly NASMVER=2.14
    readonly SRCDIR=$HOME/src
    readonly NASMDIR=$SRCDIR/nasm-$NASMVER

    mkdir -p $SRCDIR
    pushd $SRCDIR

    curl -L https://www.nasm.us/pub/nasm/releasebuilds/$NASMVER/nasm-$NASMVER.tar.gz | tar xz
    pushd $NASMDIR
    ./configure --prefix=$PREFIX
    make install
    popd

    popd
fi
