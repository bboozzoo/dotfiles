#!/bin/zsh
# -*- mode: sh -*-

set -e

idirs=(8 16 22 24 32 48 64 128 256 512 scalable)

dirs=(actions animations apps categories devices emblems mimetypes places status stock)

for en in $idirs; do
    tdir=$en
    if [[ "$en" =~ ^[0-9]+$ ]]; then
        tdir="${en}x${en}"
    fi
    echo "target dir $tdir"
    
    mkdir -p $tdir
    pushd -q $tdir
    for sdir in $dirs; do
        [[ ! -e ../$sdir/$en ]] && continue

        echo "link ../$sdir/$en as $sdir"
        ln -s ../$sdir/$en $sdir
    done
    popd
done
