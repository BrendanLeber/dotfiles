#!/bin/bash

wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -

sudo apt install clang-3.9 clang-3.9-doc libclang-common-3.9-dev libclang-3.9-dev libclang1-3.9 libclang1-3.9-dbg libllvm-3.9-ocaml-dev libllvm3.9 libllvm3.9-dbg lldb-3.9 llvm-3.9 llvm-3.9-dev llvm-3.9-doc llvm-3.9-examples llvm-3.9-runtime clang-format-3.9 python-clang-3.9 liblldb-3.9-dev liblldb-3.9-dbg

CLANG_VERSION=3.9

for FILE in /usr/bin/*-$CLANG_VERSION
do
    NEWFILE="$(echo "$FILE" | sed 's/-'$CLANG_VERSION'$//')"
    sudo update-alternatives --install "$NEWFILE" "$(basename "$NEWFILE")" "$FILE" 1000
done
