#!/bin/bash

#wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -

CLANG_VERSION=5.0

sudo apt install clang-$CLANG_VERSION clang-$CLANG_VERSION-doc libclang-common-$CLANG_VERSION-dev libclang-$CLANG_VERSION-dev libclang1-$CLANG_VERSION libclang1-$CLANG_VERSION-dbg libllvm-$CLANG_VERSION-ocaml-dev libllvm3.9 libllvm3.9-dbg lldb-$CLANG_VERSION llvm-$CLANG_VERSION llvm-$CLANG_VERSION-dev llvm-$CLANG_VERSION-doc llvm-$CLANG_VERSION-examples llvm-$CLANG_VERSION-runtime clang-format-$CLANG_VERSION python-clang-$CLANG_VERSION liblldb-$CLANG_VERSION-dev liblldb-$CLANG_VERSION-dbg clang-tidy-$CLANG_VERSION

for FILE in /usr/bin/*-$CLANG_VERSION
do
    NEWFILE="$(echo "$FILE" | sed 's/-'$CLANG_VERSION'$//')"
    sudo update-alternatives --install "$NEWFILE" "$(basename "$NEWFILE")" "$FILE" 1000
    sudo update-alternatives --set "$(basename "$NEWFILE")" "$FILE"
done
