#!/bin/bash

set -e
set -u

PRIORITY=500

# install packages
sudo apt install clang-5.0 llvm-5.0 lld-5.0 clang-tools-5.0 clang-format-5.0 clang-tidy-5.0 # lldb-5.0

# binaries to install as alternatives
declare -a pkgs=("clang-5.0"
                 "clang++-5.0"
                 "lld-5.0"
#                "lldb-5.0"
#                "lldb-mi-5.0"
                 "llvm-cov-5.0"
                 "llvm-profdata-5.0"
                 "clangd-5.0"
                 "clang-format-5.0"
                 "clang-tidy-5.0")

for PKG in "${pkgs[@]}"
do
    FILE=$(ls /usr/bin/$PKG)

    LINK="$(echo "$FILE" | sed 's/-'6.0'$//')"
    NAME="$(basename "$LINK")"
    FPATH="$FILE"
       
    sudo update-alternatives --install $LINK $NAME $FPATH $PRIORITY
done

# install alternatives for cc and c++ that point to clang and clang++
sudo update-alternatives --install /usr/bin/cc cc /usr/bin/clang-5.0 $PRIORITY
sudo update-alternatives --install /usr/bin/c++ c++ /usr/bin/clang++-5.0 $PRIORITY
