#!/bin/bash

set -e
set -u

PRIORITY=600

# install packages
sudo apt install clang-6.0 llvm-6.0 lld-6.0 lldb-6.0 clang-tools-6.0 clang-format-6.0 clang-tidy-6.0

# binaries to install as alternatives
declare -a pkgs=("clang-6.0"
	         "clang++-6.0"
		 "lld-6.0"
		 "lldb-6.0"
		 "lldb-mi-6.0"
		 "llvm-cov-6.0"
		 "llvm-profdata-6.0"
		 "clangd-6.0"
		 "clang-format-6.0"
		 "clang-tidy-6.0")

for PKG in "${pkgs[@]}"
do
    FILE=$(ls /usr/bin/$PKG)

    LINK="$(echo "$FILE" | sed 's/-'6.0'$//')"
    NAME="$(basename "$LINK")"
    FPATH="$FILE"
       
    sudo update-alternatives --install $LINK $NAME $FPATH $PRIORITY
done

# install alternatives for cc and c++ that point to clang and clang++
sudo update-alternatives --install /usr/bin/cc cc /usr/bin/clang-6.0 $PRIORITY
sudo update-alternatives --install /usr/bin/c++ c++ /usr/bin/clang++-6.0 $PRIORITY
