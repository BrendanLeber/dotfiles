#!/bin/bash

# install packages
sudo apt install clang-3.9 clang-3.9-doc llvm-3.9 llvm-3.9-doc clang-format-3.9 clang-tidy-3.9

# binaries to add to alternatives
declare -a pkgs=("clang-3.9"
	         "clang++-3.9"
		 "clang-format-3.9"
		 "clang-tidy-3.9")

for PKG in "${pkgs[@]}"
do
    FILE=$(ls /usr/bin/$PKG)
    NEWFILE="$(echo "$FILE" | sed 's/-'3.9'$//')"
   
    sudo update-alternatives --install "$NEWFILE" "$(basename "$NEWFILE")" "$FILE" 390
    # sudo update-alternatives --set "$(basename "$NEWFILE")" "$FILE"
done
