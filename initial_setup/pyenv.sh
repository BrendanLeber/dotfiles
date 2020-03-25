#!/bin/bash

sudo apt update
sudo apt upgrade
sudo apt install build-essential

# setup pyenv and plugins
git clone https://github.com/pyenv/pyenv.git ~/.pyenv
git clone https://github.com/pyenv/pyenv-doctor.git ~/.pyenv/plugins/pyenv-doctor
git clone https://github.com/pyenv/pyenv-update.git ~/.pyenv/plugins/pyenv-update
git clone https://github.com/pyenv/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv

# setup python and neovim plugins
pyenv install 2.7.17
pyenv virtualenv 2.7.17 neovim2
pyenv activate neovim2
pip install --upgrade neovim
pip install --upgrade flake8
echo let g:python_host_prog = `pyenv which python`
pyenv deactivate

pyenv install 3.8.2
pyenv virtualenv 3.8.2 neovim3
pyenv activate neovim3
pip install --upgrade neovim
pip install --upgrade flake8
echo let g:python3_host_prog = `pyenv which python`
ln -s `pyenv which flake8` ~/.local/bin/flake8
pyenv deactivate
