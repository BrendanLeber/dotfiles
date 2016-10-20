#!/bin/bash

set -e

sudo apt-add-repository ppa:dansmith/chirp-snapshots
sudo apt update
sudo apt install chirp-daily
