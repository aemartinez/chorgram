#!/bin/sh
sudo apt-get install python3-pip graphviz ghc libghc-missingh-dev libghc-split-dev ipandoc libghc-hxt-dev python3-numpy python3-scipy
make setup
pip3 install networkx==2.3
echo "REady"
