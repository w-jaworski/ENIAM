#!/bin/bash

cd semantics
make clean
sudo make install
make clean
cd ..

cd exec
make clean
sudo make install
make eniam
sudo mv eniam /usr/local/bin/eniam
make distriparser
sudo mv distriparser /usr/local/bin/distriparser
make clean
cd ..

cd theories
sudo make install
cd ..
