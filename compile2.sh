#!/bin/bash

cd LCGparser
make clean
sudo make install
make clean
cd ..

# cd xt
# make clean
# sudo make install
# make clean
# cd ..

# cd integration
# make clean
# sudo make install
# make clean
# cd ..

cd LCGlexicon
make clean
sudo make install
make clean
cd ..

cd valence
make clean
sudo make install
make clean
cd ..

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
