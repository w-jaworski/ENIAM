#!/bin/bash

cd fuzzyAnalyzer
make clean
sudo make install
make clean
cd ..

cd morphology
make clean
sudo make install
make interface
sudo mv morphology /usr/local/bin/morphology
make clean
cd ..

cd subsyntax
make clean
sudo make install
make clean
make interface
sudo mv subsyntax /usr/local/bin/subsyntax
make clean
cd ..

# cd coordination
# make clean
# sudo make install
# make clean
# make interface
# sudo mv coordination /usr/local/bin/coordination
# make clean
# cd ..

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
make validator
sudo mv validator /usr/local/bin/validator
make distriparser
sudo mv distriparser /usr/local/bin/distriparser
make clean
cd ..

cd theories
sudo make install
cd ..

cd lexcreator
make clean
sudo make
sudo mv lexcreator /usr/local/bin/lexcreator
sudo mv verse_worker /usr/local/bin/verse_worker
make clean
cd ..
