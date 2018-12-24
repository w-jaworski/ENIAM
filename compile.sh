#!/bin/bash

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

cd coordination
make clean
sudo make install
make clean
make interface
sudo mv coordination /usr/local/bin/coordination
make clean
cd ..

cd theories
sudo make install
cd ..
