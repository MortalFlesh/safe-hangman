#!/bin/bash

./build.sh
cp -R src/Client/public ./
cd public
clear
git status
