#!/bin/bash

./build.sh
cp -R src/Client/public ./
cd public
cls
git status
