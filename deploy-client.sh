#!/bin/bash

./build.sh
cp -R src/Client/public/* ./gh-pages

echo "Go to gh-pages and commit -> push"
