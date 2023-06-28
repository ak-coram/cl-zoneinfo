#!/bin/sh

git submodule update --init --recursive
cd tz
git checkout $1
cd ../
echo $1 > TZ_RELEASE
