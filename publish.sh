#!/bin/bash

ssh-add
hugo
mkdir -p ../my-blog-master
cp -r public/* ../my-blog-master/
cd ../my-blog-master/
git add .
git commit -m "update master"
git push origin master
