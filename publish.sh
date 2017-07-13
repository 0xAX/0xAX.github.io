#!/bin/bash

ssh-add
hugo
cp -r public/* ../my-blog-master/
cd ../my-blog-master/
git add .
git commit -m "update master"
git push origin master
