#!/bin/sh

./rebar compile
rsync -r --delete-after --exclude=".git" --exclude="*.beam" --exclude="ebin" --exclude="*.o" --exclude="deps" --exclude="dev" --exclude="rel/trp" --exclude="makefile" --exclude="rel/vars" . ~/work/autodeploy/trp
cd ~/work/autodeploy/trp
git add .
git commit -am "deploy"
git push origin master
