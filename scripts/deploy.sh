#!/usr/bin/env bash

set -e

# Clean

rm public/app.js

# Build

./scripts/build-prod.sh src/Main.elm

# Deploy

path="deploy"

git worktree add $path gh-pages
rm -rf $path/*
cp public/* $path
git -C $path add .
if git -C $path commit -m "Deploy $(git log -n 1 --format='%h' master)"; then
  git -C $path push -u origin HEAD
fi
git worktree remove --force $path
echo "Success!"
