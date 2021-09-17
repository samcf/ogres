#!/bin/bash
src="$PWD"
dst=$(mktemp -d -t ogre.tools)

# build
npm run build

# checkout
git clone --single-branch --branch gh-pages git@github.com:samcf/ogre.tools.git "$dst"

# copy, commit, and push
cp -r web/. "$dst"/
cd "$dst" || exit
git add --all
git commit --allow-empty --amend -m "Publish changes."
git push --force-with-lease origin gh-pages

# return
cd "$src" || exit
rm -rf "$dst"
