#!/usr/bin/env bash
set -o errexit
set -o pipefail
set -o nounset

src="$PWD"
dst=$(mktemp -d -t ogre.tools)
vrs=$1

npx shadow-cljs release app --config-merge "{:closure-defines {ogre.tools.state/VERSION \"$vrs\" ogre.tools.state/PATH \"/release/$vrs\"}}"

git clone --single-branch --branch gh-pages git@github.com:samcf/ogre.tools.git "$dst"
mkdir -p "$dst"/release/"$vrs"
cp -r web/release/. "$dst"/release/"$vrs"

cd "$dst"/release/"$vrs"
rm -rf cljs-runtime manifest.edn
git add --all
git commit -m "Release candidate version $vrs."
git push origin gh-pages

cd "$src"
rm -rf "$dst"
