#!/usr/bin/env bash
set -o errexit
set -o pipefail
set -o nounset

src="$PWD"
dst=$(mktemp -d -t ogre.tools)
vrs=$1

git clone --single-branch --branch gh-pages git@github.com:samcf/ogre.tools.git "$dst"

rm -rf "$dst"/release/latest
mkdir -p "$dst"/release/latest
cp -r "$dst"/release/"$vrs"/. "$dst"/release/latest

cd "$dst"
git add --all
git commit -m "Publish version $vrs as the latest release."
git push origin gh-pages

cd "$src"
rm -rf "$dst"
