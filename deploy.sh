#!/usr/bin/env bash
set -euo pipefail

echo "==> Building optimised JS (fullLinkJS)..."
sbt fullLinkJS

OPT_JS="target/scala-3.3.7/toddler-calm-app-opt/main.js"
if [ ! -f "$OPT_JS" ]; then
  echo "ERROR: expected $OPT_JS — did sbt fullLinkJS succeed?" >&2
  exit 1
fi

echo "==> Assembling docs/"
rm -rf docs
mkdir -p docs

cp manifest.json docs/
cp sw.js         docs/

if [ -d assets ]; then
  cp -r assets docs/
fi

cp "$OPT_JS" docs/main.js

# Patch index.html: rewrite the script src to the flat docs/ path
sed 's|src="target/scala-3.3.7/toddler-calm-app-fastopt/main.js"|src="main.js"|' \
  index.html > docs/index.html

echo "==> docs/ is ready"
echo ""
echo "GitHub Pages setup (one-time):"
echo "  Settings -> Pages -> Source: Deploy from branch, branch: main, folder: /docs"
echo ""
echo "To deploy:"
echo "  git add docs && git commit -m 'deploy' && git push"