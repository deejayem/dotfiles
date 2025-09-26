#!/usr/bin/env bash

# Make sure node2nix is available
if ! command -v node2nix &> /dev/null; then
    echo "node2nix is not available. Install it with: nix-shell -p nodePackages.node2nix"
    exit 1
fi

# Generate the nix expressions
node2nix \
    --input node-packages.json \
    --output node-packages.nix \
    --composition composition.nix \
    --node-env node-env.nix \
    --pkg-name nodejs_22

echo "Generated node packages. You can now use:"
echo "  nix-build -A cdktf-cli"
