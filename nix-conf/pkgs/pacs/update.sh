#!/usr/bin/env nix-shell
#!nix-shell -i bash -p coreutils curl gnugrep gnused jq nodejs_24 pnpm_9 nixfmt
#shellcheck shell=bash

set -eu -o pipefail

force=0
while getopts ":f" opt; do
    case "$opt" in
        f) force=1 ;;
        *) exit 2 ;;
    esac
done

dir="$(cd "$(dirname "$0")" && pwd)"
derivation="$dir/default.nix"
lockfile="$dir/pnpm-lock.yaml"
netrc="$HOME/.config/nix/netrc"

set_pnpm_deps_hash() {
    local newHash="$1"
    sed -i \
        '/^[[:space:]]*pnpmDeps[[:space:]]*=[[:space:]]*fetchPnpmDeps[[:space:]]*{/,/^[[:space:]]*};/ {
      s|^[[:space:]]*hash = "[^"]*";|    hash = "'"$newHash"'";|
    }' \
        "$derivation"
}

metadata="$(curl -sS --netrc-file "$netrc" "https://npm.pkg.github.com/@adzerk%2Fpacs-client")"
latestVersion="$(jq -r '.["dist-tags"].latest' <<<"$metadata")"
if [[ -z "$latestVersion" || "$latestVersion" == "null" ]]; then
    echo "error: couldn't find latest version from repository metadata" >&2
    exit 1
fi

currentVersion="$(sed -nE 's/^[[:space:]]*version = "([^"]+)".*/\1/p' "$derivation" | head -n1)"
if [[ "$latestVersion" == "$currentVersion" && "$force" -eq 0 ]]; then
    echo "pacs-client is already up-to-date: $currentVersion"
    exit 0
fi

tarballUrl="$(jq -r --arg v "$latestVersion" '.versions[$v].dist.tarball' <<<"$metadata")"
if [[ -z "$tarballUrl" || "$tarballUrl" == "null" ]]; then
    echo "error: couldn't find tarball URL for version $latestVersion" >&2
    exit 1
fi

downloadId="$(sed -n 's|.*/'"$latestVersion"'/\([^/?]*\).*|\1|p' <<<"$tarballUrl")"
narHash="$(nix store prefetch-file --json --unpack "$tarballUrl" | jq -r '.hash')"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

curl -fsSL --netrc-file "$netrc" "$tarballUrl" -o "$tmp/pkg.tgz"
tar -xzf "$tmp/pkg.tgz" -C "$tmp"

if [[ ! -f "$tmp/package/package.json" ]]; then
    echo "error: package/package.json missing from tarball" >&2
    exit 1
fi

cd "$tmp/package"
export HOME="$tmp/home"
pnpm install --lockfile-only --prod
cp -f pnpm-lock.yaml "$lockfile"

echo "Updating $derivation ..."

sed -i \
    -e "s|^[[:space:]]*version = \"[^\"]*\";|  version = \"${latestVersion}\";|g" \
    -e "s|^[[:space:]]*downloadId = \"[^\"]*\";|  downloadId = \"${downloadId}\";|g" \
    -e "s|^[[:space:]]*narHash = \"[^\"]*\";|    narHash = \"${narHash}\";|g" \
    "$derivation"

# Force mismatch to get correct pnpmDeps.hash
set_pnpm_deps_hash "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="

set +e
out="$(cd "$dir/../.." && nix build -L --no-link .#additions.pacs.pnpmDeps 2>&1)"
status=$?
set -e

pnpmHash="$(grep -oE 'got:[[:space:]]*sha256-[A-Za-z0-9+/=]+' <<<"$out" | head -n1 | sed -E 's/got:[[:space:]]*//')"

if [[ -z "$pnpmHash" ]]; then
    echo "error: couldn't extract 'got: sha256-...' from nix build output (exit $status)" >&2
    echo "$out" | tail -n 120 >&2
    exit 1
fi

set_pnpm_deps_hash "$pnpmHash"

nixfmt "$derivation"

echo "Updated pacs-client: $currentVersion -> $latestVersion"
