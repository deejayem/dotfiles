#!/bin/sh

SRC_DIR=${1:-${HOME}/src}

[ -d "${SRC_DIR}" ] || mkdir -p "${SRC_DIR}"
cd "${SRC_DIR}"

[ -d ./scope-capture ] || git clone git@github.com:deejayem/scope-capture.git
cd ./scope-capture
lein do pom, jar, install

