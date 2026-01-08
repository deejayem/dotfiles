#!/bin/sh

git remote add sourcehut git@git.sr.ht:~djm/dotfiles
git remote add codeberg git@codeberg.org:djm/dotfiles.git
git remote add tildegit git@tildegit.org:djm/dotfiles.git
git remote add tilde.institute djm@tilde.institute:public_repos/dotfiles
git remote add github git@github.com:deejayem/dotfiles.git

for i in `git remote -v | grep -v origin | awk '{print $2}' | uniq`; do git remote set-url --add --push origin $i ; done

