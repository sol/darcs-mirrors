#!/bin/bash

set -o nounset
set -o errexit

name="$1"
url="$2"

base_dir="$(dirname "$(readlink -f "$0")")"

darcs_to_git="$base_dir/darcs-to-git/darcs-to-git"

repos="$base_dir/repositories"
mkdir -p "$repos"

darcsdir="$repos/$name.darcs"
gitdir="$repos/$name"

# get/update darcs repo
if [ -d "$darcsdir" ]; then
    cd "$darcsdir" && darcs pull
else
    darcs get "$url" "$darcsdir"
fi

# create/update git repo
if [ ! -d "$gitdir" ]; then
    mkdir -p "$gitdir"
    cd "$gitdir"
    git init
    git remote add origin git@github.com:darcs-mirrors/$name.git
fi

cd "$gitdir" && "$darcs_to_git" "$darcsdir"
git push origin master
