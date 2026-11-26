#!/bin/sh
# Usage: scripts/drom-unnest.sh   (in a project's directory, after drom)
#
# drom (0.9.3 and earlier) looks for a .git in the project's own directory
# only. In a project nested in a larger repository, `drom project` then
# runs `git init` there, and stages the files it generates in that new,
# empty repository. This script drops such a repository and stages the
# same files in the enclosing one, along with the files drom deleted.
# It does nothing in a project at the root of its repository.
set -eu

[ -d .git ] || exit 0
# Only a repository drom just created: one without any commit. A project
# at the root of its own repository stops here.
git --git-dir=.git rev-parse -q --verify HEAD >/dev/null && exit 0
# Only inside an enclosing repository.
(cd .. && git rev-parse --show-toplevel >/dev/null 2>&1) || exit 0

staged=$(mktemp)
trap 'rm -f "$staged"' EXIT
git --git-dir=.git ls-files -z > "$staged"
rm -rf .git
echo "$0: dropped the repository drom created; staging its files in the enclosing one"
if [ -s "$staged" ]; then
  xargs -0 git add -- < "$staged"
fi
# drom removes files it no longer generates: stage those deletions too.
git ls-files -z --deleted -- . > "$staged"
if [ -s "$staged" ]; then
  xargs -0 git rm --cached --quiet -- < "$staged"
fi
