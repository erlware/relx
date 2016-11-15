#!/usr/bin/env sh

if [ -z $1 ]
then
    echo "pr2relnotes.sh: prints list of pull requests merged since <tag>"
    echo "  usage: $0 <tag> [pull-request-url (default: https://github.com/erlware/relx/pull/)]"
    exit 0
fi
export url=${2:-"https://github.com/erlware/relx/pull/"}

git log --merges --pretty=medium $1..HEAD | \
awk -v url=$url '
    # first line of a merge commit entry
    /^commit / {mode="new"}

    # merge commit default message
    / +Merge pull request/ {
        page_id=substr($4, 2, length($4)-1);
        mode="started";
        next;
    }

    # line of content including title
    mode=="started" && /    [^ ]+/ {
        print "- [" substr($0, 5) "](" url page_id ")"; mode="done"
    }'
