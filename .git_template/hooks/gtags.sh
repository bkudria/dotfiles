#!/bin/sh

set -e

if [ -f .gtags.ignore ]; then
    git ls-files | grep -vf .gtags.ignore | /usr/local/bin/gtags --gtagslabel=pygments -i -q -f-
else
    git ls-files | /usr/local/bin/gtags --gtagslabel=pygments -i -q -f-
fi
