#!/bin/sh

set -e

if [ -f .gtags.ignore ]; then
    git ls-files | grep -vf .gtags.ignore | gtags --gtagslabel=pygments -i -v -f-
else
    git ls-files | gtags --gtagslabel=pygments -i -v -f-
fi
