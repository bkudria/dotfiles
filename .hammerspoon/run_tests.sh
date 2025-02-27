#!/bin/bash

set -euo pipefail

yue -r spec
sed -i '.bak' '1s/^/require("busted.runner")()/g' spec/*.lua
unbuffer lua spec/*.lua | sed 's/\.lua/\.yue/g'
