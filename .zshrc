#! /bin/zsh -f

ZSH_CONF_DIR=~/.zsh
ZSH_CONF_FILES=(
    functions
    contrib
    env
    aliases
    prompt
    options
    modules
    completion
    zle
    config
    highlighting
)

for conf_file in $ZSH_CONF_FILES; do
	source $ZSH_CONF_DIR/$conf_file;
done
