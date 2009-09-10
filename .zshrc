#! /bin/zsh -f

ZSH_CONF_DIR=~/.zsh
ZSH_CONF_FILES=(
    contrib
    functions
	env
	aliases
	apt-get
	prompt
	options
	modules
	completion
	zle
	config
)

for conf_file in $ZSH_CONF_FILES; do
	source $ZSH_CONF_DIR/$conf_file;
done
