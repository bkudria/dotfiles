#!/usr/bin/env bash

if [[ -f /etc/bash_completion ]]; then
	. /etc/bash.bashrc
fi

export PATH=$PATH:/home/bkudria/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/games:/var/lib/gems/1.8/bin:/usr/lib/kde4/bin
OS=""
DISTRO=""
LOCATION=""

OS=`uname`

[[ -e /etc/issue ]] && DISTRO=`cat /etc/issue.net | head -n 1 | awk '{print $1 }'`

if [[ "`domainname 2> /dev/null`" = "prvt.nytimes.com" ]] || [[ "`hostname -d`" = "prvt.nytimes.com" ]]; then
	LOCATION="NYT"
fi

bind "set editing-mode emacs"
bind "set keymap emacs"
bind "set completion-ignore-case on"
bind "set match-hidden-files off"
bind "set show-all-if-ambiguous on"
bind "set visible-stats on"

set -o notify
set -o emacs
set -o noclobber

# Distro/OS specific setters
if [[ "$DISTRO" == "Debian" ]] || [[ "$DISTRO" == "Ubuntu" ]]; then
	RCDIR="/etc/init.d/"
fi

if [ "$OS" = "FreeBSD" ]; then
	LS_OPTIONS='-G'
	RCDIR="/usr/local/etc/rc.d/ /etc/rc.d/"
fi

if [[ "$OS" = "Linux" ]]; then
	LS_OPTIONS='--color=auto'
fi


function qwhich {
if [[ "$DISTRO" = "CentOS" ]]; then
	which $@ 2> /dev/null
else
	which $@
fi
}

if [[ "$LOCATION" = "NYT" ]]; then
	export CVSROOT=":ext:bkudria@cvs.prvt.nytimes.com:/nytd/nytimes/src/cvs"
	export CVS_RSH="ssh"
	export SVN="http://svn.prvt.nytimes.com/svn"
fi

if [[ -x `qwhich hosts.rb` ]]; then
	alias hosts='hosts.rb'
fi

if [[ -x `qwhich hosts` ]]; then
	alias hosts='hosts'
fi

# User-dependent Settings
if [[ -x `qwhich whoami` ]] && [[ "`whoami`" -eq "root" ]]; then
	umask 022
	export TMOUT=1200
else
	umask 033
fi

if [[ -x `qwhich inotail` ]]; then
	alias tail='inotail'
fi

if [[ -x `qwhich bing` ]]; then
	alias bandwidth='s bing localhost'
fi

if [[ -x `qwhich keychain` ]] && [[ $UID -ne 0 ]]; then
	eval `keychain --inherit any-once --eval -Q -q --ignore-missing identity 209BE410`
fi

# General
	# Functions
		function hotpluginfo { udevinfo -a -p `udevinfo -q path -n $1`; }

		function ifupdown { sudo ifdown $1; sudo ifup $1; }

		function restart { killall $1; sleep 1s; $1; }

		function common { history | awk '{print $9}' | awk 'BEGIN {FS="|"}{print $1}' | sort | uniq -c | sort -n | tail -n 20 | sort -nr ;}

		function service { sudo $RCDIR$1 $2; }

		function mcd { mkdir "$@" && cd "${!#}"; }

		function cdl { cd "$@" && ls; }

		function psg { ps aux | grep "[${1:0:1}]${1:1}"; }

		export HOSTCOLORCODE="245"

		if [[ -x `qwhich hosts` ]] && `qwhich hosts` &> /dev/null; then
				export HOSTS_TYPE='binary'
		else
			if [[ -x `qwhich hosts.rb` ]] && [[ -x `qwhich ruby` ]]; then
				export HOSTS_TYPE='script'
			else
				export HOSTS_TYPE='none'
			fi
		fi

		function hosts
		{
			if [[ $HOSTS_TYPE == "binary" ]]; then
				`qwhich hosts` "$@"
			else
				if [[ HOSTS_TYPE == 'script' ]]; then
					`qwhich hosts.rb` "$@"
				else
					return $HOSTCOLORCODE
				fi
			fi
		}

		
	# Aliases
		alias dotfiles='curl -sS http://ben.kudria.net/install-dotfiles | bash; bashrc'
		alias activeoptions='egrep -v "#|^ *$"'
		alias su='su -p'
		[[ -x `qwhich sudo` ]] && alias su='sudo -s'
		[[ ! -x `qwhich sudo` ]] && function sudo { su -p -c "$*"; }
		alias g='egrep'
		alias count='grep -c'
		alias p='ping'
		[[ -e `qwhich htop` ]] && alias top='htop'
		alias killall=pkill

		alias s='sudo'

		alias watchfile='tail -f'
		[[ -x `qwhich rlwrap` ]] && alias watchfile='rlwrap tail -f'

		alias syslog='sudo tail -f /var/log/everything/current'
		[[ -x `qwhich loco` ]] && alias syslog='sudo tail -f /var/log/everything/current | loco'
		alias netstat='sudo netstat -tpav'
		alias du='ncdu .'
		alias df='di -h'
		alias ps='ps auxwww'
		alias h='history'
		alias ?="h | g $*"
		alias ps?="psg"
		alias x='exit'
		alias leases='cat /var/lib/misc/dnsmasq.leases'
		alias irb='irb --readline -r irb/completion'
		alias ori='ri -T -f bs'
		alias ri='qri'
		alias cls='clear'
		alias md='mkdir'
		alias rd='rmdir'
		alias cd..='cd ..'
		alias up='cd ..'
		alias cdo='cd ..'
		alias ..='cd ..'
		alias ...='cd ../..'
		alias unix2dos='recode lat1..ibmpc'
		alias dos2unix='recode ibmpc..lat1'

		alias ssh='ssh -qAX'
		alias newssh='ssh -S none'

		alias more='less'
		alias airnet='sudo pon sunysb'
		alias hex="ruby -e 'printf(\"0x%X\n\", ARGV[0])'"
		alias dec="ruby -e 'printf(\"%d\n\", ARGV[0])'"
		alias bin="ruby -e 'printf(\"%bb\n\", ARGV[0])'"
		alias word="ruby -e 'printf(\"0x%04X\n\", ARGV[0])'"

		alias sml='rlwrap sml'
		alias smlnj='sml'
		alias uniq='sort | uniq'
		alias httphead="curl -I"

		# Security
		alias rm='rm -i'
		alias cp='cp -i'
		alias mv='mv -i'
		alias ln='ln -i'
		alias mkdir='mkdir -p'

		# Typos
		alias chmdo='chmod'
		alias sl='ls'
		alias sll='ls'
		alias lsl='ls'
		alias l='ls'
		alias tarx='tar x'
		alias maek='make'
		alias grpe='grep'
		alias gpre='grep'
		alias icfonfig='ifconfig'
		alias ifocnfig='ifconfig'
		alias e='$EDITOR'
		alias nano='$EDITOR'
		alias ss='sudo'
		alias se='sudo $EDITOR'
		alias vf='cd'
		alias vp='cp'
		alias nmv='mv'
		alias mann='man'
		alias nman='man'
		alias nmann='man'
		alias mb='mv'
		alias nmplayer='mplayer'
		alias les='less'
		alias bashrc='source ~/.bashrc'
		alias kate='kate -u'

		# Git
		alias gb='git branch'
		alias gba='git branch -a'
		alias gc='git commit -v -m'
		alias gca='git commit -v -a -m'
		alias gd='git diff | kompare -o -'
		alias gl='git pull'
		alias gp='git push'
		alias gst='git status'

		function status {
			if [ -d ".svn" ]; then
				svn status
			else
				git status
			fi
		}

		[[ -x `qwhich dog` ]] && alias cat='dog'
		alias c='cat'


	# Shell Options
		shopt -s extglob
		shopt -s progcomp
		shopt -s histappend
		shopt -s histreedit
		shopt -s histverify
		shopt -s cmdhist
		shopt -s lithist
		shopt -s cdspell
		shopt -s no_empty_cmd_completion
		shopt -s checkhash
		shopt -s hostcomplete
		shopt -s checkwinsize

	# Apt-Get/dpkg tools - only on Debian
	if [[ "$DISTRO" == "Debian" ]] || [[ "$DISTRO" == "Ubuntu" ]]; then

		alias aptitude='s aptitude -y'
		alias aget='aptitude install'
		alias afix='aget -f'
		alias aremove='aptitude remove'
		alias apurge='aptitude purge'
		alias aupdate='aptitude update'
		alias aupgrade='aptitude safe-upgrade'
		alias adistupgrade='aptitude dist-upgrade'
		alias aconf='s dpkg-reconfigure'
		alias ainstall='s dpkg -i'
		alias anew='aupdate; adistupgrade'
		alias astatus='apt-cache policy'
		alias ainfo='apt-cache show'
		alias adownload='aptitude download'
		alias arefresh='s dpkg --clear-avail && s dpkg --forget-old-unavail'
		alias afind='apt-cache search'
		alias areinstall='aptitude reinstall'
		alias afiles='wajig listfiles'

		function aversion { dpkg -l $@ |grep ii| awk '{ print $3 }'; }
		function acontents { dpkg-deb -c /var/cache/apt/archives/$1_`aversion $1`_*.deb; }
		function apin { sudo echo "$1 hold" | sudo dpkg --set-selections; }
		function aunpin { sudo echo "$1 install" | sudo dpkg --set-selections; }

		function asearch { apt-cache show `apt-cache search $@ | sed s/\ -\ .*//g` | grep -v Priority | grep -v Section | grep -v Installed-Size | grep -v Maintainer | grep -v Architecture | grep -v Version | grep -v Replaces | grep -v Depends | grep -v Suggests | grep -v Conflicts | grep -v Filename | grep -v Size | grep -v MD5sum | grep -v Source | grep -v Provides | grep -v Description ; }
	fi


	if [[ $HOSTS_TYPE != 'none' ]]; then
		export HASHSTRING="$HOSTNAME"
		
		if [[ $OS = "SunOS" ]]; then
			export HASHSTRING="`hostname`.`domainname`"
		fi

		if [[ $DISTRO = "CentOS" ]]; then
			export HASHSTRING="`hostname -f`"
		fi
		
		export HOSTCOLORCODE=$(hosts "$HASHSTRING")
	fi

	export HOSTCOLORESCAPE="\[\033[38;5;${HOSTCOLORCODE}m\]"

	# Prompt

	bash_prompt()
	{
		local N="\[\033[0m\]"    # unsets color to term's fg color

		local UC=$HOSTCOLORESCAPE # user's color

		local O="${UC}(${N}"
		local C="${UC})${N}"

		local D="${UC}-${N}"

		[ $UID -eq "0" ] && O="${UC}[${N}"
		[ $UID -eq "0" ] && C="${UC}]${N}"
		[ $UID -eq "0" ] && D="${UC}-${N}"


		local HO=$HOSTCOLORESCAPE
		local HC=$N

		PS1="${N}\n${O}${UC}\u${N}@${HO}\H${HC}:\w${C}${D}${O}\$?${C}${D}${O}"
		PS2="${D}${O}"
	}

	# Change title & prompt
	case "$TERM" in
	xterm*|rxvt*)
		PROMPT_COMMAND="echo -ne \"\033]0;${USER}@${HOSTNAME}:${PWD/$HOME/~}\007\""
		;;
	*)
		;;
	esac

	export PROMPT_COMMAND="history -a; ${PROMPT_COMMAND}"

	bash_prompt
	unset bash_prompt



# Misc
	# Environment variables
		export KDEDIR="/usr"
		export QTDIR="/usr/share/qt3"
		export PKG_CONFIG_PATH="/usr/lib"
		export KDE_NO_IPV6=true
		export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/X11R6/lib/pkgconfig.
		export GTK2_RC_FILES=$HOME/.gtkrc-2.0
		export XAUTHORITY=/home/bkudria/.Xauthority
		export HISTCONTROL=erasedups
		export HISTSIZE=200000
		export HISTFILESIZE=100000000
		export HISTIGNORE="&:ls:&&"
		export HISTTIMEFORMAT=%c%t
		export EDITOR="nano"
		export VISUAL="nano"
		[[ -x kate ]] && export VISUAL="kate"
		export GREP_COLOR="38;5;$HOSTCOLORCODE"
		export ACK_COLOR_MATCH="green"
		export ACK_COLOR_FILENAME="green"
		export GREP_OPTIONS="--color=auto"
		export CLICOLOR=1
		export PAGER=less
		export GPGKEY='209BE410'
		export DEBFULLNAME='Benjamin Kudria'
		export DEBEMAIL='ben@kudria.net'
		[[ -x `qwhich gem` ]] && export RUBYOPT='rubygems'

# 		[ -x /usr/bin/most ] && export PAGER=most && alias less=most

		export LESS="iRwm"
		export MOST_SWITCHES=""
		export CLASSPATH="$CLASSPATH:/usr/share/java/junit.jar:/usr/share/java/ant.jar"

		[[ "$TERM" == "xterm" ]] && [[ -x `qwhich setterm` ]] && setterm -blength 0 # Disable annoying beep (console)
		[[ -x `qwhich xset` ]] && [[ $DISPLAY != "" ]] && xset b off # disable annoying beep (X)
		ulimit -c unlimited        # No limits.
		[[ -x `qwhich lesspipe` ]] && eval `lesspipe`            # Allow less to view *.gz etc. files.


		# Completion

		# If available, source the global bash completion file.
		if [[ -f /etc/bash_completion ]]; then
			source /etc/bash_completion
		fi

		[[ $HOSTS_TYPE != 'none' ]] && HOSTS_COMPLETE=$(hosts -h)
		[[ -x `qwhich snippit` ]] && SNIPPIT_COMPLETE=$(snippit list)

# 		complete -A alias         alias unalias
		complete -A command       which qwhich s sudo tsocks
		complete -A export        export printenv
		complete -A hostname      ssh telnet ftp ncftp ping dig nmap scp
		[[ $HOSTS_TYPE != 'none' ]] && complete -A hostname -o default -W "${HOSTS_COMPLETE[*]}" ssh telnet ftp ncftp ping dig nmap scp p
		complete -W "${SNIPPIT_COMPLETE[*]}" snippit ks
# 		complete -A helptopic     help
# 		complete -A job -P '%'    fg jobs
# 		complete -A setopt        set
# 		complete -A shopt         shopt
		complete -A signal        kill killall
# 		complete -A user          su userdel passwd
# 		complete -A group         groupdel groupmod newgrp
		complete -A directory     cd rmdir
		complete -cf              sudo s

		complete -f -X '!*.@(mp?(e)g|MP?(E)G|wma|avi|AVI|asf|vob|VOB|bin|dat|vcd|ps|pes|fli|viv|rm|ram|yuv|mov|MOV|qt|QT|wmv|mp3|MP3|ogg|OGG|ogm|OGM|mp4|MP4|wav|WAV|asx|ASX|mng|MNG|m4v)' mplayer

		# Service RC.d completion
		SERVICE_COMPLETE=( $(`qwhich ls` $RCDIR | grep -v "\:" | uniq ) )
		complete -A service -o default -W "${SERVICE_COMPLETE[*]}" service

		# Colorized ls, with options
		[[ -x `qwhich dircolors` ]] && eval `dircolors`
		LS_COLORS="$LS_COLORS*.JPG=01;35:*.GIF=01;35:*.jpeg=01;35:*.pcx=01;35:*.png=01;35:*.pnm=01;35:*.bz2=01;31:*.mpg=01;38:*.mpeg=01;38:*.MPG=01;38:*.MPEG=01;38:*.m4v=01;038:*.mp4=01;038:*.swf=01;038:*.avi=01;38:*.AVI=01;38:*.wmv=01;38:*.WMV=01;38:*.asf=01;38:*.ASF=01;38:*.mov=01;38:*.MOV=01;38:*.mp3=01;39:*.ogg=01;39:*.MP3=01;39:*.Mp3=01;39"


		export LS_OPTIONS="$LS_OPTIONS -p -l -h"
		alias ls='ls $LS_OPTIONS'

		# make less more friendly for non-text input files, see lesspipe(1)
		[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

		# Terminal settings
		if ! [[ $TERM ]] ; then
			eval `tset -s -Q`
			case $TERM in
				con*|vt100|linux|xterm)
					tset -Q -e ^?
				;;
			esac
		fi

		# Import local/private settings
		if [[ -f ~/.bashrc.local ]]; then
			. ~/.bashrc.local
		fi

