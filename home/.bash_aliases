alias gdb='gdb -q'
alias net='netstat -apnA inet'
alias blog="cd ~/Desarrollo/algui91-hugo/"
alias blogserve="HUGO_THEME=gohugo-amp hugo server -Dw --templateMetrics --log --navigateToChanged"

# Some latex Aliases
#alias xelatex='xelatex -shell-escape -output-directory=metafiles'
alias mlatexmk='latexmk -shell-escape -pdf -pvc -output-directory=metafiles'
alias mlatexmkx='latexmk -shell-escape -xelatex -pdf -pvc -output-directory=metafiles'

# Emerge things
alias ucu='sudo emerge --jobs 4 --update --deep --with-bdeps=y --changed-use -atnv @world'
alias unu='sudo emerge --jobs 4 --update --deep --with-bdeps=y --newuse -atnv @world'
alias urd='sudo emerge --jobs 4 --update --newuse --deep -atnv @world'
alias depclean='sudo emerge --jobs 4 --depclean -atn'
alias eqf='equery f'
alias equ='equery u'
alias eqh='equery h'
alias eqa='equery a'
alias eqb='equery b'
alias eql='equery l'
alias eqd='equery d'
alias eqg='equery g'
alias eqc='equery c'
alias eqk='equery k'
alias eqm='equery m'
alias eqy='equery y'
alias eqs='equery s'
alias eqw='equery w'

# alias attdebug='echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope'
alias fuck='sudo $(history -p \!\!)'

# Misc
alias optipng='optipng -o9 -i0'
alias youtube-dl-audio='youtube-dl --extract-audio --audio-format mp3  --audio-quality 0'
alias youtube-dl-video="youtube-dl -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best'"
alias imgsize='identify -format "width=\"%[fx:w]\" height=\"%[fx:h]\"\n"'
alias arandr='arandr --force-version'
alias mplayerhdmi='mplayer -ao alsa:device=hdmi'
alias subliminalen='subliminal download -l en'
alias undrive='cd /tmp/; java -jar ~/Desarrollo/cryptomator/main/uber-jar/target/Cryptomator-1.3.2.jar'
alias dav="mount ~/dav"
alias undav="umount ~/dav"
alias dirs="dirs -v"
alias reducepdf="gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook"

alias rd='rm -rfv'
alias cp='cp -v'
alias mv='mv -v'
alias rm='rm -v'
alias shred='shred -zvu'
alias grepr='grep -ri'
alias locate='locate -i'
alias e='emacsclient -t'
alias ec='emacsclient -c'
alias vim='emacsclient -t'
alias vi='emacsclient -t'

# Git misc
alias st='git st'
alias cm='git cm'
alias gp='git push'
alias glol='git lol'
alias gld='git ld'
alias glol10='git --no-pager lol -n 10'
alias gld10='git --no-pager ld -n 10'

source ~/.fonts/octicons-regular.sh
source ~/.fonts/pomicons-regular.sh
source ~/.fonts/fontawesome-regular.sh

USER='Alejandro Alcalde'

RBW_PATH=$HOME/.rainbow-bash
source $RBW_PATH/init.sh
rbw_load_theme simple

export PATH=${PATH}:~/bin
export PATH=${PATH}:/home/hkr/bin/texlive/bin/x86_64-linux
export PATH=${PATH}:/opt/cisco/anyconnect/bin/
export PATH=${PATH}:~/Desarrollo/android-sdk-linux/platform-tools
export PATH=~/.npm-global/bin:$PATH
export PATH=${PATH}:~/go/bin/
export PATH=${PATH}:~/.gem/ruby/2.2.0/bin/

export GREP_COLORS="mc=00;36:ms=31:mt=01;38"
export TERM=xterm-256color
export EDITOR="emacsclient"

wmname LG3D
