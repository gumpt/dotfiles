export ATUIN_NOBIND="true"
eval "$(atuin init zsh)"
bindkey '^r' _atuin_search_widget

alias reloadzsh='. ~/.zshrc'
alias bell='print "\a"'
alias clipsort='pbpaste | sort | pbcopy'
alias emptycommit='git commit --allow-empty -m "this is empty!"'

alias ll='ls -l'
alias tmxu=tmux

alias sep='fortune | cowsay -f dragon-and-cow | lolcat'
alias gg='git ls-files | grep'
alias kctl=kubectl
alias k=kctl

export gpsu() {
    git push --set-upstream origin $(git branch --show-current)
}
export wb() {
    ($("$@") && bell) || bell
}

eval "$(rbenv init - zsh)"
. "$HOME/.cargo/env"

ssh-add ~/.ssh/id_ed25519

export PATH=$PATH:~/git/perf-tools/bin
export PATH=$PATH:~/git/FlameGraph
export PATH=/usr/local/bin:$PATH # x64 homebrew


# Fun!
fortune | cowsay | lolcat
