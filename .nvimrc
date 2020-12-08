nnoremap <leader>h :!~/.nix-profile/bin/hasktags -o tags -c $(find lib server daemon test -name \*.hs) && ~/.nix-profile/bin/ctags --options-maybe=.ctags --options=$HOME/.ctags --append=yes .<CR><CR>

nnoremap <leader>o :!~/.nix-profile/bin/ormolu --mode inplace $(find lib server daemon test -name '*.hs')<CR><CR>


