--
--                            ⢸⣦⡈⠻⣿⣿⣿⣶⣄
--                            ⢸⣿⣿⣦⡈⠻⣿⣿⣿⣷⣄
--                      ⣀⣀⣀⣀⣀⣀⣼⣿⣿⣿⣿ ⠈⠻⣿⣿⣿⣷⣄
--                      ⠈⠻⣿⣿⣿⣿⣿⡿⠿⠛⠁   ⠈⠻⢿⣿⣿⣷⣄
--
-- Personal Neovim configuration of Jess Archer <jess@jessarcher.com>

vim.g.mapleader = ','

vim.keymap.set('n', '<Leader>w', ':w<CR>')
vim.keymap.set('n', '<Leader>q', ':q<CR>')

vim.keymap.set('n', '<Leader>v', ':vs<CR>')
vim.keymap.set('n', '<Leader>h', ':sp<CR>')

vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
