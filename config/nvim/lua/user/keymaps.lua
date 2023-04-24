vim.g.mapleader = ','
vim.g.maplocalleader = ','

vim.keymap.set('n', '<Leader>w', ':w<CR>')
vim.keymap.set('n', '<Leader>q', ':q<CR>')
vim.keymap.set('n', 'q:', ':q<CR>')
vim.keymap.set('n', '<Leader>v', ':vs<CR>')
vim.keymap.set('n', '<Leader>h', ':sp<CR>')

vim.keymap.set('n', '<Leader>k', ':nohlsearch<CR>')

vim.keymap.set('n', '<Leader>ek', ':e ~/.config/nvim/lua/user/keymaps.lua<CR>')
vim.keymap.set('n', '<Leader>ep', ':e ~/.config/nvim/lua/user/plugins.lua<CR>')

-- Reselect after indent
vim.keymap.set('v', '<', '<gv')
vim.keymap.set('v', '>', '>gv')

-- Replace text without losing the clipboard
vim.keymap.set('v', 'p', '"_dP')

-- When text is wrapped, move by terminal rows, not lines, unless a count is provided.
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true })

-- Make wildmenu behave more normally
vim.keymap.set('c', '<Up>', [[(wildmenumode() ? "\<C-p>" : "\<Up>")]], { expr = true })
vim.keymap.set('c', '<Down>', [[(wildmenumode() ? "\<C-n>" : "\<Down>")]], { expr = true })

-- Telescope
vim.keymap.set('n', '<C-p>', [[<cmd>lua require('telescope.builtin').find_files()<CR>]])
vim.keymap.set('n', '<Space>fr', [[<cmd>lua require('telescope.builtin').oldfiles()<CR>]])
