return {
    'nvim-telescope/telescope.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    keys = {
        { "<C-p>", "<cmd>Telescope git_files<cr>", desc = "FindFiles" },
        { "<space>sp", "<cmd>Telescope live_grep<cr>", desc = "Project Search" },
        { "<space>fr", "<cmd>Telescope oldfiles<cr>", desc = "Recent Files" },
    }
}
