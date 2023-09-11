local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

return function(plugins)
    local lazy_setup = {}

    for _, plugin_name in ipairs(plugins) do
        table.insert(lazy_setup, {import = "plugins." .. plugin_name})
    end

    require("lazy").setup(lazy_setup)
end
