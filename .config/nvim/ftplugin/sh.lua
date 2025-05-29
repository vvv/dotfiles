-- Expand Tab to 4 spaces
--
-- See also:
-- * https://www.reddit.com/r/neovim/comments/13uvsw6/how_can_i_set_my_tab_key_to_be_4_spaces_indent/
-- * https://stackoverflow.com/questions/1878974/redefine-tab-as-4-spaces

vim.opt_local.expandtab = true
vim.opt_local.tabstop = 4
vim.opt_local.shiftwidth = 4
vim.opt_local.formatoptions:append({ c = true, r = true, o = true, q = true })
