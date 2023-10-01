# siclsp
An LSP for SIC source files. Here's how I have it configured for neovim:

![image](https://github.com/pmsanford/sicsim-rs/assets/1696007/b30975a6-bd39-4eff-bec3-158fbda54282)

```lua
vim.filetype.add({
  extension = { sic = 'sic' }
})

vim.api.nvim_create_autocmd('FileType', {
  desc = "attach sic lsp",
  pattern = "sic",
  callback = function(opts) 
    vim.api.nvim_set_hl(0, '@lsp.type.operator', { fg='Gold' })
    vim.api.nvim_set_hl(0, '@lsp.type.keyword', { fg='LightBlue' })
    vim.api.nvim_set_hl(0, '@lsp.type.number', { fg='DarkOrange' })
    vim.api.nvim_set_hl(0, '@lsp.type.string', { fg='SeaGreen' })
    vim.api.nvim_set_hl(0, '@lsp.mod.definition', { fg='LightRed' })
    vim.lsp.start({
      cmd = { "/home/psanford/dev/personal/sicsim-rs/target/debug/siclsp" },
      root_dir = vim.fn.getcwd(), -- Use PWD as project root dir.
    })
  end
})
```
