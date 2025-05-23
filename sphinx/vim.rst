SuperBOL plugin for Vim/Neovim
==============================

.. warning::

   You have to build the SuperBOL Studio binary first. See:
   `How to build the SuperBOL Studio binary <building-lsp.html>`_.

Install for Vim
---------------

Coc
~~~

You can add the SuperBOL LSP to your :code:`coc` configuration:

.. code-block:: json

  {
    "colors.enable": true,
    "languageserver": {
      "cobol": {
        "command": "opam",
        "args": [ "exec",
          "--switch",
          "superbol-studio",
          "--",
          "superbol-free",
          "lsp",
          "--force-syntax-diagnostics",
        ],
        "filetypes": ["cobol"]
      }
    },
  }

If you wish to use semantic highlighting with :code:`coc`, you  must add to the
:code:`coc` configuration:

.. code-block:: json

  "semanticTokens.enable": true

and to your vim configuration:

.. code-block:: vim

  au FileType cobol setlocal syntax=OFF

vim-lsp
~~~~~~~

To work with :code:`vim-lsp` you should add to your configuration:

.. code-block:: vim

  if executable('opam')
      au User lsp_setup call lsp#register_server({
          \ 'name': 'superbol',
          \ 'cmd': {server_info->['opam', 'exec', '--switch', 'superbol-studio', '--', 'superbol-free', 'lsp', '--force-syntax-diagnostics']},
          \ 'allowlist': ['cobol'],
          \ })
  endif

If you want semantic highlighting to work with :code:`vim-lsp` you can add to
your configuration:

.. code-block:: vim

  let g:lsp_semantic_enabled = 1
  au FileType cobol setlocal syntax=OFF

Install for Neovim
------------------

You must have :code:`neovim/nvim-lspconfig` installed.

Add these lines to your neovim configuration:

.. code-block:: lua

  require("lspconfig.configs").cobol = {
    default_config = {
      cmd = {
        "opam",
        "exec",
        "--switch",
        "superbol-studio",
        "--",
        "superbol-free",
        "lsp",
        "--force-syntax-diagnostics",
      },
      filetypes = { "cobol" },
      root_dir = function(startpath)
        return require("lspconfig").util.find_git_ancestor(startpath)
          or require("lspconfig").util.path.dirname(startpath)
      end,
      settings = {},
    },
  }

  require("lspconfig").cobol.setup({
    on_attach = my_attach_function, -- should be the same as your other lsp
    capabilities = my_capabilities, -- should be the same as other lsp
  })
