.. remote-lsp

You can simulate a remote LSP with the following script:
```
$ mkfifo /tmp/somewhere # possibly in a directory resulting from `mktemp -d`
$ nc -l 8000 < /tmp/somewhere | superbol-free lsp > /tmp/somewhere
```

Once this is running, you can enter `tcp://localhost:8000` in the `superbol.lsp-path` configuration setting. SuperBOL Studio will then rely on this LSP server instead of a binary that is bundled in the extension.
Note: this "made up" remote server terminates once the first connection is closed.
