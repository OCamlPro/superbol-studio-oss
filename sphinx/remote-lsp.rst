.. remote-lsp

You can simulate a remote LSP with the following scripts:

Start a server on any port (here I chose 8000):
`$ nc -k -l localhost 8000 > test.nc`

Then, use the following script to read on a file:
```
touch $1
inotifywait -m -e modify $1 | while read line
do
    if [ ! -s $1 ]; then
       echo 'Waiting for content'
    else
	cat $1 | superbol lsp > superbol.log
	> $1
    fi
done
```

and call it with `test.nc`.

TODO: this script only receives information, but does not send it back to
VSCODE; it only writes it in superbol.log. It should answer to the client.
