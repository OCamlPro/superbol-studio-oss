# How to debug the LSP with Emacs

## Spying communication between the LSP and Emacs

Before starting Emacs, set the following environment variable:

```
export SUPERBOL_LSP_DEBUG='/tmp/superbol-$count.log'

```


## Infinite loops


```
(setq debug-on-quit t)
```
