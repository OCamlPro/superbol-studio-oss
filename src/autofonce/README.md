
[![Actions Status](https://github.com/ocamlpro/autofonce/workflows/Main%20Workflow/badge.svg)](https://github.com/ocamlpro/autofonce/actions)
[![Release](https://img.shields.io/github/release/ocamlpro/autofonce.svg)](https://github.com/ocamlpro/autofonce/releases)

# autofonce

autofonce is a modern runner for GNU Autoconf Testsuites:
autofonce has a limited understanding of m4 macros that appear in testsuites
written for the GNU Autoconf testsuites, and can run such tests in a modern
way.


* Website: https://ocamlpro.github.io/autofonce
* General Documentation: https://ocamlpro.github.io/autofonce/sphinx
* API Documentation: https://ocamlpro.github.io/autofonce/doc
* Sources: https://github.com/ocamlpro/autofonce


## Main Features

* automatic run from any directory in the project
* short display focusing on failed tests
* automatic parallel execution
* promotion of tests results to fix tests

## Example of Autofonce Test

Here is a simple example of test understood by `autofonce`:

```
# Start of a test, and the name that will be displayed
AT_SETUP([Example test])

# can be used to select tests to run:
AT_KEYWORDS([example test autofonce]) 

# create a file `file` with its content
AT_DATA([file], [
content of file
on multiple lines
])

# call some command, check its exit code, stdout, stderr
AT_CHECK([cat file], [0], [stdout of my command], [stderr of my command])
# you can do more, ignore some results, run more tests in case of failure, etc.

# end of the test
AT_CLEANUP
```

Now, copy this test in a file `test.at`, and run `autofonce`:
```
$ autofonce init
$ autofonce run -T test.at
Project description loaded from $PWD/autofonce.toml
Loading tests from file "$PWD/test.at"
Creating testing directory $PWD/_autofonce

0001 Example test                    FAIL (test.at:14:0 stdout stderr)
Results:
* 1 checks performed
* 0 / 1 tests executed successfully
* 1 tests failed: 0001
File "$PWD/_autofonce/results.log" created with failure results
zsh: exit 1     autofonce run -T test.at
```


