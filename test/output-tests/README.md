# Output tests

Each module in this directory corresponds to one test.

## GnuCOBOL tests
The GnuCOBOL tests are done to test our
product using the GnuCOBOL testsuite. The tests are extracted using `autofonce`. As of now, only
success tests (for GnuCOBOL) are runned. So we would need to take the error output as a helper
to implement features we do not support yet.

TODO:
* Edit the GnuCOBOL flags to have `--` instead of `-`.
* Handle tests with exit code not `0`.
* Find a way to ignore the changes in the temporary files name
