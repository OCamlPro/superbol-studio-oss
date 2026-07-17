#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>

#define BUF_CAP 2046
#define ESCAPED_CAP (4 + 2 * BUF_CAP)

static_assert(ESCAPED_CAP <= PIPE_BUF);

size_t to_c_string(size_t len, char buf[restrict static len], char *restrict escaped) {
  int i = 0;
  int j = 0;
  escaped[i++] = '@';
  escaped[i++] = '\"';

  while (j < len) {
    char c = buf[j++];
    switch (c) {
      case '\a': escaped[i++] = '\\'; escaped[i++] = 'a'; break;
      case '\b': escaped[i++] = '\\'; escaped[i++] = 'b'; break;
      case '\t': escaped[i++] = '\\'; escaped[i++] = 't'; break;
      case '\v': escaped[i++] = '\\'; escaped[i++] = 'v'; break;
      case '\r': escaped[i++] = '\\'; escaped[i++] = 'r'; break;
      case '\n': escaped[i++] = '\\'; escaped[i++] = 'n'; break;
      case '\\': escaped[i++] = '\\'; escaped[i++] = '\\'; break;
      case '"':  escaped[i++] = '\\'; escaped[i++] = '"'; break;
      default:
        escaped[i++] = c;
    }
  }

  escaped[i++] = '\"';
  escaped[i++] = '\n';
  return i;
}

void read_output(void) {
  char buf[BUF_CAP] = {};
  char escaped[ESCAPED_CAP] = {};
  size_t r;

  while ((r = read(STDIN_FILENO, buf, BUF_CAP)) > 0) {
    size_t sz = to_c_string(r, buf, escaped);
    if (write(STDOUT_FILENO, escaped, sz) <= 0)
      break;
  }
}

int main(int argc, char **argv) {
  pid_t pid;
  int pipefd[2];

  if (pipe(pipefd) != 0) {
    perror("pipe");
    return EXIT_FAILURE;
  }

  switch ((pid = fork())) {
    case -1:
      perror("fork");
      return EXIT_FAILURE;
    case 0:
      close(pipefd[1]);
      dup2(pipefd[0], STDIN_FILENO);
      close(pipefd[0]);
      read_output();
      break;
    default:
      close(pipefd[0]);
      dup2(pipefd[1], STDOUT_FILENO);
      close(pipefd[1]);
      execv(argv[1], argv + 1);
  }

  return EXIT_SUCCESS;
}
