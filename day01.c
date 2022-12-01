#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>

#define handle_error(msg)                                       \
  do { perror(msg); exit(EXIT_FAILURE); } while (0)

int64_t now()
{
  struct timespec ts;

  if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
    handle_error("clock_gettime");

  return ts.tv_sec * 1000000000 + ts.tv_nsec;
}

typedef struct {
  int64_t p1;
  int64_t p2;
} solution_t;

void solve(const char *filename, solution_t *sol) {
  char *addr;
  int fd = -1;
  struct stat sb;
  off_t offset, pa_offset;
  size_t length;
  ssize_t s;

  fd = open(filename, O_RDONLY);
  if (fd == -1)
    handle_error("open");

  if (fstat(fd, &sb) == -1)
    handle_error("fstat");

  offset = 0;
  length = sb.st_size;

  addr = mmap(NULL, length + offset, PROT_READ, MAP_PRIVATE, fd, 0);
  if (addr == MAP_FAILED)
    handle_error("mmap");

  if (madvise(addr, length, MADV_SEQUENTIAL) != 0)
    handle_error("madvise");

  int64_t a = 0, b = 0, c = 0, curr = 0;

  char *buffer = addr;
  char *end = addr + length;

  while (buffer != end) {
    if (*buffer == '\n') {
      if (curr > a) {
        c = b;
        b = a;
        a = curr;
      } else if (curr > b) {
        c = b;
        b = curr;
      } else if (curr > c) {
        c = curr;
      }

      curr = 0;
    } else {
      // Do not use strtol here, it has lots of overhead related to
      // base conversion and stuff, so do it by hand instead.
      int r = 0;
      while (*buffer != '\n') {
        r = r * 10 + (*buffer - '0');
        buffer++;
      }

      curr += r;
    }

    buffer++;
  }

  sol->p1 = a;
  sol->p2 = a + b + c;

  munmap(addr, length + offset);
  close(fd);
}


int main(int argc, char **argv) {
  const char *filename = argv[1];
  solution_t sol = { 0 };
  int64_t t0 = now();
  solve(filename, &sol);
  int64_t t1 = now();
  printf("p1 = %ld, p2 = %ld, time = %.4g secs (%ld nsecs)\n",
         sol.p1, sol.p2, ((double)(t1 - t0)) / 1000000000.0, (t1 - t0));
  //assert(sol.p1 == 69836);
  //assert(sol.p2 == 207968);
  exit(EXIT_SUCCESS);
}
