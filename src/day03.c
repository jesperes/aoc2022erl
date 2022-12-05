#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <assert.h>

#define handle_error(msg) \
  do { perror(msg); exit(EXIT_FAILURE); } while (0)

void *do_mmap(const char *filename, int *fd, size_t *length);
void do_munmap(int fd, void *addr, size_t length);

static int64_t now()
{
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  return ts.tv_sec * 1000000000 + ts.tv_nsec;
}

typedef struct {
  int p1;
  int p2;
} solution_t;

static int prio(char c) {
  if (c <= 'Z')
    return c - 'A' + 27;
  else
    return c - 'a' + 1;
}

static int64_t str_to_mask(const char *str, int len)
{
  int64_t mask = 0;
  for (int i = 0; i < len; i++) {
    mask |= 1LL << prio(str[i]);
  }
  return mask;
}

static int64_t str_to_mask_with_offset(const char *str, int offset, int len)
{
  int64_t mask = 0;
  for (int i = 0; i < len; i++) {
    mask |= 1LL << prio(str[i + offset]);
  }
  return mask;
}

// mask_to_prio() is essentially "count trailing zeroes", and gcc has
// a built-in for that.
#define mask_to_prio __builtin_ctzll

static int count1(const char *str, int len)
{
  int half = len >> 1;
  int64_t left_mask = str_to_mask(str, half);
  int64_t right_mask = str_to_mask_with_offset(str, half, half);
  return mask_to_prio(left_mask & right_mask);
}

static int count2(const char *a, int alen, const char *b, int blen, const char *c, int clen)
{
  int64_t mask_a = str_to_mask(a, alen);
  int64_t mask_b = str_to_mask(b, blen);
  int64_t mask_c = str_to_mask(c, clen);
  return mask_to_prio(mask_a & mask_b & mask_c);
}

#define BUFSIZE 512

solution_t solve(const char *addr, int length)
{
  const char *buf = addr;
  const char *last = addr + length;
  solution_t sol = { 0 };

  while (1) {
    if (buf >= last)
      break;

    const char *p1 = buf;
    const char *p2 = strchr(buf, '\n') + 1;
    const char *p3 = strchr(p2, '\n') + 1;
    buf = strchr(p3, '\n') + 1;

    int p1len = p2 - p1 - 1;
    int p2len = p3 - p2 - 1;
    int p3len = buf - p3 - 1;

    sol.p1 += count1(p1, p1len) + count1(p2, p2len) + count1(p3, p3len);
    sol.p2 += count2(p1, p1len, p2, p2len, p3, p3len);
  }

  return sol;
}

int main(int argc, char **argv)
{
  const char *filename = argv[1];
  size_t length;
  int fd;
  char *addr = do_mmap(filename, &fd, &length);

  int64_t sum = 0;
  for (int i = 0; i < length; i++) {
    sum += addr[i];
  }
  printf("sum = %ld\n", sum);

  solution_t sol;
  int reps = 10000;
  int64_t t0 = now();
  for (int i = 0; i < reps; i++) {
    sol = solve(addr, length);
  }
  int64_t t1 = now();

  printf("p1 = %d, p2 = %d, time = %.2f usecs\n",
         sol.p1, sol.p2, (((double)(t1 - t0)) / 1000) / reps);

  do_munmap(fd, addr, length);
  exit(EXIT_SUCCESS);
}
