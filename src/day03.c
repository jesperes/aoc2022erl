#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <assert.h>

#define handle_error(msg) \
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

int prio(char c) {
  if (c <= 'Z')
    return c - 'A' + 27;
  else
    return c - 'a' + 1;
}

int64_t str_to_mask(char *str, int offset, int len) {
  int64_t mask = 0;
  for (int i = 0; i < len; i++) {
    mask |= 1LL << prio(str[i + offset]);
  }
  return mask;
}

int mask_to_prio(int64_t mask) {
  return __builtin_ctzll(mask);
}

int count1(char *str, int len) {
  int half = len / 2;
  int64_t left_mask = str_to_mask(str, 0, half);
  int64_t right_mask = str_to_mask(str, half, half);
  return mask_to_prio(left_mask & right_mask);
}

int count2(char *a, int alen, char *b, int blen, char *c, int clen) {
  int64_t mask_a = str_to_mask(a, 0, alen);
  int64_t mask_b = str_to_mask(b, 0, blen);
  int64_t mask_c = str_to_mask(c, 0, clen);
  return mask_to_prio(mask_a & mask_b & mask_c);
}

#define BUFSIZE 512

void solve(const char *filename, solution_t *sol) {
  size_t length;
  FILE *f = fopen(filename, "r");
  if (f == NULL)
    handle_error("fopen");

  sol->p1 = 0;
  sol->p2 = 0;

  while (1) {
    char buf1[BUFSIZE];
    char buf2[BUFSIZE];
    char buf3[BUFSIZE];

    char *p1 = fgets(buf1, BUFSIZE, f);
    if (p1 == NULL) break;

    char *p2 = fgets(buf2, BUFSIZE, f);
    char *p3 = fgets(buf3, BUFSIZE, f);

    int p1len = strlen(p1) - 1;
    int p2len = strlen(p2) - 1;
    int p3len = strlen(p3) - 1;

    sol->p1 += count1(p1, p1len) + count1(p2, p2len) + count1(p3, p3len);
    sol->p2 += count2(p1, p1len, p2, p2len, p3, p3len);
  }

  fclose(f);
}


int main(int argc, char **argv) {
  const char *filename = argv[1];
  solution_t sol = { 0 };
  int64_t t0 = now();
  solve(filename, &sol);
  int64_t t1 = now();
  printf("p1 = %ld, p2 = %ld, time = %.2f usecs\n",
         sol.p1, sol.p2, ((double)(t1 - t0)) / 1000);
  exit(EXIT_SUCCESS);
}
