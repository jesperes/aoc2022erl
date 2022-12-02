#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define handle_error(msg) \
  do { perror(msg); exit(EXIT_FAILURE); } while (0)

void *do_mmap(const char *filename, int *fd, size_t *length);
void do_munmap(int fd, void *addr, size_t length);

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

#define O_ROCK 'A'
#define O_PAPER 'B'
#define O_SCISSORS 'C'

#define P_ROCK_OR_LOSE 'X'
#define P_PAPER_OR_DRAW 'Y'
#define P_SCISSORS_OR_WIN 'Z'

#define SCORE_ROCK 1
#define SCORE_PAPER 2
#define SCORE_SCISSORS 3

#define LOSE 0
#define DRAW 3
#define WIN 6

void solve(const char *filename, solution_t *sol) {
  size_t length;
  int fd;
  char *addr = do_mmap(filename, &fd, &length);

  sol->p1 = 0;
  sol->p2 = 0;

  for (int i = 0; i < length; i += 4) {
    int opponent = addr[i];
    int player = addr[i + 2];

    if (opponent == O_ROCK) {
      if (player == P_ROCK_OR_LOSE) {
        sol->p1 += SCORE_ROCK + DRAW;
        sol->p2 += SCORE_SCISSORS + LOSE;
      } else if (player == P_PAPER_OR_DRAW) {
        sol->p1 += SCORE_PAPER + WIN;
        sol->p2 += SCORE_ROCK + DRAW;
      } else if (player == P_SCISSORS_OR_WIN) {
        sol->p1 += SCORE_SCISSORS + LOSE;
        sol->p2 += SCORE_PAPER + WIN;
      }
    } else if (opponent == O_PAPER) {
      if (player == P_ROCK_OR_LOSE) {
        sol->p1 += SCORE_ROCK + LOSE;
        sol->p2 += SCORE_ROCK + LOSE;
      } else if (player == P_PAPER_OR_DRAW) {
        sol->p1 += SCORE_PAPER + DRAW;
        sol->p2 += SCORE_PAPER + DRAW;
      } else if (player == P_SCISSORS_OR_WIN) {
        sol->p1 += SCORE_SCISSORS + WIN;
        sol->p2 += SCORE_SCISSORS + WIN;
      }
    } else if (opponent == O_SCISSORS) {
      if (player == P_ROCK_OR_LOSE) {
        sol->p1 += SCORE_ROCK + WIN;
        sol->p2 += SCORE_PAPER + LOSE;
      } else if (player == P_PAPER_OR_DRAW) {
        sol->p1 += SCORE_PAPER + LOSE;
        sol->p2 += SCORE_SCISSORS + DRAW;
      } else if (player == P_SCISSORS_OR_WIN) {
        sol->p1 += SCORE_SCISSORS + DRAW;
        sol->p2 += SCORE_ROCK + WIN;
      }
    }
  }

  do_munmap(fd, addr, length);
}


int main(int argc, char **argv) {
  const char *filename = argv[1];
  solution_t sol = { 0 };
  int64_t t0 = now();
  solve(filename, &sol);
  int64_t t1 = now();
  printf("p1 = %ld, p2 = %ld, time = %.4g secs (%ld nsecs)\n",
         sol.p1, sol.p2, ((double)(t1 - t0)) / 1000000000.0, (t1 - t0));
  exit(EXIT_SUCCESS);
}
