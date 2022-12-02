#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>

void *do_mmap(const char *filename, int *fd, size_t *length)
{
  struct stat sb;

  *fd = open(filename, O_RDONLY);
  if (*fd == -1)
    return NULL;

  if (fstat(*fd, &sb) == -1)
    return NULL;

  *length = sb.st_size;

  void *addr = mmap(NULL, *length, PROT_READ, MAP_PRIVATE, *fd, 0);
  if (addr == MAP_FAILED)
    return NULL;

  if (madvise(addr, *length, MADV_SEQUENTIAL) != 0)
    return NULL;

  return addr;
}


void do_munmap(int fd, void *addr, size_t length)
{
  munmap(addr, length);
  close(fd);
}
