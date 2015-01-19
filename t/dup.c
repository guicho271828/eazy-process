
#include <stdio.h>
#include <unistd.h>

int main ()
{
  int fd;
  fd=dup(1);
  printf("%d\n",fd);
  close(fd);
  printf("dup\n");
  close(1);
  printf("dup\n");
  return(0);
}
