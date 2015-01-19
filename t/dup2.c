
#include <stdio.h>
#include <unistd.h>

int main ()
{
  dup2(1,2);
  close(2);
  printf("dup2\n");
  return(0);
}
