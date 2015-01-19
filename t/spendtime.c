
#include <stdio.h>
#include <time.h>

int main ()
{
  time_t seconds;
  time_t seconds2;
  
  seconds = time(NULL);
  do {
    seconds2 = time(NULL);
  } while ((seconds2 - seconds)<3) ;
  printf("done\n");
  return(0);
}
