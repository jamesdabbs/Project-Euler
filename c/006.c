#include "euler.h"

int solution() {
  int sum = 0;
  int squares = 0;
  int i;
  for(i=1; i<=100; i++) {
    sum += i;
    squares += i*i;
  }
  return sum * sum - squares;
}