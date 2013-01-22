#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int is_palindrome(int n) {
  char *str;
  asprintf(&str, "%d", n);
  int l = strlen(str);
  int i;
  int mid = l/2;
  int pass = 1;
  for (i=0; i<mid; i++) {
    if (str[i] != str[l-i-1]) {
      pass = 0;
      break;
    }
  }
  free(str);
  return pass;
}

int solution() {
  int i, j, prod, best = 0;
  for (i=999; i>100; i--) {
    for (j=999; j>100; j--) {
      prod = i * j;
      if (prod < best) break;
      if (is_palindrome(prod)) best = prod;
    }
  }
  return best;
}

int main() {
  printf("%d\n", solution());
}