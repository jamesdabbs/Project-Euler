#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int is_bouncy(int n) {
  char *str;
  asprintf(&str, "%d", n);
  int i, diff;
  int trend = 0;
  int bouncy = 0;
  int len = strlen(str);
  for (i=1; i<len; i++) {
    diff = (str[i] - '0') - (str[i-1] - '0');
    if (trend && diff) {
      if ((trend > 0) != (diff > 0)) {
        bouncy = 1;
        break;
      } 
    }
    if (trend == 0) {
      trend = diff;
    }
  }
  free(str);
  return bouncy;
}

int solution() {
  int n, count = 0;
  double pct;
  for (n=100; ; n++) {
    if (is_bouncy(n)) {
      count++;
      pct = 100.0 * count / n;
      if (pct == 99.0) return n;
    }
  }
}

int main() {
  printf("%d\n", solution());
}