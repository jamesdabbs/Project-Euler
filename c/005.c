#include <stdio.h>

int gcd(int a, int b) {
    while (1) {
        if (a == 0) return b;
        b %= a;
        if (b == 0) return a;
        a %= b;
    }
}

int lcm(int a, int b) {
    int temp = gcd(a, b);

    return temp ? (a / temp * b) : 0;
}

int solution() {
  int i, acc = 1;
  for (i=2; i<20; i++) {
    acc = lcm(acc, i);
  }
  return acc;
}

int main() {
  printf("%d\n", solution());
}