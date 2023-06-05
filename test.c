#include "stdio.h"

int f1(int a, int b){
    float f;
    int g;
    g = a;
    f = 1.2;
    return g + b;
}

int f2(int b){
    return 3;
}

int main() {
    int f;
    int i;
    int n;
    float e;
    n = 5;
    f = n;
    i = 100;
    f = (f1(1, n)) + (f2(2));
    f2(2);
    for(i = 0; i < 7; i = i + 2){
        i = i;
    }
    n = f1(5, i);
    printf("%d\n", n);
    return 0;
}