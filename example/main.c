#include <stdio.h>
#include "HsFFI.h"

extern HsPtr compareBranches(HsPtr a1, HsPtr a2);

int main() {
    hs_init(NULL, NULL);
    char a[16], b[16];
    scanf("%15s%15s", a, b);
    printf("%s\n", (char *)compareBranches(a, b));
    hs_exit();
    return 0;
}
