#include <stdio.h>
#include "HsFFI.h"

extern HsPtr compareBranches(HsPtr a1, HsPtr a2);

int main() {
    hs_init(NULL, NULL);
    printf("%s\n", (char *)compareBranches("p10", "p9"));
    hs_exit();
    return 0;
}