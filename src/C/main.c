#include "vm.h"
#include <stdio.h>

static int read_into_memory(FILE* f, int base_addr);

int main(int argc, const char** argv) {
    int prg_size = read_into_memory(stdin, 0);
    if (prg_size < 0) return 1;
    fprintf(stderr, "Program size: %d\n", prg_size);

    machine_state state;
    init_vm(&state);
    MMErrorCode err = run_vm(&state);
    fprintf(stderr, "Result code: %d\n", err);
    return 0;
}

static int read_into_memory(FILE* f, int base_addr) {
    int cnt = 0;
    while (!feof(f)) {
        int x;
        if (scanf("%d\n", &x) == 1) {
            put_mem(base_addr+(cnt++), x);
        } else {
            char c = fgetc(f);
            fprintf(stderr, "Bad character: '%c'\n", c);
            return -1;
        }
    }
    return cnt;
}

