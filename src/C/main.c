#include "vm.h"
#include <stdio.h>
#include <errno.h>
#include <string.h>

static int read_into_memory(FILE* f, int base_addr);
static void dump_memory(int base_addr, int count);

int main(int argc, const char** argv) {
    int prg_size = read_into_memory(stdin, 0);
    if (prg_size < 0) return 1;
    fprintf(stderr, "Program size: %d\n", prg_size);

    dump_memory(0, 20);

    machine_state state;
    init_vm(&state);
    MMErrorCode err = run_vm(&state);
    fprintf(stderr, "Result code: %d\n", err);

    dump_memory(0, 20);

    return 0;
}

static int read_into_memory0(FILE* f, int base_addr) {
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

static int reg_from_name(const char* name, int* reg_nr);

const size_t LINE_BUF_SIZE = 200;
static int read_into_memory(FILE* f, int base_addr) {
    int cnt = 0;
    char linebuf[LINE_BUF_SIZE+1];
    char part1[LINE_BUF_SIZE+1];
    char part2[LINE_BUF_SIZE+1];
    char eolbuf[1];
    size_t line_len  = LINE_BUF_SIZE;
    while (!feof(f)) {
        char* line = linebuf;
        char* eol = eolbuf;
        if (getline(&line, &line_len, stdin) == -1) {
            if (errno) {
                fprintf(stderr, "Input error: %s\n", strerror(errno));
                return -1;
            } else {
                return 0;
            }
        }
        //fprintf(stderr, "Got line: '%s'\n", line);

        mint lit;
        if (sscanf(line, "%1[\n]", eol) == 1) {
            // Empty (whitespace-only line
            continue;
        } else if (sscanf(line, "%1[#]", eol) == 1) {
            // Comment line
            continue;
        } else if (sscanf(line, "%ld%1[\n]", &lit, eol) == 2) {
            // Decimal case.
            put_mem(base_addr+(cnt++), lit);
        } else {
            int src, dest;
            lit=0;
            if (sscanf(line, "%ld > %[A-Za-z]%1[\n]", &lit, part2, eol) == 3) {
                // Symbolic case - literal input.
                //printf ("TODO: %ld > '%s'\n", lit, part2);
                src = R_K;
                if (reg_from_name(part2, &dest)) return -1;
            } else if (sscanf(line, "%[A-Za-z] > %[A-Za-z]%1[\n]", part1, part2, eol) == 3) {
                // Symbolic case.
                //printf ("TODO: '%s' > '%s'\n", part1, part2);
                if (reg_from_name(part1, &src)) return -1;
                if (reg_from_name(part2, &dest)) return -1;
            } else {
                fprintf(stderr, "Bad line: '%s'\n", line);
                return -1;
            }
            mint d = src*10 + dest;
            if (lit<0) d=-d; // Align signs.
            d += lit*100;
            put_mem(base_addr+(cnt++), d);
        }
    }
    return cnt;
}


static int reg_from_name(const char* name, int* reg_nr) {
    if (0 == strcmp(name, "X"))      *reg_nr = R_X;
    else if (0 == strcmp(name, "Y")) *reg_nr = R_Y;
    else if (0 == strcmp(name, "Z")) *reg_nr = R_Z;
    else if (0 == strcmp(name, "W")) *reg_nr = R_W;
    else if (0 == strcmp(name, "K")) *reg_nr = R_K;
    else if (0 == strcmp(name, "R")) *reg_nr = R_R;
    else if (0 == strcmp(name, "C")) *reg_nr = R_C;
    else if (0 == strcmp(name, "A")) *reg_nr = R_A;
    else if (0 == strcmp(name, "M")) *reg_nr = R_M;
    else if (0 == strcmp(name, "J")) *reg_nr = R_J;
    else if (0 == strcmp(name, "L")) *reg_nr = R_L;
    else if (0 == strcmp(name, "IP")) *reg_nr = R_IP;
    else {
        fprintf(stderr, "Unknown register: '%s'\n", name);
        return 1;
    }

    return 0;
}

static void dump_memory(int base_addr, int count) {
    fprintf(stderr, "== Memory dump ==\n");
    int i;
    for (i=0; i<count; i++) {
        int a = base_addr+i;
        mint d = get_mem(a);
        fprintf(stderr, "@%4d\t%10ld\n", a, d);
    }
}
