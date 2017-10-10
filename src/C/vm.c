#include "vm.h"

#include <stdint.h>
#include <assert.h>
#include <stdio.h> // For debugging

#define MEM_SIZE 1000000

/* The memory. */
mint mem[MEM_SIZE];

typedef enum MMOperationID {
    OP_X = 0,
    OP_Y = 1,
    OP_ADD = 2,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_REM,
    OP_LT = 10,
    OP_GT,
    OP_LTE,
    OP_GTE,
    OP_EQ,
    OP_NEQ,
    OP_AND = 16,
    OP_OR,
    OP_XOR,
    OP_NOT,
    OP_LIMIT
} MMOperationID;

void init_vm(machine_state* st) {
    st->X = st->Y = st->Z = st->W = st->A = st->C = st->L = st->IP = 0;
}

static MMErrorCode check_mem_access(mint addr);
static mint perform_calculation(mint x, mint y, mint c, MMErrorCode* err);

MMErrorCode step(machine_state* st) {
    MMErrorCode err;
    err = check_mem_access(st->IP);
    if (err) {
        if (st->IP < 0) err = MM_STOPPED;
        return err;
    }
    mint ins = mem[st->IP];
    ++st->IP;

    fprintf(stderr, "DB| ins=%ld\n", ins);
    int dest = ins % 10; ins /= 10;
    int src = ins % 10;
    if (src<0) src=-src;
    if (dest<0) dest=-dest;

    fprintf(stderr, "DB| src=%d, dest=%d\n", src, dest);

    mint d; // Data to be moved.
    //---------- Read phase:
    switch (src) {
    case R_X: d = st->X; break;
    case R_Y: d = st->Y; break;
    case R_Z: d = st->Z; break;
    case R_W: d = st->W; break;
    case R_A: d = st->A; break;

    case R_K: d = ins / 10; break;
    case R_L: d = st->L; break;
    case R_IP: d = st->IP; break;

    case R_M:
        err = check_mem_access(st->A);
        if (err) return err;
        d = mem[st->A];
        break;

    case R_R:
        d = perform_calculation(st->X, st->Y, st->C, &err);
        if (err) return err;
        break;

    default:
        return MM_EINTERNAL;
    }

    fprintf(stderr, "DB|   d=%ld\n", d);

    //---------- Write phase:
    switch (dest) {
    case R_X: st->X = d; break;
    case R_Y: st->Y = d; break;
    case R_Z: st->Z = d; break;
    case R_W: st->W = d; break;
    case R_A: st->A = d; break;

    case R_K: return MM_EWRITE_K; break;

    case R_J: {
        // Calculate R:
        mint flag = perform_calculation(st->X, st->Y, st->C, &err);
        if (err) return err;
        if (flag != 0) st->IP = d;
    } break;

    case R_IP:
        st->L = st->IP;
        st->IP = d;
        break;

    case R_M:
        err = check_mem_access(st->A);
        if (err) return err;
        mem[st->A] = d;
        break;

    case R_C:
        if (d<0 || d>=OP_LIMIT)
            return MM_EOPERATION;
        st->C = d;
        break;

    default:
        return MM_EINTERNAL;
    }

    return MM_OK;
}

MMErrorCode run_vm(machine_state* st) {
    MMErrorCode err;
    do {
        err = step(st);
    }
    while (err == MM_OK);
    return err;
}


static MMErrorCode check_mem_access(mint addr) {
    if (addr < 0) return MM_EADDR;
    if (addr >= MEM_SIZE) return MM_EADDR;
    return MM_OK;
}

static mint perform_calculation(mint x, mint y, mint op, MMErrorCode* err) {
    fprintf(stderr, "DB| perform_calculation: op=%ld\n", op);
    mint d;
    switch (op) {
    case OP_X: d = x; break;
    case OP_Y: d = y; break;

    case OP_ADD: d = x+y; break;
    case OP_SUB: d = x-y; break;
    case OP_MUL: d = x*y; break;
    case OP_DIV: if (y==0) return MM_EDIV0; else d = x/y; break;
    case OP_REM: if (y==0) return MM_EDIV0; else d = x%y; break;

    case OP_LT:  d = x < y; break;
    case OP_GT:  d = x > y; break;
    case OP_LTE: d = x <= y; break;
    case OP_GTE: d = x >= y; break;
    case OP_EQ:  d = x == y; break;
    case OP_NEQ: d = x != y; break;

    case OP_AND: d = (x!=0) && (y!=0); break;
    case OP_OR:  d = (x!=0) || (y!=0); break;
    case OP_XOR: d = (x!=0) ^ (y!=0); break;
    case OP_NOT: d = (x==0); break;

    default:
        *err = MM_EOPERATION;
        d = 0;
    }
    return d;
}

void put_mem(int addr, mint x) {
    assert(addr>=0 && addr<MEM_SIZE);
    mem[addr] = x;
}

mint get_mem(int addr) {
    assert(addr>=0 && addr<MEM_SIZE);
    return mem[addr];
}
