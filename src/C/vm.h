#include <stdint.h>

#define MEM_SIZE 1000000

/* Machine integers */
typedef int64_t mint;

/* The registers. */
typedef struct {
    mint IP;
    mint X, Y, Z, W, A, C, L;
} machine_state;


typedef enum MMErrorCode {
    MM_OK = 0,
    MM_STOPPED = 1,
    MM_EINTERNAL,
    MM_EADDR,     // Address error
    MM_EWRITE_K,  // Write to unwriteable register
    MM_EOPERATION,// Unknown operation
    MM_EDIV0      // Division by zero
} MMErrorCode;

void put_mem(int addr, int64_t x);
void init_vm(machine_state* st);
MMErrorCode run_vm(machine_state* st);
