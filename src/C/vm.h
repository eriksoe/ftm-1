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


typedef enum MMRegisterID {
    R_K  = 0,
    R_X  = 1,
    R_Y  = 2,
    R_R  = 3, R_C = R_R,
    R_Z  = 4,
    R_W  = 5,
    R_A  = 6,
    R_M  = 7,
    R_IP = 8,
    R_J  = 9, R_L = R_J
} MMRegisterID;

void put_mem(int addr, mint x);
mint get_mem(int addr);
void init_vm(machine_state* st);
MMErrorCode run_vm(machine_state* st);
