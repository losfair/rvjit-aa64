.globl _start
_start:
    li t0, 1
    li t1, 100000000
    j _test_loop

_bad_loop:
    j _bad_loop

_test_loop:
    beq t0, t1, _end
    call _add_t0
    j _test_loop

_end:
    call _report
    ebreak
    j _end

_report:
    ret

_add_t0:
    addi t0, t0, 1
    ret