.globl _start
_start:
    li t0, 1
    li t1, 100000000
    li s0, 0x20010
    sd t0, 0(s0)
    j _test_loop

_bad_loop:
    j _bad_loop

_test_loop:
    lw t0, 0(s0)
    beq t0, t1, _end
    call _add_counter
    j _test_loop

_end:
    call _report
    ebreak
    j _end

_report:
    ret

_add_counter:
    addi t0, t0, 1
    sw t0, 0(s0)
    ret