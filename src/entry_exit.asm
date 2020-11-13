#offset_error_vpc: 0                                                                                                                                                                        │················
#offset_error_reason: 8                                                                                                                                                                     │················
#offset_exception_entry: 16                                                                                                                                                                 │················
#offset_memory_regs: 24                                                                                                                                                                     │················
#offset_spill: 56                                                                                                                                                                           │················
#offset_guest_save: 312   

.macro SAVE_ONE reg, base, offset, index
    str \reg, [\base, #(\offset + (8 * \index))]
.endm

.macro LOAD_ONE reg, base, offset, index
    ldr \reg, [\base, #(\offset + (8 * \index))]
.endm

.macro SAVE_REGS base, offset
    SAVE_ONE x0, \base, \offset, 0
    SAVE_ONE x1, \base, \offset, 1
    SAVE_ONE x2, \base, \offset, 2
    SAVE_ONE x3, \base, \offset, 3
    SAVE_ONE x4, \base, \offset, 4
    SAVE_ONE x5, \base, \offset, 5
    SAVE_ONE x6, \base, \offset, 6
    SAVE_ONE x7, \base, \offset, 7
    SAVE_ONE x8, \base, \offset, 8
    SAVE_ONE x9, \base, \offset, 9
    SAVE_ONE x10, \base, \offset, 10
    SAVE_ONE x11, \base, \offset, 11
    SAVE_ONE x12, \base, \offset, 12
    SAVE_ONE x13, \base, \offset, 13
    SAVE_ONE x14, \base, \offset, 14
    SAVE_ONE x15, \base, \offset, 15
    SAVE_ONE x16, \base, \offset, 16
    SAVE_ONE x17, \base, \offset, 17
    SAVE_ONE x18, \base, \offset, 18
    SAVE_ONE x19, \base, \offset, 19
    SAVE_ONE x20, \base, \offset, 20
    SAVE_ONE x21, \base, \offset, 21
    SAVE_ONE x22, \base, \offset, 22
    SAVE_ONE x23, \base, \offset, 23
    SAVE_ONE x24, \base, \offset, 24
    SAVE_ONE x25, \base, \offset, 25
    SAVE_ONE x26, \base, \offset, 26
    SAVE_ONE x27, \base, \offset, 27
    SAVE_ONE x28, \base, \offset, 28
    SAVE_ONE x29, \base, \offset, 29
    SAVE_ONE x30, \base, \offset, 30
.endm

.macro LOAD_REGS_NO_X2_X30 base, offset
    LOAD_ONE x0, \base, \offset, 0
    LOAD_ONE x1, \base, \offset, 1
    LOAD_ONE x3, \base, \offset, 3
    LOAD_ONE x4, \base, \offset, 4
    LOAD_ONE x5, \base, \offset, 5
    LOAD_ONE x6, \base, \offset, 6
    LOAD_ONE x7, \base, \offset, 7
    LOAD_ONE x8, \base, \offset, 8
    LOAD_ONE x9, \base, \offset, 9
    LOAD_ONE x10, \base, \offset, 10
    LOAD_ONE x11, \base, \offset, 11
    LOAD_ONE x12, \base, \offset, 12
    LOAD_ONE x13, \base, \offset, 13
    LOAD_ONE x14, \base, \offset, 14
    LOAD_ONE x15, \base, \offset, 15
    LOAD_ONE x16, \base, \offset, 16
    LOAD_ONE x17, \base, \offset, 17
    LOAD_ONE x18, \base, \offset, 18
    LOAD_ONE x19, \base, \offset, 19
    LOAD_ONE x20, \base, \offset, 20
    LOAD_ONE x21, \base, \offset, 21
    LOAD_ONE x22, \base, \offset, 22
    LOAD_ONE x23, \base, \offset, 23
    LOAD_ONE x24, \base, \offset, 24
    LOAD_ONE x25, \base, \offset, 25
    LOAD_ONE x26, \base, \offset, 26
    LOAD_ONE x27, \base, \offset, 27
    LOAD_ONE x28, \base, \offset, 28
    LOAD_ONE x29, \base, \offset, 29
.endm

.macro LOAD_REGS base, offset
    LOAD_REGS_NO_X2_X30 \base, \offset
    LOAD_ONE x30, \base, \offset, 30
    LOAD_ONE x2, \base, \offset, 2
.endm

.globl _rvjit_enter_guest
_rvjit_enter_guest:
sub sp, sp, 32 * 8
SAVE_REGS sp, 0

mov x2, x0
mov x30, x1
add x2, x2, 312
LOAD_REGS_NO_X2_X30 x2, 0
sub x2, x2, 312
br x30

.globl _rvjit_guest_exception
_rvjit_guest_exception:
add x2, x2, 312
SAVE_REGS x2, 0
sub x2, x2, 312
LOAD_REGS sp, 0
add sp, sp, 32 * 8
ret x30
