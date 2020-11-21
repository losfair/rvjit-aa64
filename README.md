# rvjit-aa64

RISC-V static binary translator targeting AArch64.

This project is no longer being developed, since the overhead of static binary translation is too high and I decided to move to DBT (dynamic binary translation). The new DBT version is at [losfair/rvrt](https://github.com/losfair/rvrt) (not yet public).

CoreMark scores of the current version on different CPUs:

- Apple M1: 10871 (30911 native) - 35.2% native performance
- Kunpeng 920: 4665 (18964 native) - 24.6% native performance
