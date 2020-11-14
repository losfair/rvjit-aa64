all:
	cd play && xargo build --release --target rvjit
	rust-objcopy \
			./play/target/rvjit/release/play \
			--binary-architecture=riscv64 --strip-all -O binary \
			play.bin

disasm:
	rust-objdump -d --arch-name riscv64 ./play/target/rvjit/release/play
