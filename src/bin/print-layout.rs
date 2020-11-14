use rvjit_aa64::runtime::Runtime;

fn main() {
    println!("offset_pending_jalr_target: {}", Runtime::offset_pending_jalr_target());
    println!("offset_error_reason: {}", Runtime::offset_error_reason());
    println!("offset_exception_entry: {}", Runtime::offset_exception_entry());
    println!("offset_memory_regs: {}", Runtime::offset_memory_regs());
    println!("offset_spill: {}", Runtime::offset_spill());
    println!("offset_guest_save: {}", Runtime::offset_guest_save());
}
