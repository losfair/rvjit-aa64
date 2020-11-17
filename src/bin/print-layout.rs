use rvjit_aa64::runtime::Runtime;

fn main() {
    println!("offset_error_data: {}", Runtime::offset_error_data());
    println!("offset_error_reason: {}", Runtime::offset_error_reason());
    println!("offset_exception_entry: {}", Runtime::offset_exception_entry());
    println!("offset_memory_regs: {}", Runtime::offset_memory_regs());
    println!("offset_current_vbase: {}", Runtime::offset_current_vbase());
    println!("offset_spill: {}", Runtime::offset_spill());
    println!("offset_guest_save: {}", Runtime::offset_guest_save());
}
