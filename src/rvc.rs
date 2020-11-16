#![allow(dead_code)]

pub fn decompress(raw: u16) -> Option<u32> {
    let opcode = ((raw >> 13) << 2) | (raw & 0b11);
    match opcode {
        _ => None
    }
}

fn encode_itype(op: u32, funct3: u32, rd: u32, rs: u32, imm: u32) -> u32 {
    op | (rd << 7) | (funct3 << 12) | (rs << 15) | (imm << 20)
}

fn encode_rtype(op: u32, funct3: u32, funct7: u32, rd: u32, rs: u32, rt: u32) -> u32 {
    op | (rd << 7) | (funct3 << 12) | (rs << 15) | (rt << 20) | (funct7 << 25)
}

fn encode_stype(op: u32, funct3: u32, rs: u32, rt: u32, imm: u32) -> u32 {
    op | ((imm & 0b11111) << 7) | (funct3 << 12) | (rs << 15) | (rt << 20) | (((imm >> 5) & 0b1111111) << 25)
}

fn encode_btype(op: u32, funct3: u32, rs: u32, rt: u32, imm: u32) -> u32 {
    op | (((imm >> 11) & 0b1) << 7) | (((imm >> 1) & 0b1111) << 8) | (funct3 << 12) | (rs << 15) | (rt << 20) | (((imm >> 5) & 0b111111) << 25) | (((imm >> 12) & 0b1) << 31)
}

fn encode_utype(op: u32, rd: u32, imm: u32) -> u32 {
    op | (rd << 7) | ((imm >> 12) << 12)
}

fn encode_jtype(op: u32, rd: u32, imm: u32) -> u32 {
    op | (rd << 7) | (((imm >> 12) & 0b1111_1111) << 12) | (((imm >> 11) & 0b1) << 20) | (((imm >> 1) & 0b11111_11111) << 21) | (((imm >> 20) & 0b1) << 31)
}