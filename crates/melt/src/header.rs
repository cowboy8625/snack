// 168
// 01ba 00bb 80cd 457f 464c 0101 0001 0000
// 0000 0000 0000 0002 0003 0001 0000 8054
// 0804 0034 0000 0000 0000 0000 0000 0034
// 0020 0001 0028 0000 0000 0001 0000 0054
// 0000 8054 0804 0000 0000 002e 0000 002e
// 0000 0005 0000 1000 0000 # >>>>>>>>>>>>> ELF FILE HEADER <<<<<<<<<<<<< # All numbers (except in names) are in base sixteen (hexadecimal)
//                 # 00 <- number of bytes listed so far
// 7F 45 4C 46     # 04 e_ident[EI_MAG]: ELF magic number
// 01              # 05 e_ident[EI_CLASS]: 1: 32-bit, 2: 64-bit
//    01           # 06 e_ident[EI_DATA]: 1: little-endian, 2: big-endian
//       01        # 07 e_ident[EI_VERSION]: ELF header version; must be 1
//          00     # 08 e_ident[EI_OSABI]: Target OS ABI; should be 0
//
// 00              # 09 e_ident[EI_ABIVERSION]: ABI version; 0 is ok for Linux
//    00 00 00     # 0C e_ident[EI_PAD]: unused, should be 0
// 00 00 00 00     # 10
//
// 02 00           # 12 e_type: object file type; 2: executable
//       03 00     # 14 e_machine: instruction set architecture; 3: x86, 3E: amd64
// 01 00 00 00     # 18 e_version: ELF identification version; must be 1
//
// 54 80 04 08     # 1C e_entry: memory address of entry point (where process starts)
// 34 00 00 00     # 20 e_phoff: file offset where program headers begin
//
// 00 00 00 00     # 24 e_shoff: file offset where section headers begin 00 00 00 00     # 28 e_flags: 0 for x86
//
// 34 00           # 2A e_ehsize: size of this header (34: 32-bit, 40: 64-bit)
//       20 00     # 2C e_phentsize: size of each program header (20: 32-bit, 38: 64-bit)
// 01 00           # 2E e_phnum: #program headers
//       28 00     # 30 e_shentsize: size of each section header (28: 32-bit, 40: 64-bit)
//
// 00 00           # 32 e_shnum: #section headers
//       00 00     # 34 e_shstrndx: index of section header containing section names
//
// # >>>>>>>>>>>>> ELF PROGRAM HEADER <<<<<<<<<<<<<
//
// 01 00 00 00     # 38 p_type: segment type; 1: loadable
//
// 54 00 00 00     # 3C p_offset: file offset where segment begins
// 54 80 04 08     # 40 p_vaddr: virtual address of segment in memory (x86: 08048054)
//
// 00 00 00 00     # 44 p_paddr: physical address of segment, unspecified by 386 supplement
// 2E 00 00 00     # 48 p_filesz: size in bytes of the segment in the file image ############
//
// 2E 00 00 00     # 4C p_memsz: size in bytes of the segment in memory; p_filesz <= p_memsz
// 05 00 00 00     # 50 p_flags: segment-dependent flags (1: X, 2: W, 4: R)
//
// 00 10 00 00     # 54 p_align: 1000 for x86

// Struct FileHeader will need to contain all fields and a known size of bytes.
struct FileHeader {
    // 7F 45 4C 46     # 04 e_ident[EI_MAG]: ELF magic number
    magic_number: [u8; 4],
    // 01              # 05 e_ident[EI_CLASS]: 1: 32-bit, 2: 64-bit
    mode: u8,
    //    01           # 06 e_ident[EI_DATA]: 1: little-endian, 2: big-endian
    endian: u8,
    //       01        # 07 e_ident[EI_VERSION]: ELF header version; must be 1
    header_version: u8,
    //          00     # 08 e_ident[EI_OSABI]: Target OS ABI; should be 0
    target: u8,
    // 00              # 09 e_ident[EI_ABIVERSION]: ABI version; 0 is ok for Linux
    abi_version: u8,
    //    00 00 00     # 0C e_ident[EI_PAD]: unused, should be 0
    // 00 00 00 00     # 10
    unused: [u8; 7],
    // 02 00           # 12 e_type: object file type; 2: executable
    file_type: [u8; 2],
    //       03 00     # 14 e_machine: instruction set architecture; 3: x86, 3E: amd64
    architecture: [u8; 2],
    // 01 00 00 00     # 18 e_version: ELF identification version; must be 1
    id_version: [u8; 4],
    // 54 80 04 08     # 1C e_entry: memory address of entry point (where process starts)
    entry_point: [u8; 4],
    // 34 00 00 00     # 20 e_phoff: file offset where program headers begin
    header_offset: [u8; 4],
    section_offset: [u8; 4],
    flags: [u8; 4],
    header_size: [u8; 2],
    program_header_size: [u8; 2],
    program_header_count: [u8; 2],
    section_header_size: [u8; 2],
    section_headers: [u8; 2],
    idx_section_header_names: [u8; 2],
}

impl FileHeader {
    pub fn new() -> Self {
        Self {
            magic_number: [0x7F, 0x45, 0x4C, 0x46],   // 4
            mode: 0x01,                               // 5
            endian: 0x01,                             // 6
            header_version: 0x01,                     // 7
            target: 0x00,                             // 8
            abi_version: 0x00,                        // 9
            unused: [0x00; 7],                        // 16
            file_type: [0x02, 0x00],                  // 18
            architecture: [0x03, 0x00],               // 20
            id_version: [0x01, 0x00, 0x00, 0x00],     // 24
            entry_point: [0x54, 0x80, 0x04, 0x08],    // 28
            header_offset: [0x34, 0x00, 0x00, 0x00],  // 32
            section_offset: [0x00, 0x00, 0x00, 0x00], // 36
            flags: [0x00; 4],                         // 40
            header_size: [0x34, 0x00],                // 42
            program_header_size: [0x20, 0x00],        // 44
            program_header_count: [0x01, 0x00],       // 46
            section_header_size: [0x28, 0x00],        // 48
            section_headers: [0x00, 0x00],            // 50
            idx_section_header_names: [0x00, 0x00],   // 52
        }
    }

    // pub fn len(&self) -> usize {
    //     0x34
    // }
    pub fn as_bytes(&self) -> Vec<u8> {
        let mut header = Vec::with_capacity(0x34);
        header.extend_from_slice(&self.magic_number);
        header.push(self.mode);
        header.push(self.endian);
        header.push(self.header_version);
        header.push(self.target);
        header.push(self.abi_version);
        header.extend_from_slice(&self.unused);
        header.extend_from_slice(&self.file_type);
        header.extend_from_slice(&self.architecture);
        header.extend_from_slice(&self.id_version);
        header.extend_from_slice(&self.entry_point);
        header.extend_from_slice(&self.header_offset);
        header.extend_from_slice(&self.section_offset);
        header.extend_from_slice(&self.flags);
        header.extend_from_slice(&self.header_size);
        header.extend_from_slice(&self.program_header_size);
        header.extend_from_slice(&self.program_header_count);
        header.extend_from_slice(&self.section_header_size);
        header.extend_from_slice(&self.section_headers);
        header.extend_from_slice(&self.idx_section_header_names);
        header
    }
}

struct ProgramHeader {
    // 01 00 00 00     # 38 p_type: segment type; 1: loadable
    segment_type: [u8; 4],
    // 54 00 00 00     # 3C p_offset: file offset where segment begins
    offset: [u8; 4],
    // 54 80 04 08     # 40 p_vaddr: virtual address of segment in memory (x86: 08048054)
    v_address: [u8; 4],
    // 00 00 00 00     # 44 p_paddr: physical address of segment, unspecified by 386 supplement
    p_address: [u8; 4],
    // 2E 00 00 00     # 48 p_filesz: size in bytes of the segment in the file image ############
    file_size: [u8; 4],
    // 2E 00 00 00     # 4C p_memsz: size in bytes of the segment in memory; p_filesz <= p_memsz
    mem_size: [u8; 4],
    // 05 00 00 00     # 50 p_flags: segment-dependent flags (1: X, 2: W, 4: R)
    flags: [u8; 4],
    // 00 10 00 00     # 54 p_align: 1000 for x86
    align: [u8; 4],
}

impl ProgramHeader {
    pub fn new(file_size: [u8; 4], mem_size: [u8; 4]) -> Self {
        Self {
            segment_type: [0x01, 0x00, 0x00, 0x00],
            offset: [0x54, 0x00, 0x00, 0x00],
            v_address: [0x54, 0x80, 0x04, 0x08],
            p_address: [00, 00, 00, 00],
            file_size,
            mem_size,
            flags: [0x05, 0x00, 0x00, 0x00],
            align: [0x00, 0x10, 0x00, 0x00],
        }
    }

    // pub fn len(&self) -> usize {
    //     0x20
    // }
    pub fn as_bytes(&self) -> Vec<u8> {
        let mut program_header = Vec::with_capacity(32);
        program_header.extend_from_slice(&self.segment_type);
        program_header.extend_from_slice(&self.offset);
        program_header.extend_from_slice(&self.v_address);
        program_header.extend_from_slice(&self.p_address);
        program_header.extend_from_slice(&self.file_size);
        program_header.extend_from_slice(&self.mem_size);
        program_header.extend_from_slice(&self.flags);
        program_header.extend_from_slice(&self.align);
        program_header
    }
}

pub struct Header {
    file_header: FileHeader,
    program_header: ProgramHeader,
}

impl Header {
    pub fn new(program_size: usize) -> Self {
        let file_header = FileHeader::new();
        let size = (program_size as u32).to_le_bytes();
        Self {
            file_header,
            program_header: ProgramHeader::new(size.clone(), size),
        }
    }

    pub fn v_address(&self) -> [u8; 4] {
        self.program_header.v_address.clone()
    }

    // fn len(&self) -> usize {
    //     self.file_header.len() + self.program_header.len()
    // }

    pub fn as_bytes(&self) -> Vec<u8> {
        let fh = self.file_header.as_bytes();
        let ph = self.program_header.as_bytes();
        let mut fhv = fh.to_vec();
        fhv.extend_from_slice(&ph);
        fhv
    }
}
