#![allow(dead_code)]
use super::{Header, Monic};
use std::collections::HashMap;

pub struct Program {
    instructions: Vec<Monic>,
    labels: HashMap<String, u32>,
    data: Vec<(String, String)>,
    size: u32,
}

impl Program {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            labels: HashMap::new(),
            data: vec![],
            size: 0,
        }
    }
    pub fn with_op(mut self, instruction: Monic) -> Self {
        self.size += instruction.size();
        self.instructions.push(instruction);
        self
    }

    pub fn with_data(mut self, name: &str, data: &str) -> Self {
        self.data.push((name.into(), data.into()));
        self
    }

    pub fn with_label(mut self, name: &str) -> Self {
        println!("{:02X}: {}", self.size, name);
        self.labels.insert(name.into(), self.size() + 0x54);
        self
    }

    pub fn label(&mut self, name: &str) {
        println!("{}: # {}", name, self.size);
        self.labels.insert(name.into(), self.size() + 0x54);
    }

    pub fn op(&mut self, instruction: Monic) {
        self.size += instruction.size();
        self.instructions.push(instruction);
    }

    pub fn data(&mut self, name: &str, data: &str) {
        self.data.push((name.into(), data.into()));
    }

    fn size(&self) -> u32 {
        self.size
    }

    fn data_size(&self) -> usize {
        self.data.iter().fold(0, |acc, (_, data)| acc + data.len())
    }

    pub fn build(self) -> Vec<u8> {
        let header = Header::new(self.size() as usize + self.data_size());
        let mut data_seg = Vec::new();
        let mut data_loc = HashMap::<String, u32>::new();
        let header_len = u32::from_le_bytes(header.v_address());
        let mut pointer = header_len + self.size();
        for (name, data) in self.data.iter() {
            data_loc.insert(name.into(), pointer);
            data_seg.extend_from_slice(data.as_bytes());
            pointer += data.len() as u32;
        }

        let program = {
            let mut bytes = Vec::new();
            let mut current = 0;
            for monic in self.instructions.iter() {
                current += monic.size();
                let code = monic.code(current + 0x54, &self.labels, &data_loc);
                let c = code
                    .iter()
                    .map(|i| format!("{:02X}", i))
                    .collect::<String>();
                eprintln!(
                    "{:02X} {:02X}: {:<15} {} {}",
                    current + 0x54,
                    current,
                    format!("{:%<15}", monic),
                    format!("{:<10}", c),
                    monic.size()
                );
                bytes.extend_from_slice(&code);
            }
            bytes
        };
        eprintln!("{:02X}: PROGRAM LEN", self.size());
        eprintln!("{:02X}: DATA LEN", data_seg.len());
        eprintln!("{:02X}: DATA START", self.size() + 0x54);
        eprintln!(
            "{:02X}: DATA & PROGRAM LEN",
            data_seg.len() as u32 + self.size()
        );
        eprintln!(
            "{:02X}: Size\n{:02X}: Estimation",
            program.len(),
            self.size()
        );
        assert_eq!(program.len() as u32, self.size());
        let mut elf = header.as_bytes();
        elf.extend_from_slice(&program);
        elf.extend_from_slice(&data_seg);
        elf
    }
}
