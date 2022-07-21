use clap::{crate_description, crate_name, crate_version, Arg, Command};

#[derive(Debug)]
pub enum Assember {
    Custom,
    Fasm,
}

#[derive(Debug)]
pub enum Target {
    X86_64Lunux,
}

#[derive(Debug)]
pub struct Settings {
    pub filename: String,
    pub assembler: Assember,
    pub target: Target,
}

impl Settings {
    fn filename(&mut self, filename: &str) {
        self.filename = filename.into();
    }

    fn x86_64_linux(&mut self) {
        self.target = Target::X86_64Lunux;
    }

    fn fasm(&mut self) {
        self.assembler = Assember::Fasm;
    }
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            filename: "".into(),
            assembler: Assember::Custom,
            target: Target::X86_64Lunux,
        }
    }
}

pub fn cargs() -> Settings {
    let matches = Command::new(crate_name!())
        .version(crate_version!())
        .author("Cowboy8625")
        .about(crate_description!())
        .arg(Arg::new("filenames").required(true).multiple_values(true))
        .arg(
            Arg::new("target")
                .short('t')
                .long("target")
                .help("target to build to")
                .takes_value(true),
        )
        .arg(
            Arg::new("backend")
                .short('b')
                .long("backend")
                .help("backend use of compilers.  Compile with fasm or home made.")
                .takes_value(true),
        )
        .get_matches();

    let mut setting = Settings::default();
    match matches.value_of("filenames") {
        Some(filename) => setting.filename(filename),
        None => {
            eprintln!("a filename is needed");
            std::process::exit(0);
        }
    }
    match matches.value_of("target") {
        Some("x86_64-linux") => setting.x86_64_linux(),
        Some(t) => {
            eprintln!("'{}' is not an option for 'target'", t);
            std::process::exit(0);
        }
        None => {}
    }
    match matches.value_of("backend") {
        Some("fasm") => setting.fasm(),
        Some(wrong) => {
            eprintln!("'{}' is not an option for 'backend'", wrong);
            std::process::exit(0);
        }
        None => {}
    }
    //
    //
    // let shading = matches.is_present("shade");

    setting
}
