mod describe;
mod validate;

pub use describe::Describe;
pub use validate::Validate;

pub trait Command {
    fn run(app: &App) -> Result<(), Box<dyn std::error::Error>>;
}

pub struct App<'a> {
    cli_args: clap::ArgMatches<'a>,
}

impl<'a> App<'a> {
    pub fn from_clap<'b>(clap_app: clap::App<'a, 'b>) -> Self {
        let cli_args = clap_app
            .name(env!("CARGO_PKG_NAME"))
            .version(env!("CARGO_PKG_VERSION"))
            .author(env!("CARGO_PKG_AUTHORS"))
            .about(env!("CARGO_PKG_DESCRIPTION"))
            .get_matches();

        Self { cli_args }
    }

    pub fn subcommand_name(&self) -> Option<&str> {
        self.cli_args.subcommand_name()
    }

    pub fn value_of(&self, key: &str) -> Option<&str> {
        self.cli_args.subcommand().1
            .unwrap_or(&self.cli_args)
            .value_of(key)
    }

    pub fn occurrences_of(&self, key: &str) -> u64 {
        self.cli_args.subcommand().1
            .unwrap_or(&self.cli_args)
            .occurrences_of(key)
    }

    pub fn is_present(&self, key: &str) -> bool {
        self.cli_args.subcommand().1
            .unwrap_or(&self.cli_args)
            .is_present(key)
    }
}
