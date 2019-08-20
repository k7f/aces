mod describe;
mod validate;

pub use describe::Describe;
pub use validate::Validate;

pub trait Command {
    fn name_of_log_file(&self) -> String;

    fn console_level(&self) -> Option<log::LevelFilter> {
        None
    }

    fn run(&self) -> Result<(), Box<dyn std::error::Error>>;
}

pub struct App<'a> {
    app_name: String,
    bin_name: Option<String>,
    cli_args: clap::ArgMatches<'a>,
}

impl<'a> App<'a> {
    pub fn from_clap<'b>(clap_app: clap::App<'a, 'b>) -> Self {
        let cli_app = clap_app
            .name(env!("CARGO_PKG_NAME"))
            .version(env!("CARGO_PKG_VERSION"))
            .author(env!("CARGO_PKG_AUTHORS"))
            .about(env!("CARGO_PKG_DESCRIPTION"));

        let app_name = cli_app.get_name().to_owned();
        let bin_name = cli_app.get_bin_name().map(|s| s.to_owned());
        let cli_args = cli_app.get_matches();

        Self { app_name, bin_name, cli_args }
    }

    pub fn get_name(&self) -> &str {
        self.app_name.as_str()
    }

    pub fn get_bin_name(&self) -> Option<&str> {
        self.bin_name.as_ref().map(|s| s.as_str())
    }

    pub fn subcommand_name(&self) -> Option<&str> {
        self.cli_args.subcommand_name()
    }

    pub fn value_of<S: AsRef<str>>(&self, key: S) -> Option<&str> {
        self.cli_args.subcommand().1.unwrap_or(&self.cli_args).value_of(key)
    }

    pub fn occurrences_of<S: AsRef<str>>(&self, key: S) -> u64 {
        self.cli_args.subcommand().1.unwrap_or(&self.cli_args).occurrences_of(key)
    }

    pub fn is_present<S: AsRef<str>>(&self, key: S) -> bool {
        self.cli_args.subcommand().1.unwrap_or(&self.cli_args).is_present(key)
    }
}
