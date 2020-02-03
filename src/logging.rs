use std::path::PathBuf;
use fern::colors::{Color, ColoredLevelConfig};

macro_rules! error_pre_log {
    ($lgr:expr,$($arg:tt)*) => (
        eprintln!("{}{}{}",
                  $lgr.console_prefix(log::Level::Error),
                  format_args!($($arg)*),
                  $lgr.console_suffix()));
}

#[derive(Default)]
pub struct Logger {
    app_name:       String,
    dispatcher:     Option<fern::Dispatch>,
    level_colors:   ColoredLevelConfig,
    message_colors: ColoredLevelConfig,
    directory:      Option<PathBuf>,
}

impl Logger {
    pub fn new<S: AsRef<str>>(app_name: S) -> Self {
        let app_name = app_name.as_ref().to_owned();
        let dispatcher = Some(fern::Dispatch::new());

        // FIXME these should be configurable by the user
        let level_colors = ColoredLevelConfig::new()
            .trace(Color::Blue)
            .debug(Color::Yellow)
            .info(Color::BrightGreen)
            .warn(Color::BrightMagenta)
            .error(Color::BrightRed);

        let message_colors = ColoredLevelConfig::new()
            .trace(Color::White)
            .debug(Color::White)
            .info(Color::BrightWhite)
            .warn(Color::BrightWhite)
            .error(Color::BrightWhite);

        Self { app_name, dispatcher, level_colors, message_colors, directory: None }
    }

    pub fn with_console(mut self, level: log::LevelFilter) -> Self {
        let level_colors = self.level_colors;
        let message_colors = self.message_colors;

        let dispatcher = self.dispatcher.unwrap().chain(
            fern::Dispatch::new()
                .format(move |out, message, record| match record.level() {
                    log::Level::Info => out.finish(format_args!("{}.", message)),
                    _ => out.finish(format_args!(
                        "[{}]\t\x1B[{}m{}.\x1B[0m",
                        level_colors.color(record.level()),
                        message_colors.get_color(&record.level()).to_fg_str(),
                        message
                    )),
                })
                .level(level)
                .chain(std::io::stdout()),
        );

        self.dispatcher = Some(dispatcher);
        self
    }

    pub fn with_explicit_directory<S: AsRef<str>>(mut self, dirname: S) -> Self {
        let path = PathBuf::from(dirname.as_ref());

        if path.is_dir() {
            self.directory = Some(path);
        } else if path.exists() {
            error_pre_log!(
                self,
                "Can't use \"{}\" as a logging directory, because it exists and isn't a directory.",
                path.display(),
            );
            self.directory = None;
        } else if let Err(err) = std::fs::create_dir(&path) {
            error_pre_log!(self, "Can't create \"{}\" directory: {}.", path.display(), err);
            self.directory = None;
        } else {
            self.directory = Some(path);
        }

        self
    }

    pub fn with_file<S: AsRef<str>>(mut self, filename: S, level: log::LevelFilter) -> Self {
        if self.directory.is_none() {
            let path = PathBuf::from("log");

            if path.is_dir() {
                self.directory = Some(path);
            } else {
                error_pre_log!(
                    self,
                    "Logging to file is disabled, because directory \"log\" doesn't \
                     exist...\n\tCreate this directory or run '{} --log-dir <LOG_DIR> ...'.",
                    self.app_name
                );
            }
        }

        if let Some(ref mut path) = self.directory {
            let path = path.join(filename.as_ref());

            let log_file = std::fs::OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .append(false)
                .open(path);

            match log_file {
                Ok(log_file) => {
                    let dispatcher = self.dispatcher.unwrap().chain(
                        fern::Dispatch::new()
                            .format(move |out, message, record| {
                                out.finish(format_args!(
                                    "[{}][{}] {}.",
                                    record.target(),
                                    record.level(),
                                    message,
                                ))
                            })
                            .level(level)
                            .chain(log_file),
                    );
                    self.dispatcher = Some(dispatcher);
                }
                Err(err) => {
                    error_pre_log!(self, "{}.", err);
                }
            }
        }

        self
    }

    pub fn get_directory(&self) -> Option<&PathBuf> {
        self.directory.as_ref()
    }

    pub fn apply(&mut self) {
        if let Some(dispatcher) = self.dispatcher.take() {
            dispatcher.apply().unwrap_or_else(|err| error_pre_log!(self, "{}.", err));
        } else {
            error_pre_log!(self, "Logger can't be applied (probably it has already been applied).");
        }
    }

    fn console_prefix(&self, level: log::Level) -> String {
        format!(
            "[{}]\t\x1B[{}m",
            self.level_colors.color(level),
            self.message_colors.get_color(&level).to_fg_str()
        )
    }

    fn console_suffix(&self) -> &str {
        "\x1B[0m"
    }
}
