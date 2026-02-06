use lexopt_elevate::model::Arg;
use lexopt_elevate::parser::Parser;

fn main() {
    if std::env::args().skip(1).any(|arg| arg == "--help" || arg == "-h") {
        print_usage();
        return;
    }

    let parser = Parser::from_env();
    let handle = Parser::handle_id(parser);

    loop {
        match Parser::next(handle) {
            Ok(Some(Arg::Short(name))) => println!("short:{name}"),
            Ok(Some(Arg::Long(name))) => {
                println!("long:{name}");
                if let Some(value) = Parser::optional_value(handle) {
                    println!("long-value:{value}");
                }
            }
            Ok(Some(Arg::Value(value))) => println!("value:{value}"),
            Ok(None) => break,
            Err(error) => {
                eprintln!("parse-error:{error:?}");
                std::process::exit(1);
            }
        }
    }
}

fn print_usage() {
    eprintln!("usage: lexopt-elevate [args...]");
    eprintln!("prints parsed argument tokens using the Elevate parser implementation");
}
