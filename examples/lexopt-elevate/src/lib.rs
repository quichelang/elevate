pub mod model;
pub mod parser;
pub mod runtime;

#[cfg(test)]
mod tests {
    use crate::model::{Arg, ParseError};
    use crate::parser;

    #[test]
    fn parses_short_option_with_value() {
        let handle = parser::from_raw("-n 10 file".to_string());

        match parser::next(handle).expect("next should succeed") {
            Some(Arg::Short(name)) => assert_eq!(name, "n"),
            other => panic!("expected short option, got {:?}", other),
        }

        let value = parser::value(handle).expect("value should exist");
        assert_eq!(value, "10");

        match parser::next(handle).expect("next should succeed") {
            Some(Arg::Value(name)) => assert_eq!(name, "file"),
            other => panic!("expected positional value, got {:?}", other),
        }

        assert!(parser::next(handle).expect("next should succeed").is_none());
    }

    #[test]
    fn parses_long_option_with_equals_value() {
        let handle = parser::from_raw("--number=42".to_string());

        match parser::next(handle).expect("next should succeed") {
            Some(Arg::Long(name)) => assert_eq!(name, "number"),
            other => panic!("expected long option, got {:?}", other),
        }

        let value = parser::value(handle).expect("value should exist");
        assert_eq!(value, "42");
    }

    #[test]
    fn reports_unexpected_value_if_option_value_is_not_consumed() {
        let handle = parser::from_raw("--name=alice".to_string());

        let _ = parser::next(handle).expect("first token should parse");
        let err = parser::next(handle).expect_err("second next should fail");
        match err {
            ParseError::UnexpectedValue(message) => {
                assert!(message.contains("--name"));
                assert!(message.contains("alice"));
            }
            other => panic!("expected UnexpectedValue, got {:?}", other),
        }
    }

    #[test]
    fn parses_combined_short_options() {
        let handle = parser::from_raw("-abc".to_string());

        match parser::next(handle).expect("next should succeed") {
            Some(Arg::Short(name)) => assert_eq!(name, "a"),
            other => panic!("expected -a, got {:?}", other),
        }
        match parser::next(handle).expect("next should succeed") {
            Some(Arg::Short(name)) => assert_eq!(name, "b"),
            other => panic!("expected -b, got {:?}", other),
        }
        match parser::next(handle).expect("next should succeed") {
            Some(Arg::Short(name)) => assert_eq!(name, "c"),
            other => panic!("expected -c, got {:?}", other),
        }
    }
}
