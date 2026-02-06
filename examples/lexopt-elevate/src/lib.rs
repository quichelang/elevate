pub mod model;
pub mod parser;

#[cfg(test)]
mod tests {
    use crate::model::{Arg, ParseError};
    use crate::parser;

    #[test]
    fn parses_short_option_with_value() {
        let parser0 = parser::Parser::from_raw("-n 10 file".to_string());
        let next1 = parser::Parser::next(parser0.clone());

        match next1.expect("next should succeed") {
            Some(Arg::Short(name)) => assert_eq!(name, "n"),
            other => panic!("expected short option, got {:?}", other),
        }

        let value = parser::Parser::value(parser0.clone()).expect("value should exist");
        assert_eq!(value, "10");

        let next2 = parser::Parser::next(parser0.clone());
        match next2.expect("next should succeed") {
            Some(Arg::Value(name)) => assert_eq!(name, "file"),
            other => panic!("expected positional value, got {:?}", other),
        }

        let next3 = parser::Parser::next(parser0);
        assert!(next3.expect("next should succeed").is_none());
    }

    #[test]
    fn parses_long_option_with_equals_value() {
        let parser0 = parser::Parser::from_raw("--number=42".to_string());
        let next1 = parser::Parser::next(parser0.clone());

        match next1.expect("next should succeed") {
            Some(Arg::Long(name)) => assert_eq!(name, "number"),
            other => panic!("expected long option, got {:?}", other),
        }

        let value = parser::Parser::value(parser0).expect("value should exist");
        assert_eq!(value, "42");
    }

    #[test]
    fn reports_unexpected_value_if_option_value_is_not_consumed() {
        let parser0 = parser::Parser::from_raw("--name=alice".to_string());
        let next1 = parser::Parser::next(parser0.clone());

        let _ = next1.expect("first token should parse");
        let next2 = parser::Parser::next(parser0);
        let err = next2.expect_err("second next should fail");
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
        let parser0 = parser::Parser::from_raw("-abc".to_string());
        let next1 = parser::Parser::next(parser0.clone());

        match next1.expect("next should succeed") {
            Some(Arg::Short(name)) => assert_eq!(name, "a"),
            other => panic!("expected -a, got {:?}", other),
        }
        let next2 = parser::Parser::next(parser0.clone());
        match next2.expect("next should succeed") {
            Some(Arg::Short(name)) => assert_eq!(name, "b"),
            other => panic!("expected -b, got {:?}", other),
        }
        let next3 = parser::Parser::next(parser0);
        match next3.expect("next should succeed") {
            Some(Arg::Short(name)) => assert_eq!(name, "c"),
            other => panic!("expected -c, got {:?}", other),
        }
    }
}
