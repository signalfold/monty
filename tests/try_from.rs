use monty::{Executor, Exit};

/// Tests for successful TryFrom conversions from Python values to Rust types.
///
/// These tests validate that the `TryFrom` implementations on `Value` correctly
/// convert Python objects to their corresponding Rust types when the conversion
/// is valid (e.g., Python int to Rust i64, Python str to Rust String).

#[test]
#[allow(clippy::float_cmp)]
fn try_from_ok_int_to_i64() {
    let mut ex = Executor::new("42", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(ref obj) => {
            let value: i64 = obj.try_into().expect("conversion should succeed");
            assert_eq!(value, 42);
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn try_from_ok_zero_to_i64() {
    let mut ex = Executor::new("0", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(ref obj) => {
            let value: i64 = obj.try_into().expect("conversion should succeed");
            assert_eq!(value, 0);
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn try_from_ok_float_to_f64() {
    let mut ex = Executor::new("2.5", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(ref obj) => {
            let value: f64 = obj.try_into().expect("conversion should succeed");
            assert_eq!(value, 2.5);
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn try_from_ok_int_to_f64() {
    let mut ex = Executor::new("42", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(ref obj) => {
            let value: f64 = obj.try_into().expect("conversion should succeed");
            assert_eq!(value, 42.0);
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn try_from_ok_string_to_string() {
    let mut ex = Executor::new("'hello'", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(ref obj) => {
            let value: String = obj.try_into().expect("conversion should succeed");
            assert_eq!(value, "hello".to_string());
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn try_from_ok_empty_string_to_string() {
    let mut ex = Executor::new("''", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(ref obj) => {
            let value: String = obj.try_into().expect("conversion should succeed");
            assert_eq!(value, String::new());
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn try_from_ok_multiline_string_to_string() {
    let mut ex = Executor::new("'hello\\nworld'", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(ref obj) => {
            let value: String = obj.try_into().expect("conversion should succeed");
            assert_eq!(value, "hello\nworld".to_string());
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn try_from_ok_bool_true_to_bool() {
    let mut ex = Executor::new("True", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(ref obj) => {
            let value: bool = obj.try_into().expect("conversion should succeed");
            assert!(value);
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn try_from_ok_bool_false_to_bool() {
    let mut ex = Executor::new("False", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(ref obj) => {
            let value: bool = obj.try_into().expect("conversion should succeed");
            assert!(!value);
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

/// Tests for failed TryFrom conversions from Python values to Rust types.
///
/// These tests validate that the `TryFrom` implementations correctly reject
/// invalid conversions with appropriate error messages (e.g., trying to convert
/// a Python str to a Rust i64).

#[test]
fn try_from_err_string_to_i64() {
    let mut ex = Executor::new("'hello'", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(obj) => {
            let err = TryInto::<i64>::try_into(&obj).expect_err("conversion should fail");
            assert_eq!(err.to_string(), "expected int, got str");
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
fn try_from_err_float_to_i64() {
    let mut ex = Executor::new("2.5", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(obj) => {
            let err = TryInto::<i64>::try_into(&obj).expect_err("conversion should fail");
            assert_eq!(err.to_string(), "expected int, got float");
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
fn try_from_err_none_to_i64() {
    let mut ex = Executor::new("None", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(obj) => {
            let err = TryInto::<i64>::try_into(&obj).expect_err("conversion should fail");
            assert_eq!(err.to_string(), "expected int, got NoneType");
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
fn try_from_err_list_to_i64() {
    let mut ex = Executor::new("[1, 2, 3]", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(obj) => {
            let err = TryInto::<i64>::try_into(&obj).expect_err("conversion should fail");
            assert_eq!(err.to_string(), "expected int, got list");
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
fn try_from_err_int_to_string() {
    let mut ex = Executor::new("42", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(obj) => {
            let err = TryInto::<String>::try_into(&obj).expect_err("conversion should fail");
            assert_eq!(err.to_string(), "expected str, got int");
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
fn try_from_err_none_to_string() {
    let mut ex = Executor::new("None", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(obj) => {
            let err = TryInto::<String>::try_into(&obj).expect_err("conversion should fail");
            assert_eq!(err.to_string(), "expected str, got NoneType");
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
fn try_from_err_list_to_string() {
    let mut ex = Executor::new("[1, 2]", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(obj) => {
            let err = TryInto::<String>::try_into(&obj).expect_err("conversion should fail");
            assert_eq!(err.to_string(), "expected str, got list");
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
fn try_from_err_int_to_bool() {
    let mut ex = Executor::new("1", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(obj) => {
            let err = TryInto::<bool>::try_into(&obj).expect_err("conversion should fail");
            assert_eq!(err.to_string(), "expected bool, got int");
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
fn try_from_err_string_to_bool() {
    let mut ex = Executor::new("'true'", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(obj) => {
            let err = TryInto::<bool>::try_into(&obj).expect_err("conversion should fail");
            assert_eq!(err.to_string(), "expected bool, got str");
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}

#[test]
fn try_from_err_none_to_bool() {
    let mut ex = Executor::new("None", "test.py", &[]).unwrap();
    let result = ex.run(vec![]).unwrap();
    match result {
        Exit::Return(obj) => {
            let err = TryInto::<bool>::try_into(&obj).expect_err("conversion should fail");
            assert_eq!(err.to_string(), "expected bool, got NoneType");
        }
        otherwise @ Exit::Raise(_) => panic!("Unexpected exit: {otherwise:?}"),
    }
}
