use monty::{Executor, Exit};

// ============================================================================
// String (Str) Tests
// ============================================================================

macro_rules! str_tests {
    ($($name:ident: $code:literal, $expected:expr;)*) => {
        $(
            paste::item! {
                #[test]
                fn [< str_ $name >]() {
                    let mut ex = Executor::new($code, "test.py", &[]).unwrap();
                    let result = ex.run(vec![]);
                    let output = match result {
                        Ok(Exit::Return(obj)) => format!("{}: {}", obj.py_type(), obj.py_repr()),
                        otherwise => panic!("Unexpected exit: {:?}", otherwise),
                    };
                    let expected = $expected.trim_matches('\n');
                    assert_eq!(output, expected);
                }
            }
        )*
    }
}

str_tests! {
    // py_add tests
    add_basic: "'hello' + ' ' + 'world'", "str: 'hello world'";
    add_empty: "'' + 'test'", "str: 'test'";
    add_empty_both: "'' + ''", "str: ''";
    add_multiple: "'a' + 'b' + 'c' + 'd'", "str: 'abcd'";

    // py_iadd tests
    iadd_basic: "
s = 'hello'
s += ' world'
s
", "str: 'hello world'";
    iadd_multiple: "
s = 'a'
s += 'b'
s += 'c'
s
", "str: 'abc'";
    iadd_empty: "
s = 'test'
s += ''
s
", "str: 'test'";
    iadd_self: "
s = 'ab'
s += s
s
", "str: 'abab'";

    // py_len tests
    len_empty: "len('')", "int: 0";
    len_single: "len('a')", "int: 1";
    len_basic: "len('hello')", "int: 5";
    len_after_concat: "len('abc' + 'def')", "int: 6";
    len_unicode: "len('hello')", "int: 5";

    // Combined operations
    concat_in_loop: "
result = ''
for i in range(5):
    result += 'x'
len(result)
", "int: 5";
}

// ============================================================================
// Bytes Tests
// ============================================================================

macro_rules! bytes_tests {
    ($($name:ident: $code:literal, $expected:expr;)*) => {
        $(
            paste::item! {
                #[test]
                fn [< bytes_ $name >]() {
                    let mut ex = Executor::new($code, "test.py", &[]).unwrap();
                    let result = ex.run(vec![]);
                    let output = match result {
                        Ok(Exit::Return(obj)) => format!("{}: {}", obj.py_type(), obj.py_repr()),
                        otherwise => panic!("Unexpected exit: {:?}", otherwise),
                    };
                    let expected = $expected.trim_matches('\n');
                    assert_eq!(output, expected);
                }
            }
        )*
    }
}

bytes_tests! {
    // py_len tests
    len_empty: "len(b'')", "int: 0";
    len_basic: "len(b'hello')", "int: 5";
}

// ============================================================================
// List Tests
// ============================================================================

macro_rules! list_tests {
    ($($name:ident: $code:literal, $expected:expr;)*) => {
        $(
            paste::item! {
                #[test]
                fn [< list_ $name >]() {
                    let mut ex = Executor::new($code, "test.py", &[]).unwrap();
                    let result = ex.run(vec![]);
                    let output = match result {
                        Ok(Exit::Return(obj)) => format!("{}: {}", obj.py_type(), obj.py_repr()),
                        otherwise => panic!("Unexpected exit: {:?}", otherwise),
                    };
                    let expected = $expected.trim_matches('\n');
                    assert_eq!(output, expected);
                }
            }
        )*
    }
}

list_tests! {
    // py_add tests
    add_basic: "[1, 2] + [3, 4]", "list: [1, 2, 3, 4]";
    add_empty_left: "[] + [1, 2]", "list: [1, 2]";
    add_empty_right: "[1, 2] + []", "list: [1, 2]";
    add_empty_both: "[] + []", "list: []";
    add_multiple: "[1] + [2] + [3]", "list: [1, 2, 3]";
    add_nested: "[[1]] + [[2]]", "list: [[1], [2]]";

    // py_iadd tests
    iadd_basic: "
lst = [1, 2]
lst += [3, 4]
lst
", "list: [1, 2, 3, 4]";
    iadd_empty: "
lst = [1]
lst += []
lst
", "list: [1]";
    iadd_self: "
lst = [1, 2]
lst += lst
lst
", "list: [1, 2, 1, 2]";
    iadd_multiple: "
lst = [1]
lst += [2]
lst += [3]
lst
", "list: [1, 2, 3]";

    // py_len tests
    len_empty: "len([])", "int: 0";
    len_basic: "len([1, 2, 3])", "int: 3";
    len_after_append: "
lst = [1]
lst.append(2)
len(lst)
", "int: 2";
    len_after_concat: "len([1] + [2, 3])", "int: 3";

    // py_call_attr tests (insert)
    insert_beginning: "
lst = [2, 3]
lst.insert(0, 1)
lst
", "list: [1, 2, 3]";
    insert_middle: "
lst = [1, 3]
lst.insert(1, 2)
lst
", "list: [1, 2, 3]";
    insert_end: "
lst = [1, 2]
lst.insert(2, 3)
lst
", "list: [1, 2, 3]";
    insert_out_of_bounds: "
lst = [1, 2]
lst.insert(10, 3)
lst
", "list: [1, 2, 3]";

    // Combined operations
    mixed_operations: "
lst = [1]
lst.append(2)
lst += [3, 4]
lst.insert(0, 0)
lst
", "list: [0, 1, 2, 3, 4]";

    // Shared reference tests
    shared_ref_iadd: "
a = [1, 2]
b = a
a += [3]
b
", "list: [1, 2, 3]";
}

// ============================================================================
// Tuple Tests
// ============================================================================

macro_rules! tuple_tests {
    ($($name:ident: $code:literal, $expected:expr;)*) => {
        $(
            paste::item! {
                #[test]
                fn [< tuple_ $name >]() {
                    let mut ex = Executor::new($code, "test.py", &[]).unwrap();
                    let result = ex.run(vec![]);
                    let output = match result {
                        Ok(Exit::Return(obj)) => format!("{}: {}", obj.py_type(), obj.py_repr()),
                        otherwise => panic!("Unexpected exit: {:?}", otherwise),
                    };
                    let expected = $expected.trim_matches('\n');
                    assert_eq!(output, expected);
                }
            }
        )*
    }
}

tuple_tests! {
    // py_len tests
    len_empty: "len(())", "int: 0";
    len_single: "len((1,))", "int: 1";
    len_basic: "len((1, 2, 3))", "int: 3";

    // Immutability (tuples shouldn't support mutation)
    nested: "((1, 2), (3, 4))", "tuple: ((1, 2), (3, 4))";
}

// ============================================================================
// Integer (Int) Tests
// ============================================================================

macro_rules! int_tests {
    ($($name:ident: $code:literal, $expected:expr;)*) => {
        $(
            paste::item! {
                #[test]
                fn [< int_ $name >]() {
                    let mut ex = Executor::new($code, "test.py", &[]).unwrap();
                    let result = ex.run(vec![]);
                    let output = match result {
                        Ok(Exit::Return(obj)) => format!("{}: {}", obj.py_type(), obj.py_repr()),
                        otherwise => panic!("Unexpected exit: {:?}", otherwise),
                    };
                    let expected = $expected.trim_matches('\n');
                    assert_eq!(output, expected);
                }
            }
        )*
    }
}

int_tests! {
    // py_add tests
    add_basic: "1 + 2", "int: 3";
    add_zero: "10 + 0", "int: 10";

    // py_sub tests
    sub_basic: "5 - 3", "int: 2";
    sub_zero: "10 - 0", "int: 10";

    // py_mod tests
    mod_basic: "10 % 3", "int: 1";
    mod_zero_result: "10 % 5", "int: 0";
    mod_larger_divisor: "3 % 10", "int: 3";

    // py_iadd tests
    iadd_basic: "
x = 5
x += 3
x
", "int: 8";
    iadd_multiple: "
x = 1
x += 2
x += 3
x
", "int: 6";

    // py_mod_eq optimization test
    mod_eq_true: "13 % 13 == 0", "bool: True";
    mod_eq_false: "13 % 10 == 0", "bool: False";
}

// ============================================================================
// Mixed Type Error Tests
// ============================================================================

macro_rules! type_error_tests {
    ($($name:ident: $code:literal, $expected_exc:expr;)*) => {
        $(
            paste::item! {
                #[test]
                fn [< type_error_ $name >]() {
                    let mut ex = Executor::new($code, "test.py", &[]).unwrap();
                    let result = ex.run(vec![]);
                    let output = match result {
                        Ok(Exit::Raise(exc_raise)) => {
                            format!("{}", exc_raise.exc)
                        }
                        otherwise => panic!("Unexpected result: {:?}", otherwise),
                    };
                    let expected = $expected_exc.trim_matches('\n');
                    assert_eq!(output, expected);
                }
            }
        )*
    }
}

type_error_tests! {
    // String + non-string
    str_add_int: "'hello' + 1", "TypeError(\"unsupported operand type(s) for +: 'str' and 'int'\")";

    // List + non-list
    list_add_int: "[1, 2] + 3", "TypeError(\"unsupported operand type(s) for +: 'list' and 'int'\")";
    list_add_str: "[1] + 'x'", "TypeError(\"unsupported operand type(s) for +: 'list' and 'str'\")";

    // Int operations with incompatible types
    int_sub_str: "5 - 'x'", "TypeError(\"unsupported operand type(s) for -: 'int' and 'str'\")";
    int_mod_str: "5 % 'x'", "TypeError(\"unsupported operand type(s) for %: 'int' and 'str'\")";
}

// ============================================================================
// Edge Case Tests
// ============================================================================

macro_rules! edge_case_tests {
    ($($name:ident: $code:literal, $expected:expr;)*) => {
        $(
            paste::item! {
                #[test]
                fn [< edge_ $name >]() {
                    let mut ex = Executor::new($code, "test.py", &[]).unwrap();
                    let result = ex.run(vec![]);
                    let output = match result {
                        Ok(Exit::Return(obj)) => format!("{}: {}", obj.py_type(), obj.py_repr()),
                        otherwise => panic!("Unexpected exit: {:?}", otherwise),
                    };
                    let expected = $expected.trim_matches('\n');
                    assert_eq!(output, expected);
                }
            }
        )*
    }
}

edge_case_tests! {
    // Large lists
    large_list_concat: "
lst = []
for i in range(100):
    lst += [i]
len(lst)
", "int: 100";

    // Large strings
    large_string_concat: "
s = ''
for i in range(100):
    s += 'x'
len(s)
", "int: 100";

    // Empty containers
    all_empty: "(len([]), len(()), len(''))", "tuple: (0, 0, 0)";

    // Multiple self-references
    list_self_concat_twice: "
lst = [1]
lst += lst
lst += lst
lst
", "list: [1, 1, 1, 1]";

    // Mixed numeric operations
    int_float_mod: "7 % 2.5", "float: 2";
    float_int_mod: "7.5 % 2", "float: 1.5";

    // Comparison with modulus
    mod_comparison_chain: "
count = 0
for i in range(100):
    if i % 7 == 0:
        count += 1
count
", "int: 15";
}
