//! ```text
//! ├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├├
//! ```

#![allow(unused)]

use proc_utils::*;

#[docif::export]
struct MyCoolStruct {
    field1: u32,
    field2: bool,
}

#[docif::export]
#[test]
fn some_random_test() {
    assert_eq!(2 + 2, 4);
}

#[docif::export(test_with_custom_name)]
#[test]
fn another_test() {
    assert_eq!(2 + 3, 5);
}

trait DoSomething {
    fn do_something();
}

#[docif::export(SomeImpl)]
impl DoSomething for MyCoolStruct {
    fn do_something() {
        println!("foo!");
    }
}

#[docif::export(Duplicate)]
struct _StructOne;

#[docif::export(Duplicate)]
struct _StructTwo;

#[docif::export(Duplicate)]
struct _StructThree;

#[docif::export]
#[allow(unused)]
fn runnable() {
    assert_eq!(2 + 2, 4);
}

// This one gets embedded automatically in README.md and src/lib.rs!
#[docif::export]
fn some_example() {
    assert_eq!(2 + 2, 4);
    assert_eq!(2 + 3, 5);
    assert_eq!(3 + 3, 6);
}

/// Some doc comments
#[docif::export]
fn some_complex_example() {
    // some comments
    /// some doc comments
    assert_eq!(2 + 2, 4);
    assert_eq!(2 + 3, 5);
    /* some multi line
    comment that spans multiple
    "string literal in multi-line comment"
    // comment in a comment
    lines */
    // "string literal in comment"
    /// "string literal in doc comment"
    assert_eq!(3 + 3, 6);
}

#[rustfmt::skip]
mod bad {
    #[docif::export]
    fn
    wonky_comment_example() { /* first comment */
       // this is a line comment
                // this is also a line comment
            /*
        some multilinestuff
    */
                            println!("hello world");
        }
}

#[docif::export]
#[test]
fn test_with_normal_ordering() {
    assert_eq!(2 + 2, 4);
}

#[test]
#[docif::export]
/// This is a comment
fn test_with_weird_ordering() {
    assert_eq!(2 + 2, 4);
}

#[test]
#[docif::export]
/// This is a comment plus `#[docif::export]`
/// `#[docif::export]`
fn docif_keyword_in_comments() {
    assert_eq!(2 + 3, 5);
}

mod some_module {
    use super::*;

    #[docif::export]
    #[rustfmt::skip]
    #[suppress_item]
    fn oliver_substrate_example_2() {
        // a line comment
        assert_events(vec![
            UpgradeStarted { migrations: 2 },
            /// A doc comment
            MigrationAdvanced { index: 0, blocks: 1 },
            MigrationCompleted { index: 0, blocks: 2 },
            MigrationAdvanced { index: 1, blocks: 0 },
            /// Another doc comment
            MigrationAdvanced { index: 1, blocks: 1 },
            MigrationCompleted { index: 1, blocks: 2 },
            UpgradeCompleted,
        ]);
    }
}

#[docif::export]
/// Example struct holding the most recently set [`u32`] and the second
/// most recently set [`u32`] (if one existed).
struct LiamIssue7;

fn main() {}

trait SomeTrait {
    fn trait_impl_method();
}

pub struct Liam9;

impl SomeTrait for Liam9 {
    #[docif::export]
    fn trait_impl_method() {
        println!("foo!");
    }
}

#[docif::export_content]
trait SomeOtherTrait {
    fn foo();
    fn bar();
    type Something;
}

#[docif::export_content(impl_some_other_trait_for_my_cool_struct)]
impl SomeOtherTrait for MyCoolStruct {
    fn foo() {
        println!("foo!");
    }

    fn bar() {
        println!("bar!");
    }

    type Something = ();
}

#[docif::export_content]
pub fn some_other_fn(x: i32, y: i32) -> Result<i32, i32> {
    if x > 10 {
        Ok(33)
    } else if y < 10 {
        Ok(44)
    } else {
        Err(420)
    }
}

#[docif::export_content]
const MY_CONST: &'static str = "hello world";

#[docif::export]
pub mod outer_mod {

    pub fn hello() {
        println!("hello");
    }

    #[docif::export]
    pub fn outer_foo() {
        println!("foo!");
    }

    #[docif::export]
    pub mod inner_mod {
        const SOMETHING: i32 = 55;

        #[docif::export]
        pub fn inner_inner_bar() {
            println!("bar!");
        }

        #[docif::export_content]
        pub fn inner_inner_fizz() {
            println!("fizz!");
        }
    }

    #[docif::export_content]
    pub mod inner_mod2 {
        #[docif::export]
        pub fn inner_inner_wiz() {
            println!("wiz!");
        }
    }
}
