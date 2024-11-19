use super::*;
use proc_macro2::TokenStream as TokenStream2;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
fn test_export_basic_parsing_valid() {
    export_internal(
        quote!(),
        quote!(
            struct SomeStruct;
        ),
    )
    .unwrap();
    export_internal(
        quote!(some_ident),
        quote!(
            struct SomeStruct;
        ),
    )
    .unwrap();
    export_internal(
        quote!(SomethingSomething),
        quote!(
            struct SomeStruct;
        ),
    )
    .unwrap();
}

#[test]
fn test_export_basic_parsing_invalid() {
    assert!(export_internal(
        quote!(),
        quote!(
            struct SomeStruct
        ),
    )
    .is_err());
    assert!(export_internal(
        quote!(something as something),
        quote!(
            struct SomeStruct;
        ),
    )
    .is_err());
    assert!(export_internal(
        quote!(something something),
        quote!(
            struct SomeStruct;
        ),
    )
    .is_err());
}

#[test]
fn test_compile_markdown_dir() {
    compile_markdown_dir("fixtures", "test_bin").unwrap();
}

#[test]
fn test_compile_markdown_valid() {
    compile_markdown_internal(quote!("fixtures", "test_bin")).unwrap();
    compile_markdown_internal(quote!("fixtures/file_1.md", "test_bin/alternate_output.md"))
        .unwrap();
    assert_eq!(
        compile_markdown_internal(quote!("fixtures/file_1.md"))
            .unwrap()
            .to_string(),
        "\"# This is a markdown file\\n\\n```rust\\nstruct \
        Something;\\n```\\n<!-- this is a comment -->\\n\\n`\
        ``rust,ignore\\nfn some_fn() {\\n    println!(\\\"foo\\\");\
        \\n}\\n```\\n\\nSome text this is some text\\n\""
    );
}

#[test]
fn test_compile_markdown_invalid() {
    assert!(compile_markdown_internal(quote!("&97298", "79*&(")).is_err());
    assert!(compile_markdown_internal(quote!("&97298", "test_bin")).is_err());
    assert!(compile_markdown_internal(quote!("fixtures")).is_err());
    assert!(compile_markdown_internal(quote!("fixtures/file_1.md", "test_bin")).is_err());
    assert!(compile_markdown_internal(quote!("something", "")).is_err());
    assert!(compile_markdown_internal(quote!("", "something")).is_err());
    assert!(compile_markdown_internal(quote!("", "")).is_err());
}

#[test]
fn test_compile_markdown_source_valid() {
    assert_eq!(
        compile_markdown_source(
            "this is some markdown\n\
            this is some more markdown\n\
            # this is a title\n\
            <!-- docify::embed!(\"fixtures/file.rs\", some_fn) -->\n\
            this is some more text\n",
        )
        .unwrap(),
        "this is some markdown\n\
        this is some more markdown\n\
        # this is a title\n\
        ```rust,ignore\n\
        fn some_fn() {\n    \
            println!(\"foo\");\n\
        }\n\
        ```\n\
        this is some more text\n"
    );
    assert!(compile_markdown_source(
        "this is some markdown\n\
        this is some more markdown\n\
        # this is a title\n\
        <!-- docify::embed!(\"fixtures/file.rs\", some_other_fn) -->\n\
        this is some more text\n",
    )
    .unwrap()
    .contains("bar"));
    assert!(compile_markdown_source(
        "this is some markdown\n\
        this is some more markdown\n\
        # this is a title\n\
        <!--docify::embed!(\"fixtures/file.rs\", some_other_fn) -->\n\
        this is some more text\n",
    )
    .unwrap()
    .contains("bar"));
    assert!(compile_markdown_source(
        "this is some markdown\n\
        this is some more markdown\n\
        # this is a title\n\
        <!-- docify::embed!(\"fixtures/file.rs\", some_fn)-->\n\
        this is some more text\n",
    )
    .unwrap()
    .contains("foo"));
    assert!(compile_markdown_source(
        "this is some markdown\n\
        this is some more markdown\n\
        # this is a title\n\
        <!--docify::embed!(\"fixtures/file.rs\", some_fn)-->\n\
        this is some more text\n",
    )
    .unwrap()
    .contains("foo"));
    assert!(compile_markdown_source(
        "<!-- docify::embed!(\"fixtures/file.rs\", some_fn) --> this is some more text\n",
    )
    .unwrap()
    .ends_with("more text\n"));
    assert!(compile_markdown_source(
        "prefix<!-- docify::embed!(\"fixtures/file.rs\", some_fn) -->",
    )
    .unwrap()
    .starts_with("prefix"));
}

#[test]
fn test_compile_markdown_source_invalid() {
    assert!(compile_markdown_source(
        "# this is a title\n\
        <!-- docify:embed!(\"fixtures/file.rs\", some_fn) -->\n\
        this is some more text\n",
    )
    .is_err());
    assert!(compile_markdown_source(
        "# this is a title\n\
        <!-- docify::em!(\"fixtures/file.rs\", some_fn) -->\n\
        this is some more text\n",
    )
    .is_err());
    assert!(compile_markdown_source(
        "# this is a title\n\
        <!-- docify -->\n\
        this is some more text\n",
    )
    .is_err());
}

#[test]
fn test_fix_leading_indentation() {
    let input = r#"    fn foo() {
        println!("foo!");
    }

    fn bar() {
        println!("bar!");
    }
"#;
    let output = r#"fn foo() {
    println!("foo!");
}

fn bar() {
    println!("bar!");
}
"#;
    assert_eq!(fix_leading_indentation(input), output);
}

#[test]
fn test_embed_args_basic() {
    // Test basic file path only
    let args = parse2::<EmbedArgs>(quote!("src/lib.rs")).unwrap();
    assert!(args.git_url.is_none());
    assert_eq!(args.file_path.value(), "src/lib.rs");
    assert!(args.item_ident.is_none());
    assert!(args.branch_name.is_none());
    assert!(args.commit_hash.is_none());
    assert!(args.tag_name.is_none());

    // Test file path with item
    let args = parse2::<EmbedArgs>(quote!("src/lib.rs", my_function)).unwrap();
    assert!(args.git_url.is_none());
    assert_eq!(args.file_path.value(), "src/lib.rs");
    assert_eq!(args.item_ident.unwrap().to_string(), "my_function");
}

#[test]
fn test_embed_args_git() {
    // Test with git URL
    let args = parse2::<EmbedArgs>(quote!(git: "https://github.com/user/repo", path: "src/lib.rs"))
        .unwrap();
    assert_eq!(
        args.git_url.unwrap().value(),
        "https://github.com/user/repo"
    );
    assert_eq!(args.file_path.value(), "src/lib.rs");

    // Test with git URL and branch
    let args = parse2::<EmbedArgs>(quote!(
        git: "https://github.com/user/repo",
        path: "src/lib.rs",
        branch: "main"
    ))
    .unwrap();
    assert_eq!(args.branch_name.unwrap().value(), "main");
}

#[test]
fn test_embed_args_git_refs() {
    // Test with commit hash
    let args = parse2::<EmbedArgs>(quote!(
        git: "https://github.com/user/repo",
        path: "src/lib.rs",
        commit: "abc123"
    ))
    .unwrap();
    assert_eq!(args.commit_hash.unwrap().value(), "abc123");

    // Test with tag
    let args = parse2::<EmbedArgs>(quote!(
        git: "https://github.com/user/repo",
        path: "src/lib.rs",
        tag: "v1.0.0"
    ))
    .unwrap();
    assert_eq!(args.tag_name.unwrap().value(), "v1.0.0");
}

#[test]
fn test_embed_args_with_item() {
    // Test git URL with item
    let args = parse2::<EmbedArgs>(quote!(
        git: "https://github.com/user/repo",
        path: "src/lib.rs",
        item: my_function
    ))
    .unwrap();
    assert_eq!(args.item_ident.unwrap().to_string(), "my_function");
}
#[test]
fn test_embed_args_invalid() {
    // Test empty path
    assert!(parse2::<EmbedArgs>(quote!("")).is_err());

    // Test multiple git refs (should fail)
    assert!(
        parse2::<EmbedArgs>(quote!(
            git: "https://github.com/user/repo",
            path: "src/lib.rs",
            branch: "main",
            tag: "v1.0.0"
        ))
        .is_err(),
        "Should fail when multiple git refs are provided"
    );

    // Test git refs without git URL (should fail)
    assert!(
        parse2::<EmbedArgs>(quote!(
            path: "src/lib.rs",
            branch: "main"
        ))
        .is_err(),
        "Should fail when git refs are provided without git URL"
    );

    // Test missing path with git URL (should fail)
    assert!(
        parse2::<EmbedArgs>(quote!(
            git: "https://github.com/user/repo"
        ))
        .is_err(),
        "Should fail when path is missing"
    );

    // Test invalid URL format
    assert!(
        parse2::<EmbedArgs>(quote!(
            git: "not a valid url",
            path: "src/lib.rs"
        ))
        .is_err(),
        "Should fail with invalid git URL format"
    );

    // Test empty git URL
    assert!(
        parse2::<EmbedArgs>(quote!(
            git: "",
            path: "src/lib.rs"
        ))
        .is_err(),
        "Should fail with empty git URL"
    );
}
#[test]
fn test_embed_args_complex() {
    // Test full featured usage
    let args = parse2::<EmbedArgs>(quote!(
        git: "https://github.com/user/repo",
        path: "src/lib.rs",
        branch: "feature/new",
        item: test_function
    ))
    .unwrap();

    assert_eq!(
        args.git_url.unwrap().value(),
        "https://github.com/user/repo"
    );
    assert_eq!(args.file_path.value(), "src/lib.rs");
    assert_eq!(args.branch_name.unwrap().value(), "feature/new");
    assert_eq!(args.item_ident.unwrap().to_string(), "test_function");
}
