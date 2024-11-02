use super::*;
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
        ``rust\\nfn some_fn() {\\n    println!(\\\"foo\\\");\
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
        ```rust\n\
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

#[cfg(test)]
mod embed_args_tests {
    use super::*;
    use quote::quote;
    use syn::parse2;

    // Fixed helper function - using syn::Result instead of std::result::Result
    fn assert_error_contains(result: syn::Result<EmbedArgs>, expected: &str) {
        match result {
            Ok(_) => panic!("Expected error, got success"),
            Err(e) => assert!(
                e.to_string().contains(expected),
                "Error message '{}' should contain '{}'",
                e.to_string(),
                expected
            ),
        }
    }

    #[test]
    fn test_basic_positional_args() {
        // Test: Basic file path only
        let input = quote!("src/lib.rs");
        let args = parse2::<EmbedArgs>(input.clone()).unwrap();
        assert_eq!(args.file_path.value(), "src/lib.rs");
        assert!(args.item_ident.is_none());

        // Test: File path with item identifier
        let input = quote!("src/lib.rs", my_function);
        let args = parse2::<EmbedArgs>(input.clone()).unwrap();
        assert_eq!(args.file_path.value(), "src/lib.rs");
        assert_eq!(args.item_ident.unwrap().to_string(), "my_function");
    }

    #[test]
    fn test_named_args_basic() {
        let input = quote!(
            git: "https://github.com/user/repo",
            path: "src/lib.rs",
            branch: "main"
        );
        let args = parse2::<EmbedArgs>(input).unwrap();

        assert_eq!(
            args.git_url.unwrap().value(),
            "https://github.com/user/repo"
        );
        assert_eq!(args.file_path.value(), "src/lib.rs");
        assert_eq!(args.branch_name.unwrap().value(), "main");
    }

    #[test]
    fn test_named_args_all_fields() {
        let input = quote!(
            git: "https://github.com/user/repo",
            path: "src/lib.rs",
            branch: "main",
            item: my_function
        );
        let args = parse2::<EmbedArgs>(input).unwrap();

        assert!(args.git_url.is_some());
        assert_eq!(args.file_path.value(), "src/lib.rs");
        assert_eq!(args.branch_name.unwrap().value(), "main");
        assert_eq!(args.item_ident.unwrap().to_string(), "my_function");
    }

    #[test]
    fn test_error_missing_path() {
        // Test: Named args without path
        let input = quote!(
            git: "https://github.com/user/repo",
            branch: "main"
        );
        assert_error_contains(parse2::<EmbedArgs>(input), "path parameter is required");
    }

    #[test]
    fn test_error_multiple_git_refs() {
        // Test: Multiple git references (branch and tag)
        let input = quote!(
            git: "https://github.com/user/repo",
            path: "src/lib.rs",
            branch: "main",
            tag: "v1.0"
        );
        assert_error_contains(
            parse2::<EmbedArgs>(input),
            "Only one of branch, commit, or tag can be specified",
        );

        // Test: Multiple git references (branch and commit)
        let input = quote!(
            git: "https://github.com/user/repo",
            path: "src/lib.rs",
            branch: "main",
            commit: "abc123"
        );
        assert_error_contains(
            parse2::<EmbedArgs>(input),
            "Only one of branch, commit, or tag can be specified",
        );
    }

    #[test]
    fn test_error_git_refs_without_git_url() {
        // Test: Branch without git URL
        let input = quote!(
            path: "src/lib.rs",
            branch: "main"
        );
        assert_error_contains(
            parse2::<EmbedArgs>(input),
            "branch, commit, or tag can only be used with git parameter",
        );

        // Test: Tag without git URL
        let input = quote!(
            path: "src/lib.rs",
            tag: "v1.0"
        );
        assert_error_contains(
            parse2::<EmbedArgs>(input),
            "branch, commit, or tag can only be used with git parameter",
        );
    }

    #[test]
    fn test_error_invalid_url_in_path() {
        // Test: URL in file path for positional args
        let input = quote!("https://github.com/user/repo/file.rs");
        assert_error_contains(parse2::<EmbedArgs>(input), "File path cannot be a URL");
    }

    #[test]
    fn test_error_invalid_syntax() {
        // Test: Invalid token in positional args
        let input = quote!("src/lib.rs" something);
        let result = parse2::<EmbedArgs>(input);
        assert!(result.is_err());

        // Test: Invalid named argument
        let input = quote!(
            git: "https://github.com/user/repo",
            invalid: "value",
            path: "src/lib.rs"
        );
        let result = parse2::<EmbedArgs>(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_token_stream_conversion_positional() {
        // Test positional args only
        let input = quote!("src/lib.rs", my_function);
        let args = parse2::<EmbedArgs>(input).unwrap();

        // Convert to token stream
        let mut tokens = TokenStream2::new();
        args.to_tokens(&mut tokens);

        // Convert to string carefully
        let tokens_str = tokens
            .into_iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");

        println!("Positional tokens: {}", tokens_str);
        assert!(tokens_str.contains("src/lib.rs"));
        assert!(tokens_str.contains("my_function"));
    }

    #[test]
    fn test_token_stream_conversion_named() {
        // Test named args only
        let input = quote!(
            path: "src/lib.rs"
        );
        let args = parse2::<EmbedArgs>(input).unwrap();

        // Convert to token stream
        let mut tokens = TokenStream2::new();
        args.to_tokens(&mut tokens);

        // Convert to string carefully
        let tokens_str = tokens
            .into_iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");

        println!("Named tokens: {}", tokens_str);
        assert!(tokens_str.contains("path"));
        assert!(tokens_str.contains("src/lib.rs"));
    }

    #[test]
    fn test_token_stream_conversion_git() {
        // Test git args
        let input = quote!(
            git: "https://github.com/user/repo",
            path: "src/lib.rs",
            branch: "main"
        );
        let args = parse2::<EmbedArgs>(input).unwrap();

        // Convert to token stream
        let mut tokens = TokenStream2::new();
        args.to_tokens(&mut tokens);

        // Convert to string carefully
        let tokens_str = tokens
            .into_iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");

        println!("Git tokens: {}", tokens_str);
        assert!(tokens_str.contains("git"));
        assert!(tokens_str.contains("https://github.com/user/repo"));
        assert!(tokens_str.contains("path"));
        assert!(tokens_str.contains("src/lib.rs"));
        assert!(tokens_str.contains("branch"));
        assert!(tokens_str.contains("main"));
    }

    #[test]
    fn test_whitespace_handling() {
        // Test: Extra whitespace in positional args
        let input = quote!("src/lib.rs", my_function);
        let result = parse2::<EmbedArgs>(input);
        assert!(result.is_ok());

        // Test: Extra whitespace in named args
        let input = quote!(
            git : "https://github.com/user/repo" ,
            path : "src/lib.rs" ,
            branch : "main"
        );
        let result = parse2::<EmbedArgs>(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_empty_string_values() {
        // Test: Empty file path
        let input = quote!("");
        let result = parse2::<EmbedArgs>(input);
        assert!(result.is_ok()); // Empty string is valid, but might fail later validation

        // Test: Empty values in named args
        let input = quote!(
            git: "",
            path: "",
            branch: ""
        );
        let result = parse2::<EmbedArgs>(input);
        assert!(result.is_ok()); // Syntactically valid, but might fail later validation
    }
}
