use git2::{Direction, FetchOptions, RemoteCallbacks, Repository};
use proc_macro2::Span;
use sha2::{Digest, Sha256};
use std::fs;
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::time::Duration;
use syn::visit::Visit;
use syn::Error;
use syn::Result;

use crate::{source_excerpt, ItemVisitor};

pub fn manage_snippet(crate_root: &Path, file_path: &str, item_ident: &str) -> Result<String> {
    let snippets_dir = crate_root.join(".snippets");
    println!("Snippets directory: {}", snippets_dir.display());

    // Ensure snippets directory exists
    fs::create_dir_all(&snippets_dir).map_err(|e| {
        Error::new(
            Span::call_site(),
            format!("Failed to create .snippets directory: {}", e),
        )
    })?;

    let snippet_path = generate_snippet_path(&snippets_dir, file_path, item_ident);
    println!("Snippet path: {}", snippet_path.display());

    let has_internet = check_internet_connectivity();
    println!(
        "Internet connectivity: {}",
        if has_internet {
            "Available"
        } else {
            "Not available"
        }
    );

    if !has_internet {
        println!("No internet connection, attempting to read from cached snippet");
        // Try to read from the snippet file
        match fs::read_to_string(&snippet_path) {
            Ok(content) => {
                println!("Successfully read content from cached snippet");
                Ok(content)
            }
            Err(e) => Err(Error::new(
                Span::call_site(),
                format!(
                    "No internet connection and failed to read cached snippet at {}: {}.
                        Please ensure you have internet connectivity for the first run.",
                    snippet_path.display(),
                    e
                ),
            )),
        }
    } else {
        // Internet is available, proceed with full path checking and content updating
        let full_path = crate_root.join(file_path);
        println!(
            "Internet available, checking file at: {}",
            full_path.display()
        );

        let existing_content = fs::read_to_string(&snippet_path).ok();
        let new_content = extract_item_from_file(&full_path, item_ident)?;

        if existing_content.as_ref().map(|c| hash_content(c)) != Some(hash_content(&new_content)) {
            println!("Updating snippet file with new content");
            fs::write(&snippet_path, &new_content).map_err(|e| {
                Error::new(
                    Span::call_site(),
                    format!("Failed to write snippet file: {}", e),
                )
            })?;
        } else {
            println!("Snippet is up to date");
        }

        Ok(new_content)
    }
}

pub fn generate_snippet_path(snippets_dir: &Path, file_path: &str, item_ident: &str) -> PathBuf {
    println!(
        "inside generate_snippet_path ----> Snippets directory: {}",
        snippets_dir.display()
    );
    println!(
        "inside generate_snippet_path ----> File path: {}",
        file_path
    );
    println!(
        "inside generate_snippet_path ----> Item ident: {}",
        item_ident
    );
    let path = PathBuf::from(file_path);
    let file_name = path
        .file_name()
        .and_then(|f| f.to_str())
        .unwrap_or("unknown");
    snippets_dir.join(format!(".docify-snippet-{}-{}", file_name, item_ident))
}
pub fn extract_item_from_file(file_path: &Path, item_ident: &str) -> Result<String> {
    println!(
        "inside extract_item_from_file ----> Extracting item '{}' from '{}'",
        item_ident,
        file_path.display()
    );

    let source_code = fs::read_to_string(file_path).map_err(|e| {
        Error::new(
            Span::call_site(),
            format!(
                "Could not read the specified path '{}': {}",
                file_path.display(),
                e
            ),
        )
    })?;

    println!(
        "inside extract_item_from_file ----> Source code: {}",
        source_code
    );

    let mut visitor = ItemVisitor {
        search: syn::parse_str(item_ident)?,
        results: Vec::new(),
    };
    visitor.visit_file(&syn::parse_file(&source_code)?);
    println!(
        "inside extract_item_from_file ----> Visitor results: {:?}",
        visitor.results
    );
    if visitor.results.is_empty() {
        return Err(Error::new(
            Span::call_site(),
            format!(
                "Could not find docify export item '{}' in '{}'.",
                item_ident,
                file_path.display()
            ),
        ));
    }

    println!("Successfully extracted item from file");
    let (item, style) = visitor.results.first().unwrap();
    source_excerpt(&source_code, item, *style)
}

pub fn hash_content(content: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content);
    let result = format!("{:x}", hasher.finalize());
    println!("Content hash: {}", result);
    result
}

/// Checks if there is an active internet connection by attempting to connect to multiple reliable hosts
pub fn check_internet_connectivity() -> bool {
    // List of reliable hosts and ports to try
    let hosts = [
        ("8.8.8.8", 53),        // Google DNS
        ("1.1.1.1", 53),        // Cloudflare DNS
        ("208.67.222.222", 53), // OpenDNS
    ];

    // Set a timeout for connection attempts
    let timeout = Duration::from_secs(1);

    for &(host, port) in hosts.iter() {
        if let Ok(stream) = TcpStream::connect((host, port)) {
            // Set the timeout for read/write operations
            if stream.set_read_timeout(Some(timeout)).is_ok()
                && stream.set_write_timeout(Some(timeout)).is_ok()
            {
                return true;
            }
        }
    }

    false
}
// comment
/// Helper function to convert git2::Error to syn::Error
fn git_err_to_syn(err: git2::Error) -> syn::Error {
    syn::Error::new(Span::call_site(), format!("Git error: {}", err))
}

/// Gets commit SHA without cloning entire repo
pub fn get_remote_commit_sha_without_clone(
    git_url: &str,
    branch: Option<&str>,
    tag: Option<&str>,
) -> Result<String> {
    let temp_dir = tempfile::Builder::new()
        .prefix("docify-temp-")
        .rand_bytes(5)
        .tempdir()
        .map_err(|e| {
            Error::new(
                Span::call_site(),
                format!("Failed to create temp dir: {}", e),
            )
        })?;
    println!("Created temp dir: {}", temp_dir.path().display());

    let repo = Repository::init(temp_dir.path()).map_err(git_err_to_syn)?;
    let mut remote = repo.remote_anonymous(git_url).map_err(git_err_to_syn)?;

    println!("Initialized repo");

    // First, fetch the remote HEAD to determine default branch
    println!("ℹ️  Fetching remote references...");
    remote.connect(Direction::Fetch).map_err(git_err_to_syn)?;

    // Handle default branch resolution with proper error conversion
    let default_branch = remote
        .default_branch()
        .map_err(git_err_to_syn)?
        .as_str()
        .map(String::from)
        .ok_or_else(|| Error::new(Span::call_site(), "Invalid default branch name"))?;

    print!("defult branch --------> {}", default_branch);
    remote.disconnect().map_err(git_err_to_syn)?;

    // Convert refs/heads/main to just main
    let default_branch = default_branch
        .strip_prefix("refs/heads/")
        .unwrap_or(&default_branch);

    println!("ℹ️  Default branch: {}", default_branch);

    // Determine which refs to fetch
    let refspecs = if let Some(tag_name) = tag {
        vec![format!("refs/tags/{}:refs/tags/{}", tag_name, tag_name)]
    } else {
        let branch_name = branch.unwrap_or(default_branch);
        vec![format!(
            "refs/heads/{}:refs/heads/{}",
            branch_name, branch_name
        )]
    };

    println!("Fetching refs: {:?}", refspecs);
    remote
        .fetch(
            refspecs
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
                .as_slice(),
            None,
            None,
        )
        .map_err(git_err_to_syn)?;

    // Determine which commit to use
    let commit_id = if let Some(tag_name) = tag {
        let tag_ref = repo
            .find_reference(&format!("refs/tags/{}", tag_name))
            .map_err(git_err_to_syn)?;
        tag_ref.peel_to_commit().map_err(git_err_to_syn)?.id()
    } else {
        let branch_name = branch.unwrap_or(default_branch);
        let reference = repo
            .find_reference(&format!("refs/heads/{}", branch_name))
            .map_err(git_err_to_syn)?;
        reference.peel_to_commit().map_err(git_err_to_syn)?.id()
    };

    Ok(commit_id.to_string())
}

pub fn get_or_create_commit_dir(git_url: &str, commit_sha: &str) -> Result<PathBuf> {
    let temp_base = std::env::temp_dir().join("docify-repos");

    // Extract repo name from git URL
    let repo_name = git_url
        .split('/')
        .last()
        .map(|s| {
            if s.ends_with(".git") {
                s.strip_suffix(".git").unwrap_or(s)
            } else {
                s
            }
        })
        .unwrap_or("repo");
    println!(
        "Repo name inside get_or_create_commit_dir: ------>  {}",
        repo_name
    );

    // Use first 8 chars of commit hash
    let short_commit = &commit_sha[..8];

    // Create directory name: docify-{short_commit}-{repo_name}
    let dir_name = format!("docify-{}-{}", short_commit, repo_name);
    let commit_dir = temp_base.join(dir_name);

    if commit_dir.exists() {
        println!("Found existing repo directory: {}", commit_dir.display());
        Ok(commit_dir)
    } else {
        println!("Creating new repo directory: {}", commit_dir.display());
        fs::create_dir_all(&commit_dir).map_err(|e| {
            Error::new(
                Span::call_site(),
                format!("Failed to create commit directory: {}", e),
            )
        })?;
        Ok(commit_dir)
    }
}

/// Clones repo and checks out specific commit, reusing existing clone if available
pub fn clone_and_checkout_repo(git_url: &str, commit_sha: &str) -> Result<PathBuf> {
    let commit_dir = get_or_create_commit_dir(git_url, commit_sha)?;

    // Check if repo is already cloned and checked out
    if commit_dir.join(".git").exists() {
        println!(
            "Using existing repo clone with commit hash -->: {} returning existing dir path ----> {}",
            commit_sha,
            commit_dir.display()
        );
        return Ok(commit_dir);
    }

    println!("Cloning new repo for commit: {}", commit_sha);

    let mut callbacks = RemoteCallbacks::new();
    callbacks.transfer_progress(|p| {
        println!(
            "Fetching: {}/{} objects",
            p.received_objects(),
            p.total_objects()
        );
        true
    });

    let mut fetch_opts = FetchOptions::new();
    fetch_opts.remote_callbacks(callbacks);
    fetch_opts.depth(1);

    let repo = Repository::init(&commit_dir).map_err(git_err_to_syn)?;
    let mut remote = repo.remote_anonymous(git_url).map_err(git_err_to_syn)?;

    remote
        .fetch(
            &[&format!("+{commit_sha}:refs/heads/temp")],
            Some(&mut fetch_opts),
            None,
        )
        .map_err(git_err_to_syn)?;

    let commit_id = git2::Oid::from_str(commit_sha).map_err(git_err_to_syn)?;
    let commit = repo.find_commit(commit_id).map_err(git_err_to_syn)?;
    let tree = commit.tree().map_err(git_err_to_syn)?;

    repo.checkout_tree(tree.as_object(), None)
        .map_err(git_err_to_syn)?;
    repo.set_head_detached(commit_id).map_err(git_err_to_syn)?;

    Ok(commit_dir)
}

/// Generates a deterministic filename for the snippet
pub fn generate_snippet_filename(commit_sha: &str, path: &str) -> String {
    let path_buf = PathBuf::from(path);
    let file_name = path_buf
        .file_name()
        .and_then(|f| f.to_str())
        .unwrap_or("unknown");

    format!("{}-{}-{}", &commit_sha[..8], hash_path(path), file_name)
}

fn hash_path(path: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(path.as_bytes());
    format!("{:.8x}", hasher.finalize()) // First 8 chars of hash
}
