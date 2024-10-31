use git2::{FetchOptions, Oid, RemoteCallbacks, Repository};
use once_cell::sync::Lazy;
use proc_macro2::Span;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::fs;
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::Duration;
use syn::visit::Visit;
use syn::Error;
use syn::Result;
use tempfile::{Builder, TempDir};

use crate::{source_excerpt, ItemVisitor};

// Cache for storing repository clones
static REPO_CACHE: Lazy<Mutex<HashMap<String, TempDir>>> = Lazy::new(|| Mutex::new(HashMap::new()));

// Generate a unique cache key for a repository
pub fn generate_cache_key(
    git_url: &str,
    branch: Option<&str>,
    commit: Option<&str>,
    tag: Option<&str>,
) -> String {
    format!(
        "{}:{}:{}:{}",
        git_url,
        branch.unwrap_or("master"),
        commit.unwrap_or("none"),
        tag.unwrap_or("none")
    )
}
pub fn get_or_clone_repo(
    git_url: &str,
    project_root: &PathBuf,
    branch_name: Option<String>,
    commit_hash: Option<String>,
    tag_name: Option<String>,
) -> Result<PathBuf> {
    let cache_key = generate_cache_key(
        git_url,
        branch_name.as_deref(),
        commit_hash.as_deref(),
        tag_name.as_deref(),
    );

    let mut cache = REPO_CACHE.lock().unwrap();

    if !cache.contains_key(&cache_key) {
        println!("Cache miss for {}, cloning repository...", git_url);
        let temp_dir = clone_repo(git_url, project_root, branch_name, commit_hash, tag_name)?;
        cache.insert(cache_key.clone(), temp_dir);
    } else {
        println!("Cache hit for {}, using existing clone", git_url);
    }

    Ok(cache.get(&cache_key).unwrap().path().to_path_buf())
}
pub fn clone_repo(
    git_url: &str,
    _project_root: &PathBuf,
    branch_name: Option<String>,
    commit_hash: Option<String>,
    tag_name: Option<String>,
) -> Result<TempDir> {
    let temp_dir = Builder::new()
        .prefix("docify-")
        .rand_bytes(5)
        .tempdir()
        .map_err(|e| {
            Error::new(
                Span::call_site(),
                format!("Failed to create temp directory: {}", e),
            )
        })?;

    println!(
        "Temporary directory created at: {}",
        temp_dir.path().display()
    );

    let mut callbacks = RemoteCallbacks::new();
    callbacks.transfer_progress(|progress| {
        println!(
            "Transfer progress: {}/{} objects",
            progress.received_objects(),
            progress.total_objects()
        );
        true
    });

    let repo = Repository::clone(git_url, temp_dir.path()).map_err(|e| {
        Error::new(
            Span::call_site(),
            format!("Failed to clone repository: {}", e),
        )
    })?;

    println!("Repository cloned successfully");
    println!("Repository cloned to: {}", temp_dir.path().display());

    if branch_name.is_some() || commit_hash.is_some() || tag_name.is_some() {
        let mut fetch_opts = FetchOptions::new();
        fetch_opts.remote_callbacks(callbacks);

        let mut remote = repo.find_remote("origin").map_err(|e| {
            Error::new(
                Span::call_site(),
                format!("Failed to find remote 'origin': {}", e),
            )
        })?;

        remote
            .fetch(
                &["refs/heads/*:refs/heads/*", "refs/tags/*:refs/tags/*"],
                Some(&mut fetch_opts),
                None,
            )
            .map_err(|e| {
                Error::new(
                    Span::call_site(),
                    format!("Failed to fetch all branches and tags: {}", e),
                )
            })?;

        if let Some(tag) = tag_name.as_deref() {
            let (object, reference) =
                repo.revparse_ext(&format!("refs/tags/{}", tag))
                    .map_err(|e| {
                        Error::new(
                            Span::call_site(),
                            format!("Failed to find tag '{}': {}", tag, e),
                        )
                    })?;

            repo.checkout_tree(&object, None).map_err(|e| {
                Error::new(
                    Span::call_site(),
                    format!("Failed to checkout tag '{}': {}", tag, e),
                )
            })?;

            repo.set_head_detached(object.id()).map_err(|e| {
                Error::new(
                    Span::call_site(),
                    format!("Failed to set HEAD to tag '{}': {}", tag, e),
                )
            })?;

            println!("Checked out tag '{}'", tag);
        } else if let Some(commit) = commit_hash.as_deref() {
            let oid = Oid::from_str(commit).map_err(|e| {
                Error::new(
                    Span::call_site(),
                    format!("Invalid commit hash '{}': {}", commit, e),
                )
            })?;

            let commit_obj = repo.find_commit(oid).map_err(|e| {
                Error::new(
                    Span::call_site(),
                    format!("Failed to find commit '{}': {}", commit, e),
                )
            })?;

            repo.set_head_detached(commit_obj.id()).map_err(|e| {
                Error::new(
                    Span::call_site(),
                    format!("Failed to set HEAD to commit '{}': {}", commit, e),
                )
            })?;

            repo.checkout_head(Some(git2::build::CheckoutBuilder::new().force()))
                .map_err(|e| {
                    Error::new(
                        Span::call_site(),
                        format!("Failed to checkout commit '{}': {}", commit, e),
                    )
                })?;

            println!("Checked out commit '{}'", commit);
        } else if let Some(branch) = branch_name.as_deref() {
            let (object, reference) =
                repo.revparse_ext(&format!("origin/{}", branch))
                    .map_err(|e| {
                        Error::new(
                            Span::call_site(),
                            format!("Failed to find '{}' branch: {}", branch, e),
                        )
                    })?;

            repo.checkout_tree(&object, None).map_err(|e| {
                Error::new(
                    Span::call_site(),
                    format!("Failed to checkout '{}' branch: {}", branch, e),
                )
            })?;

            repo.set_head(&format!("refs/heads/{}", branch))
                .map_err(|e| {
                    Error::new(
                        Span::call_site(),
                        format!("Failed to set HEAD to '{}' branch: {}", branch, e),
                    )
                })?;

            println!("Switched to '{}' branch", branch);
        }
    } else {
        println!("Cloned default branch only");
    }

    Ok(temp_dir)
}

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
fn extract_item_from_file(file_path: &Path, item_ident: &str) -> Result<String> {
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
