use git2::{Direction, FetchOptions, RemoteCallbacks, Repository};
use proc_macro2::Span;
use sha2::{Digest, Sha256};
use std::fs;
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::time::Duration;
use syn::visit::Visit;
use syn::Result;
use syn::{Error, LitStr};

use crate::{caller_crate_root, source_excerpt, ItemVisitor};

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

/// Helper function to convert io::Error to syn::Error
fn io_err_to_syn(err: std::io::Error) -> syn::Error {
    syn::Error::new(Span::call_site(), format!("IO error: {}", err))
}

pub fn get_remote_commit_sha_without_clone(
    git_url: &str,
    branch: Option<&str>,
    tag: Option<&str>,
) -> Result<String> {
    let temp_dir = tempfile::Builder::new()
        .prefix("docify-temp-")
        .rand_bytes(5)
        .tempdir()
        .map_err(io_err_to_syn)?;

    let repo = Repository::init(temp_dir.path()).map_err(git_err_to_syn)?;
    let mut remote = repo.remote_anonymous(git_url).map_err(git_err_to_syn)?;

    // Set up fetch options
    let mut fetch_opts = FetchOptions::new();
    fetch_opts.depth(1); // Only fetch the most recent commit

    println!("fetching ref");
    // let mut callbacks = RemoteCallbacks::new();
    // callbacks.transfer_progress(|p| {
    //     println!(
    //         "Fetching: {}/{} objects",
    //         p.received_objects(),
    //         p.total_objects()
    //     );
    //     true
    // });

    // fetch_opts.remote_callbacks(callbacks);

    // First connect to get default branch if needed
    remote.connect(Direction::Fetch).map_err(git_err_to_syn)?;

    // Determine which ref to fetch
    let refspec = if let Some(tag_name) = tag {
        format!("refs/tags/{}:refs/tags/{}", tag_name, tag_name)
    } else {
        let branch_ref = if let Some(b) = branch {
            format!("refs/heads/{}", b)
        } else {
            // Get default branch name
            let default_branch = remote
                .default_branch()
                .map_err(git_err_to_syn)?
                .as_str()
                .map(String::from)
                .ok_or_else(|| Error::new(Span::call_site(), "Invalid default branch name"))?;

            // Convert refs/heads/main to just refs/heads/main
            if !default_branch.starts_with("refs/heads/") {
                format!("refs/heads/{}", default_branch)
            } else {
                default_branch
            }
        };
        format!("{}:{}", branch_ref, branch_ref)
    };

    println!("Fetching ref: {}", refspec);

    // Disconnect before fetch to ensure clean state
    remote.disconnect().map_err(git_err_to_syn)?;

    // Fetch the specific ref
    remote
        .fetch(&[&refspec], Some(&mut fetch_opts), None)
        .map_err(git_err_to_syn)?;

    // Get commit ID
    let reference = if let Some(tag_name) = tag {
        repo.find_reference(&format!("refs/tags/{}", tag_name))
            .map_err(git_err_to_syn)?
    } else {
        let ref_name = if let Some(b) = branch {
            format!("refs/heads/{}", b)
        } else {
            // Use the actual fetched ref name
            let default_branch = remote
                .default_branch()
                .map_err(git_err_to_syn)?
                .as_str()
                .map(String::from)
                .ok_or_else(|| Error::new(Span::call_site(), "Invalid default branch name"))?;

            if !default_branch.starts_with("refs/heads/") {
                format!("refs/heads/{}", default_branch)
            } else {
                default_branch
            }
        };
        repo.find_reference(&ref_name).map_err(git_err_to_syn)?
    };

    let commit_id = reference.peel_to_commit().map_err(git_err_to_syn)?.id();

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

/// Represents a parsed snippet filename
pub struct SnippetFile {
    pub prefix: String,
    pub commit_hash: Option<String>,
    pub full_name: String,
}

/// Functions to handle snippet file operations
impl SnippetFile {
    pub fn new_without_commit(
        git_url: &str,
        git_option_type: &str,
        git_option_value: &str,
        path: &str,
        item_ident: &str,
    ) -> Self {
        println!("\n📝 Creating new SnippetFile...");
        println!("ℹ️  Input path: {}", path);
        println!("ℹ️  Item identifier: {}", item_ident);

        let prefix = format!(
            "{}-{}-{}-{}",
            hash_git_url(git_url),
            hash_git_option(git_option_type, git_option_value),
            hash_string(path),
            item_ident,
        );
        println!("ℹ️  Generated prefix: {}", prefix);

        Self {
            prefix: prefix.clone(),
            commit_hash: None,
            full_name: format!("{}.rs", prefix),
        }
    }

    pub fn new_with_commit(
        git_url: &str,
        git_option_type: &str,
        git_option_value: &str,
        path: &str,
        commit_sha: &str,
        item_ident: &str,
    ) -> Self {
        println!("\n📝 Creating new SnippetFile...");
        println!("ℹ️  Input path: {}", path);
        println!("ℹ️  Item identifier: {}", item_ident);

        let prefix = format!(
            "{}-{}-{}-{}",
            hash_git_url(git_url),
            hash_git_option(git_option_type, git_option_value),
            hash_string(path),
            item_ident,
        );
        println!("ℹ️  Generated prefix: {}", prefix);

        let full_name = format!("{}-{}.rs", prefix, commit_sha);
        println!("✅ Created snippet filename: {}", full_name);

        Self {
            prefix,
            commit_hash: Some(commit_sha.to_string()),
            full_name,
        }
    }

    pub fn find_existing(prefix: &str) -> Option<Self> {
        println!("\n🔍 Looking for existing snippet with prefix: {}", prefix);

        // Get the crate root path
        let crate_root = match crate::caller_crate_root() {
            Some(root) => root,
            None => {
                println!("❌ Failed to resolve crate root");
                return None;
            }
        };

        // Use absolute path by joining with crate root
        let snippets_dir = crate_root.join(".snippets");
        println!("📁 Checking snippets directory: {}", snippets_dir.display());

        // Check if directory exists and is actually a directory
        if !snippets_dir.exists() {
            println!(
                "❌ .snippets directory does not exist at {}",
                snippets_dir.display()
            );
            return None;
        }

        fs::read_dir(snippets_dir).ok()?.find_map(|entry| {
            let entry = entry.ok()?;
            println!("entry: {:?}", entry);
            let file_name = entry.file_name().to_string_lossy().to_string();

            println!("ℹ️  Checking file: {}", file_name);

            if file_name.starts_with(prefix) {
                println!("✅ Found matching file!");
                // Extract commit hash from filename if it exists
                let commit_hash = file_name
                    .strip_suffix(".rs")?
                    .rsplit('-')
                    .next()
                    .map(|s| s.to_string());
                println!(
                    "ℹ️  Extracted commit hash from existing file: {:?}",
                    commit_hash
                );

                Some(Self {
                    prefix: prefix.to_string(),
                    commit_hash,
                    full_name: file_name,
                })
            } else {
                None
            }
        })
    }

    /// Creates a new SnippetFile for default branch case (when no branch is specified)
    pub fn new_for_default_branch(
        git_url: &str,
        path: &str,
        item_ident: &str,
        commit_sha: Option<&str>,
    ) -> Self {
        println!("\n📝 Creating new SnippetFile for default branch case");
        println!("ℹ️  Input path: {}", path);
        println!("ℹ️  Item identifier: {}", item_ident);

        let prefix = format!(
            "{}-{}-{}",
            hash_git_url(git_url),
            hash_string(path),
            item_ident,
        );
        println!("ℹ️  Generated prefix: {}", prefix);

        if let Some(commit) = commit_sha {
            let full_name = format!("{}-{}.rs", prefix, commit);
            println!("✅ Created snippet filename with commit: {}", full_name);
            Self {
                prefix,
                commit_hash: Some(commit.to_string()),
                full_name,
            }
        } else {
            println!("✅ Created snippet filename without commit");
            Self {
                prefix: prefix.clone(),
                commit_hash: None,
                full_name: format!("{}.rs", prefix),
            }
        }
    }
}

fn hash_string(input: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(input.as_bytes());
    format!("{:.8x}", hasher.finalize())
}

fn hash_git_option(option_type: &str, value: &str) -> String {
    hash_string(&format!("{}-{}", option_type, value))
}

/// Normalizes a git URL by removing trailing .git if present
fn normalize_git_url(url: &str) -> &str {
    url.strip_suffix(".git").unwrap_or(url)
}

fn hash_git_url(url: &str) -> String {
    println!("ℹ️  Hashing git URL: {}", url);
    let normalized_url = normalize_git_url(url);
    hash_string(normalized_url)
}

/// Creates and returns the snippets directory path, ensuring it exists
pub fn get_or_create_snippets_dir() -> Result<PathBuf> {
    let crate_root = caller_crate_root()
        .ok_or_else(|| Error::new(Span::call_site(), "Failed to resolve caller crate root"))?;

    let snippets_dir = crate_root.join(".snippets");
    println!(
        "📁 Ensuring snippets directory exists at: {}",
        snippets_dir.display()
    );

    fs::create_dir_all(&snippets_dir).map_err(|e| {
        Error::new(
            Span::call_site(),
            format!("Failed to create .snippets directory: {}", e),
        )
    })?;

    Ok(snippets_dir)
}

/// Determines the git option type and value based on the provided arguments
pub fn get_git_options(
    commit_hash: &Option<LitStr>,
    tag_name: &Option<LitStr>,
    branch_name: &Option<LitStr>,
) -> (Option<String>, Option<String>) {
    if let Some(hash) = commit_hash {
        println!("Using commit hash: {}", hash.value());
        (Some("commit".to_string()), Some(hash.value()))
    } else if let Some(tag) = tag_name {
        println!("Using tag: {}", tag.value());
        (Some("tag".to_string()), Some(tag.value()))
    } else if let Some(branch) = branch_name {
        println!("Using provided branch: {}", branch.value());
        (Some("branch".to_string()), Some(branch.value()))
    } else {
        // No specific git option provided - default branch case
        println!("No specific git option provided, using flexible naming");
        (None, None)
    }
}

/// Creates a new snippet file based on git options and internet connectivity
pub fn get_snippet_file_name(
    git_url: &str,
    args: &crate::EmbedArgs,
    allow_updates: bool,
) -> Result<SnippetFile> {
    if let Some(hash) = &args.commit_hash {
        // If commit hash is provided, use it regardless of internet connectivity
        println!("Using provided commit hash: {}", hash.value());
        return Ok(SnippetFile::new_with_commit(
            git_url,
            "commit",
            &hash.value(),
            &args.file_path.value(),
            &hash.value(),
            &args.item_ident.as_ref().unwrap().to_string(),
        ));
    }

    if let Some(tag) = &args.tag_name {
        return if allow_updates {
            let commit_sha =
                get_remote_commit_sha_without_clone(git_url, None, Some(tag.value().as_str()))?;
            println!("commit hash for tag: {} -> {}", tag.value(), commit_sha);
            Ok(SnippetFile::new_with_commit(
                git_url,
                "tag",
                &tag.value(),
                &args.file_path.value(),
                &commit_sha,
                &args.item_ident.as_ref().unwrap().to_string(),
            ))
        } else {
            println!("📡 Offline mode: Creating snippet without commit hash for tag");
            Ok(SnippetFile::new_without_commit(
                git_url,
                "tag",
                &tag.value(),
                &args.file_path.value(),
                &args.item_ident.as_ref().unwrap().to_string(),
            ))
        };
    }

    if let Some(branch) = &args.branch_name {
        return if allow_updates {
            let commit_sha =
                get_remote_commit_sha_without_clone(git_url, Some(branch.value().as_str()), None)?;
            println!(
                "commit hash for branch: {} -> {}",
                branch.value(),
                commit_sha
            );
            Ok(SnippetFile::new_with_commit(
                git_url,
                "branch",
                branch.value().as_str(),
                &args.file_path.value(),
                &commit_sha,
                &args.item_ident.as_ref().unwrap().to_string(),
            ))
        } else {
            println!("📡 Offline mode: Creating snippet without commit hash for branch");
            Ok(SnippetFile::new_without_commit(
                git_url,
                "branch",
                branch.value().as_str(),
                &args.file_path.value(),
                &args.item_ident.as_ref().unwrap().to_string(),
            ))
        };
    }

    // Default branch case - more flexible naming
    if allow_updates {
        let commit_sha = get_remote_commit_sha_without_clone(git_url, None, None)?;
        println!(
            "branch not provided getting latest commit hash for the default branch -> {}",
            commit_sha
        );
        Ok(SnippetFile::new_for_default_branch(
            git_url,
            &args.file_path.value(),
            &args.item_ident.as_ref().unwrap().to_string(),
            Some(&commit_sha),
        ))
    } else {
        Ok(SnippetFile::new_for_default_branch(
            git_url,
            &args.file_path.value(),
            &args.item_ident.as_ref().unwrap().to_string(),
            None,
        ))
    }
}

/// Checks if a snippet file already exists and handles updating if necessary
/// Returns Some(filename) if a valid snippet exists, None if we need to create a new one
pub fn check_existing_snippet(
    new_snippet: &SnippetFile,
    allow_updates: bool,
    snippets_dir: &Path,
) -> Result<Option<String>> {
    let Some(existing_snippet) = SnippetFile::find_existing(&new_snippet.prefix) else {
        if !allow_updates {
            return Err(Error::new(
                Span::call_site(),
                "No matching snippet found and no internet connection available",
            ));
        }
        return Ok(None);
    };

    if !allow_updates {
        println!(
            "✅ Found existing snippet (offline mode): .snippets/{}",
            existing_snippet.full_name
        );
        return Ok(Some(existing_snippet.full_name));
    }

    // Online mode comparison
    if let (Some(existing_hash), Some(new_hash)) =
        (&existing_snippet.commit_hash, &new_snippet.commit_hash)
    {
        if existing_hash == new_hash {
            println!(
                "✅ Existing snippet is up to date at: .snippets/{}",
                existing_snippet.full_name
            );
            return Ok(Some(existing_snippet.full_name));
        }

        println!("ℹ️  Found existing snippet with different commit hash:");
        println!("   Current: {}", existing_hash);
        println!("   New: {}", new_hash);
        println!("🔄 Removing outdated snippet...");

        // Remove old snippet file
        fs::remove_file(snippets_dir.join(&existing_snippet.full_name)).map_err(|e| {
            Error::new(
                Span::call_site(),
                format!("Failed to remove old snippet file: {}", e),
            )
        })?;
    }

    Ok(None)
}
