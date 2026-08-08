use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::lockfile::{LockedPackage, Lockfile};
use crate::manifest::{DepSpec, Manifest};

// ══════════════════════════════════════════════════════════════
// Resolver
// ══════════════════════════════════════════════════════════════

#[derive(Debug)]
pub enum ResolveError {
    ManifestNotFound { path: PathBuf },
    ManifestError(crate::manifest::ManifestError),
    LockfileError(crate::lockfile::LockfileError),
    GitError { dep: String, message: String },
    CycleDetected { chain: Vec<String> },
    VersionConflict { dep: String, sources: Vec<String> },
    Io { message: String },
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolveError::ManifestNotFound { path } => {
                write!(f, "manifest not found at '{}'", path.display())
            }
            ResolveError::ManifestError(e) => write!(f, "{e}"),
            ResolveError::LockfileError(e) => write!(f, "{e}"),
            ResolveError::GitError { dep, message } => {
                write!(f, "git error for dependency '{dep}': {message}")
            }
            ResolveError::CycleDetected { chain } => {
                write!(f, "dependency cycle detected: {}", chain.join(" -> "))
            }
            ResolveError::VersionConflict { dep, sources } => {
                write!(
                    f,
                    "version conflict for '{dep}': required from multiple sources:\n  {}",
                    sources.join("\n  ")
                )
            }
            ResolveError::Io { message } => write!(f, "{message}"),
        }
    }
}

impl From<crate::manifest::ManifestError> for ResolveError {
    fn from(e: crate::manifest::ManifestError) -> Self {
        ResolveError::ManifestError(e)
    }
}

impl From<crate::lockfile::LockfileError> for ResolveError {
    fn from(e: crate::lockfile::LockfileError) -> Self {
        ResolveError::LockfileError(e)
    }
}

/// Returns the global cache directory: `~/.gorget/cache/`
pub fn cache_dir() -> PathBuf {
    let home = std::env::var("HOME")
        .or_else(|_| std::env::var("USERPROFILE"))
        .unwrap_or_else(|_| ".".to_string());
    PathBuf::from(home).join(".gorget").join("cache")
}

/// Resolve all dependencies from a manifest, producing a lockfile.
///
/// If a lockfile already exists and is up-to-date (all deps match), returns it.
/// Otherwise, resolves from scratch.
pub fn resolve(
    project_root: &Path,
    manifest: &Manifest,
) -> Result<Lockfile, ResolveError> {
    let lock_path = project_root.join("gorget.lock");

    // Check if existing lockfile is still valid
    if lock_path.exists() {
        if let Ok(existing) = Lockfile::from_path(&lock_path) {
            if is_lockfile_current(&existing, manifest) {
                return Ok(existing);
            }
        }
    }

    // Resolve from scratch
    let cache = cache_dir();
    let mut resolved: BTreeMap<String, LockedPackage> = BTreeMap::new();
    let mut resolving: Vec<String> = Vec::new();

    resolve_deps(
        project_root,
        &manifest.dependencies,
        &cache,
        &mut resolved,
        &mut resolving,
    )?;

    let lockfile = Lockfile {
        packages: resolved.into_values().collect(),
    };

    lockfile.save(&lock_path)?;
    Ok(lockfile)
}

/// Check whether the lockfile covers exactly the dependencies in the manifest.
fn is_lockfile_current(lockfile: &Lockfile, manifest: &Manifest) -> bool {
    if manifest.dependencies.is_empty() && lockfile.packages.is_empty() {
        return true;
    }

    let locked_names: HashSet<&str> = lockfile.packages.iter().map(|p| p.name.as_str()).collect();

    // Every manifest dep must be in the lockfile
    for name in manifest.dependencies.keys() {
        if !locked_names.contains(name.as_str()) {
            return false;
        }
    }

    true
}

/// Recursively resolve dependencies.
fn resolve_deps(
    context_dir: &Path,
    deps: &BTreeMap<String, DepSpec>,
    cache: &Path,
    resolved: &mut BTreeMap<String, LockedPackage>,
    resolving: &mut Vec<String>,
) -> Result<(), ResolveError> {
    for (name, spec) in deps {
        // Skip already resolved
        if resolved.contains_key(name) {
            continue;
        }

        // Cycle detection
        if resolving.contains(name) {
            let mut chain = resolving.clone();
            chain.push(name.clone());
            return Err(ResolveError::CycleDetected { chain });
        }

        resolving.push(name.clone());

        let (source_string, dep_dir, version) = match spec {
            DepSpec::Path { path } => {
                let abs_path = if path.is_absolute() {
                    path.clone()
                } else {
                    context_dir.join(path)
                };
                let canonical = abs_path.canonicalize().map_err(|e| ResolveError::Io {
                    message: format!(
                        "cannot resolve path dependency '{}' ({}): {}",
                        name,
                        abs_path.display(),
                        e
                    ),
                })?;

                // Read the dep's manifest for its version
                let dep_manifest_path = crate::manifest::find_manifest_in(&canonical)
                    .map(|(p, _)| p)
                    .unwrap_or_else(|| crate::manifest::manifest_path_in(&canonical));
                let dep_version = if dep_manifest_path.exists() {
                    let dep_manifest = Manifest::from_path(&dep_manifest_path)?;
                    dep_manifest.package.version
                } else {
                    "0.0.0".to_string()
                };

                // For path deps, store the relative path in the lockfile
                let rel_path = path.display().to_string();
                (format!("path+{rel_path}"), canonical, dep_version)
            }
            DepSpec::Git {
                git,
                tag,
                branch,
                rev,
            } => {
                let commit = resolve_git_ref(name, git, tag.as_deref(), branch.as_deref(), rev.as_deref())?;
                let dep_dir = fetch_git_dep(name, git, &commit, cache)?;

                // Read the dep's manifest for its version
                let dep_manifest = crate::manifest::find_manifest_in(&dep_dir);
                let dep_version = if let Some((dep_manifest_path, _)) = dep_manifest {
                    let dep_manifest = Manifest::from_path(&dep_manifest_path)?;
                    dep_manifest.package.version
                } else {
                    "0.0.0".to_string()
                };

                (format!("git+{git}#{commit}"), dep_dir, dep_version)
            }
        };

        // Check for version conflicts (same package from different sources)
        if let Some(existing) = resolved.get(name) {
            if existing.source != source_string {
                return Err(ResolveError::VersionConflict {
                    dep: name.clone(),
                    sources: vec![existing.source.clone(), source_string],
                });
            }
        }

        // Read transitive dependencies
        let transitive_deps = if let Some((dep_manifest_path, _)) = crate::manifest::find_manifest_in(&dep_dir) {
            let dep_manifest = Manifest::from_path(&dep_manifest_path)?;
            dep_manifest.dependencies
        } else {
            BTreeMap::new()
        };

        let dep_names: Vec<String> = transitive_deps.keys().cloned().collect();

        resolved.insert(
            name.clone(),
            LockedPackage {
                name: name.clone(),
                source: source_string,
                version,
                dependencies: dep_names,
            },
        );

        // Recurse into transitive deps
        if !transitive_deps.is_empty() {
            resolve_deps(&dep_dir, &transitive_deps, cache, resolved, resolving)?;
        }

        resolving.pop();
    }

    Ok(())
}

/// Resolve a git ref (tag/branch/rev) to a commit hash using `git ls-remote`.
fn resolve_git_ref(
    dep_name: &str,
    url: &str,
    tag: Option<&str>,
    branch: Option<&str>,
    rev: Option<&str>,
) -> Result<String, ResolveError> {
    // If an exact rev is specified, use it directly
    if let Some(rev) = rev {
        return Ok(rev.to_string());
    }

    let refspec = if let Some(tag) = tag {
        format!("refs/tags/{tag}")
    } else if let Some(branch) = branch {
        format!("refs/heads/{branch}")
    } else {
        "HEAD".to_string()
    };

    let output = Command::new("git")
        .args(["ls-remote", url, &refspec])
        .output()
        .map_err(|e| ResolveError::GitError {
            dep: dep_name.to_string(),
            message: format!("failed to run git ls-remote: {e}"),
        })?;

    if !output.status.success() {
        return Err(ResolveError::GitError {
            dep: dep_name.to_string(),
            message: format!(
                "git ls-remote failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ),
        });
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let commit = stdout
        .lines()
        .next()
        .and_then(|line| line.split_whitespace().next())
        .ok_or_else(|| ResolveError::GitError {
            dep: dep_name.to_string(),
            message: format!("no matching ref '{refspec}' found at {url}"),
        })?;

    Ok(commit.to_string())
}

/// Fetch a git dependency into the cache if not already present.
/// Returns the path to the cached checkout.
fn fetch_git_dep(
    dep_name: &str,
    url: &str,
    commit: &str,
    cache: &Path,
) -> Result<PathBuf, ResolveError> {
    let url_hash = {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        url.hash(&mut hasher);
        let h1 = hasher.finish();
        h1.hash(&mut hasher);
        let h2 = hasher.finish();
        format!("{h1:016x}{h2:016x}")
    };

    let dep_dir = cache.join("git").join(&url_hash).join(commit);

    // Already cached?
    if dep_dir.exists() && crate::manifest::find_manifest_in(&dep_dir).is_some() {
        return Ok(dep_dir);
    }

    // Create parent directories
    fs::create_dir_all(&dep_dir).map_err(|e| ResolveError::Io {
        message: format!("cannot create cache directory '{}': {e}", dep_dir.display()),
    })?;

    // Clone with depth=1 at the specific commit
    // First try: clone the repo, then checkout the commit
    let clone_output = Command::new("git")
        .args(["clone", "--depth=1", url, &dep_dir.display().to_string()])
        .output()
        .map_err(|e| ResolveError::GitError {
            dep: dep_name.to_string(),
            message: format!("failed to run git clone: {e}"),
        })?;

    if !clone_output.status.success() {
        // Clean up failed clone
        let _ = fs::remove_dir_all(&dep_dir);
        return Err(ResolveError::GitError {
            dep: dep_name.to_string(),
            message: format!(
                "git clone failed: {}",
                String::from_utf8_lossy(&clone_output.stderr)
            ),
        });
    }

    // Fetch the specific commit and checkout
    let fetch_output = Command::new("git")
        .args(["fetch", "origin", commit])
        .current_dir(&dep_dir)
        .output()
        .map_err(|e| ResolveError::GitError {
            dep: dep_name.to_string(),
            message: format!("failed to run git fetch: {e}"),
        })?;

    if fetch_output.status.success() {
        let checkout_output = Command::new("git")
            .args(["checkout", commit])
            .current_dir(&dep_dir)
            .output()
            .map_err(|e| ResolveError::GitError {
                dep: dep_name.to_string(),
                message: format!("failed to run git checkout: {e}"),
            })?;

        if !checkout_output.status.success() {
            eprintln!(
                "Warning: could not checkout commit {commit} for {dep_name}, using default branch"
            );
        }
    }

    Ok(dep_dir)
}

/// Build the dep_paths map from a lockfile, mapping package names to source directories.
pub fn build_dep_paths(
    lockfile: &Lockfile,
    project_root: &Path,
) -> HashMap<String, PathBuf> {
    let cache = cache_dir();
    let mut dep_paths = HashMap::new();

    for pkg in &lockfile.packages {
        if let Some(dir) = pkg.source_dir(project_root, &cache) {
            dep_paths.insert(pkg.name.clone(), dir);
        }
    }

    dep_paths
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_lockfile_current_empty() {
        let lockfile = Lockfile::new();
        let manifest = Manifest::new("test");
        assert!(is_lockfile_current(&lockfile, &manifest));
    }

    #[test]
    fn is_lockfile_current_missing_dep() {
        let lockfile = Lockfile::new();
        let mut manifest = Manifest::new("test");
        manifest.dependencies.insert(
            "foo".to_string(),
            DepSpec::Path {
                path: PathBuf::from("../foo"),
            },
        );
        assert!(!is_lockfile_current(&lockfile, &manifest));
    }
}
