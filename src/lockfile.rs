use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

// ══════════════════════════════════════════════════════════════
// Lockfile (gorget.lock)
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lockfile {
    #[serde(rename = "package")]
    pub packages: Vec<LockedPackage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockedPackage {
    pub name: String,
    pub source: String,
    pub version: String,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub dependencies: Vec<String>,
}

#[derive(Debug)]
pub enum LockfileError {
    Io { path: PathBuf, error: std::io::Error },
    Parse { path: PathBuf, error: String },
    Serialize { error: String },
}

impl fmt::Display for LockfileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LockfileError::Io { path, error } => {
                write!(f, "cannot read '{}': {}", path.display(), error)
            }
            LockfileError::Parse { path, error } => {
                write!(f, "invalid gorget.lock at '{}': {}", path.display(), error)
            }
            LockfileError::Serialize { error } => {
                write!(f, "failed to serialize lockfile: {error}")
            }
        }
    }
}

impl Lockfile {
    pub fn new() -> Self {
        Self {
            packages: Vec::new(),
        }
    }

    /// Read and parse a `gorget.lock` file.
    pub fn from_path(path: &Path) -> Result<Self, LockfileError> {
        let content = fs::read_to_string(path).map_err(|e| LockfileError::Io {
            path: path.to_path_buf(),
            error: e,
        })?;
        toml::from_str(&content).map_err(|e| LockfileError::Parse {
            path: path.to_path_buf(),
            error: e.to_string(),
        })
    }

    /// Serialize and write to a file, with a header comment.
    pub fn save(&self, path: &Path) -> Result<(), LockfileError> {
        let body =
            toml::to_string_pretty(self).map_err(|e| LockfileError::Serialize {
                error: e.to_string(),
            })?;
        let content = format!("# gorget.lock — auto-generated, do not edit\n\n{body}");
        fs::write(path, content).map_err(|e| LockfileError::Io {
            path: path.to_path_buf(),
            error: e,
        })
    }

    /// Look up a package by name.
    pub fn find(&self, name: &str) -> Option<&LockedPackage> {
        self.packages.iter().find(|p| p.name == name)
    }
}

impl LockedPackage {
    /// Extract the local filesystem path for this locked package.
    /// For `path+<dir>` sources, returns the directory.
    /// For `git+<url>#<commit>` sources, returns the cache directory.
    pub fn source_dir(&self, project_root: &Path, cache_root: &Path) -> Option<PathBuf> {
        if let Some(dir) = self.source.strip_prefix("path+") {
            let p = PathBuf::from(dir);
            if p.is_absolute() {
                Some(p)
            } else {
                Some(project_root.join(p))
            }
        } else if let Some(rest) = self.source.strip_prefix("git+") {
            // git+<url>#<commit>
            if let Some(hash_pos) = rest.rfind('#') {
                let url = &rest[..hash_pos];
                let commit = &rest[hash_pos + 1..];
                let url_hash = sha256_hex(url);
                Some(cache_root.join("git").join(url_hash).join(commit))
            } else {
                None
            }
        } else {
            None
        }
    }
}

/// Simple SHA-256 hash of a string, returning a hex string.
/// Uses a minimal implementation to avoid adding a crypto dependency.
fn sha256_hex(input: &str) -> String {
    // Use std::hash as a lightweight fingerprint for cache directory naming.
    // This is NOT cryptographic, but sufficient for cache path deduplication.
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    input.hash(&mut hasher);
    let h1 = hasher.finish();
    // Hash again for more bits
    h1.hash(&mut hasher);
    let h2 = hasher.finish();
    format!("{h1:016x}{h2:016x}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_lockfile() {
        let toml_str = r#"
[[package]]
name = "utils"
source = "path+../shared/utils"
version = "0.1.0"

[[package]]
name = "http-router"
source = "git+https://github.com/alice/http-router#abc1234"
version = "1.2.0"
dependencies = ["utils"]
"#;
        let lockfile: Lockfile = toml::from_str(toml_str).unwrap();
        assert_eq!(lockfile.packages.len(), 2);
        assert_eq!(lockfile.packages[0].name, "utils");
        assert_eq!(lockfile.packages[1].name, "http-router");
        assert_eq!(lockfile.packages[1].dependencies, vec!["utils"]);
    }

    #[test]
    fn roundtrip_lockfile() {
        let lockfile = Lockfile {
            packages: vec![LockedPackage {
                name: "mylib".to_string(),
                source: "path+../mylib".to_string(),
                version: "0.1.0".to_string(),
                dependencies: vec![],
            }],
        };

        let serialized = toml::to_string_pretty(&lockfile).unwrap();
        let reparsed: Lockfile = toml::from_str(&serialized).unwrap();
        assert_eq!(reparsed.packages.len(), 1);
        assert_eq!(reparsed.packages[0].name, "mylib");
    }

    #[test]
    fn source_dir_path() {
        let pkg = LockedPackage {
            name: "mylib".to_string(),
            source: "path+../mylib".to_string(),
            version: "0.1.0".to_string(),
            dependencies: vec![],
        };
        let root = Path::new("/project");
        let cache = Path::new("/home/user/.gorget/cache");
        let dir = pkg.source_dir(root, cache).unwrap();
        assert_eq!(dir, PathBuf::from("/project/../mylib"));
    }

    #[test]
    fn source_dir_git() {
        let pkg = LockedPackage {
            name: "router".to_string(),
            source: "git+https://github.com/alice/router#abc1234".to_string(),
            version: "1.0.0".to_string(),
            dependencies: vec![],
        };
        let root = Path::new("/project");
        let cache = Path::new("/home/user/.gorget/cache");
        let dir = pkg.source_dir(root, cache).unwrap();
        assert!(dir.starts_with("/home/user/.gorget/cache/git/"));
        assert!(dir.ends_with("abc1234"));
    }
}
