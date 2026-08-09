use std::collections::BTreeMap;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

// ══════════════════════════════════════════════════════════════
// Manifest (manifest.toml; legacy `gorget.toml` still accepted — D44)
// ══════════════════════════════════════════════════════════════

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub package: PackageInfo,
    #[serde(default)]
    pub dependencies: BTreeMap<String, DepSpec>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageInfo {
    pub name: String,
    pub version: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DepSpec {
    Git {
        git: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        tag: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        branch: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        rev: Option<String>,
    },
    Path {
        path: PathBuf,
    },
}

#[derive(Debug)]
pub enum ManifestError {
    Io { path: PathBuf, error: std::io::Error },
    Parse { path: PathBuf, error: String },
    Serialize { error: String },
}

impl fmt::Display for ManifestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ManifestError::Io { path, error } => {
                write!(f, "cannot read '{}': {}", path.display(), error)
            }
            ManifestError::Parse { path, error } => {
                write!(f, "invalid manifest at '{}': {}", path.display(), error)
            }
            ManifestError::Serialize { error } => {
                write!(f, "failed to serialize manifest: {error}")
            }
        }
    }
}

impl Manifest {
    /// Read and parse a manifest file.
    pub fn from_path(path: &Path) -> Result<Self, ManifestError> {
        let content = fs::read_to_string(path).map_err(|e| ManifestError::Io {
            path: path.to_path_buf(),
            error: e,
        })?;
        toml::from_str(&content).map_err(|e| ManifestError::Parse {
            path: path.to_path_buf(),
            error: e.to_string(),
        })
    }

    /// Serialize and write to a file.
    pub fn save(&self, path: &Path) -> Result<(), ManifestError> {
        let content =
            toml::to_string_pretty(self).map_err(|e| ManifestError::Serialize {
                error: e.to_string(),
            })?;
        fs::write(path, content).map_err(|e| ManifestError::Io {
            path: path.to_path_buf(),
            error: e,
        })
    }

    /// Create a new manifest with the given package name.
    pub fn new(name: &str) -> Self {
        Self {
            package: PackageInfo {
                name: name.to_string(),
                version: "0.1.0".to_string(),
            },
            dependencies: BTreeMap::new(),
        }
    }
}

/// The manifest filename (D44). Every site that needs to name the manifest reads
/// this constant or calls [`manifest_path_in`] / [`find_manifest_in`] — never
/// spells the string inline, so the name lives at one source of truth.
///
/// D44 briefly renamed this to `manifest.toml` and then reverted: the rename's
/// only argument was that `manifest.toml` names the file's role, which is an
/// argument against `package.toml` (which stutters against its own `[package]`
/// table), not an argument for changing a name already in use. See the D44
/// ledger entry's amendment.
pub const MANIFEST_NAME: &str = "gorget.toml";

/// The canonical manifest path inside `dir` — whether or not it exists.
pub fn manifest_path_in(dir: &Path) -> PathBuf {
    dir.join(MANIFEST_NAME)
}

/// Locate an existing manifest in `dir`.
pub fn find_manifest_in(dir: &Path) -> Option<PathBuf> {
    let path = manifest_path_in(dir);
    path.exists().then_some(path)
}

/// Walk up from `start` looking for a manifest. Returns the directory containing
/// it, or `None` if none is found.
///
/// Accepts either filename so the rename does not strand existing checkouts.
pub fn find_project_root(start: &Path) -> Option<PathBuf> {
    let mut dir = if start.is_file() {
        start.parent()?.to_path_buf()
    } else {
        start.to_path_buf()
    };
    loop {
        if find_manifest_in(&dir).is_some() {
            return Some(dir);
        }
        if !dir.pop() {
            return None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_minimal_manifest() {
        let toml_str = r#"
[package]
name = "hello"
version = "0.1.0"
"#;
        let manifest: Manifest = toml::from_str(toml_str).unwrap();
        assert_eq!(manifest.package.name, "hello");
        assert_eq!(manifest.package.version, "0.1.0");
        assert!(manifest.dependencies.is_empty());
    }

    #[test]
    fn parse_manifest_with_deps() {
        let toml_str = r#"
[package]
name = "my-app"
version = "1.0.0"

[dependencies]
utils = { path = "../shared/utils" }
http-router = { git = "https://github.com/alice/http-router", tag = "v1.2.0" }
"#;
        let manifest: Manifest = toml::from_str(toml_str).unwrap();
        assert_eq!(manifest.dependencies.len(), 2);

        match &manifest.dependencies["utils"] {
            DepSpec::Path { path } => assert_eq!(path, &PathBuf::from("../shared/utils")),
            _ => panic!("expected path dep"),
        }

        match &manifest.dependencies["http-router"] {
            DepSpec::Git { git, tag, .. } => {
                assert_eq!(git, "https://github.com/alice/http-router");
                assert_eq!(tag.as_deref(), Some("v1.2.0"));
            }
            _ => panic!("expected git dep"),
        }
    }

    #[test]
    fn roundtrip_manifest() {
        let manifest = Manifest {
            package: PackageInfo {
                name: "test".to_string(),
                version: "0.1.0".to_string(),
            },
            dependencies: {
                let mut deps = BTreeMap::new();
                deps.insert(
                    "mylib".to_string(),
                    DepSpec::Path {
                        path: PathBuf::from("../mylib"),
                    },
                );
                deps
            },
        };

        let serialized = toml::to_string_pretty(&manifest).unwrap();
        let reparsed: Manifest = toml::from_str(&serialized).unwrap();
        assert_eq!(reparsed.package.name, "test");
        assert_eq!(reparsed.dependencies.len(), 1);
    }
}
