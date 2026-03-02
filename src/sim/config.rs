/// Configuration for the GIR interpreter (gg sim).
///
/// Controls UB detection, seeded RNG, leak reporting, and error detail level.
/// All checks are gated on fields in this struct so that `SimConfig::default()`
/// reproduces the Phase 0–3 behavior with zero overhead.
#[derive(Clone, Debug)]
pub struct SimConfig {
    /// Seed for deterministic random number generation (optional).
    pub seed: Option<u64>,
    /// Run the interpreter over a range of seeds to find non-deterministic bugs.
    pub many_seeds: Option<(u64, u64)>,
    /// Suppress memory leak reports at program exit.
    pub ignore_leaks: bool,
    /// Allow real I/O (future: sandbox enforcement).
    pub isolation: bool,
    /// Controls how much context is included in UB error messages.
    pub backtrace: BacktraceLevel,
    /// Enable use-after-free / double-free / uninitialized-read / value-validity checks.
    pub ub_checks: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BacktraceLevel {
    Off,
    Pruned,
    Full,
}

impl Default for SimConfig {
    fn default() -> Self {
        Self {
            seed: None,
            many_seeds: None,
            ignore_leaks: false,
            isolation: true,
            backtrace: BacktraceLevel::Pruned,
            ub_checks: false,
        }
    }
}

impl SimConfig {
    /// Parse config from CLI args and the `GGSIMFLAGS` env var.
    ///
    /// Recognized flags:
    ///   --seed=N               Seed RNG with N
    ///   --many-seeds=from..to  Run with each seed in [from, to)
    ///   --ignore-leaks         Suppress leak reports
    ///   --disable-isolation    Allow real I/O (future)
    ///   --backtrace=0|1|full   Error detail level
    ///   --ub-checks            Enable UB detection (P4b–P4d)
    pub fn from_args(args: &[String]) -> Self {
        let mut config = SimConfig::default();

        // Collect all flags from args + GGSIMFLAGS env var.
        let env_flags: Vec<String> = std::env::var("GGSIMFLAGS")
            .unwrap_or_default()
            .split_whitespace()
            .map(|s| s.to_string())
            .collect();

        for flag in args.iter().chain(env_flags.iter()) {
            if let Some(seed_str) = flag.strip_prefix("--seed=") {
                if let Ok(n) = seed_str.parse::<u64>() {
                    config.seed = Some(n);
                }
            } else if let Some(range_str) = flag.strip_prefix("--many-seeds=") {
                if let Some((from_str, to_str)) = range_str.split_once("..") {
                    if let (Ok(from), Ok(to)) = (from_str.parse::<u64>(), to_str.parse::<u64>()) {
                        config.many_seeds = Some((from, to));
                    }
                }
            } else if flag == "--ignore-leaks" {
                config.ignore_leaks = true;
            } else if flag == "--disable-isolation" {
                config.isolation = false;
            } else if let Some(bt_str) = flag.strip_prefix("--backtrace=") {
                config.backtrace = match bt_str {
                    "0" | "off" => BacktraceLevel::Off,
                    "1" | "pruned" => BacktraceLevel::Pruned,
                    "full" => BacktraceLevel::Full,
                    _ => BacktraceLevel::Pruned,
                };
            } else if flag == "--ub-checks" {
                config.ub_checks = true;
            }
        }

        config
    }
}
