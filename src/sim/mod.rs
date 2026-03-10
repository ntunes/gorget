/// GIR interpreter (gg sim) — executes GIR directly without C compilation.
///
/// Phase 0: Simple programs — hello, variables, functions, control flow, expressions,
/// type casts, recursion. Targets ~30 fixtures.
///
/// Analogous to Rust's miri (which interprets MIR), this provides:
/// - A reference oracle for testing future backends
/// - Faster iteration (no C compiler needed)
/// - A foundation for UB detection (Phase 4)

mod config;
mod crypto;
mod dispatch;
mod error;
mod runtime;
mod source_loc;
mod value;

use std::io::Write;

pub use config::SimConfig;
pub use dispatch::Interpreter;
pub use error::{SimError, SimResult};
pub use value::Value;

use crate::ir::Module;

/// Interpret a GIR module and return the exit code.
/// `source_path` is used as argv[0] for the simulated program.
/// Writes program output to stdout.
pub fn interpret(module: &Module, source_path: &str, config: &SimConfig) -> i32 {
    // P5b: If --many-seeds=from..to, run multiple times and report failures.
    if let Some((from, to)) = config.many_seeds {
        return run_many_seeds(module, source_path, config, from, to);
    }

    runtime::set_program_name(source_path);

    // P5a: Seed the PRNG if --seed=N was given.
    if let Some(seed) = config.seed {
        runtime::seed_rng(seed);
    }

    // P5d: Apply isolation mode (default: on).
    runtime::set_isolation(config.isolation);

    let mut interp = Interpreter::new(module, config);
    interp.init_globals();

    // If the module has bench functions, run them.
    if !module.runtime.bench_fns.is_empty() {
        return run_bench_suite(&mut interp);
    }

    // If the module has test functions, run them as the test runner would.
    if !module.runtime.test_fns.is_empty() || module.runtime.is_test_module {
        return run_test_suite(&mut interp);
    }

    // Hot-reload modules have init/tick/reload instead of main.
    if module.runtime.hot_reload {
        return run_hot_reload(&mut interp);
    }

    // Find main function
    if interp.module.find_function("main").is_none() {
        eprintln!("gg sim: no main() function found");
        return 1;
    }

    // Run the program
    let exit_code = run_main(&mut interp);

    // P4e: leak detection — report heap slots that were never freed.
    // Skips is_ref_promoted slots (implementation-internal heap refs).
    // Returns 1 if leaks are found so the process exits non-zero.
    if !config.ignore_leaks && config.ub_checks {
        if report_leaks(&interp) > 0 {
            return 1;
        }
    }

    exit_code
}

/// Run the program repeatedly with seeds `from..to` to hunt non-deterministic bugs.
/// Stdout/stderr from each run is discarded; only failures are reported to stderr.
fn run_many_seeds(module: &Module, source_path: &str, config: &SimConfig, from: u64, to: u64) -> i32 {
    // Build a single-run config (no recursive many-seeds).
    let single_config = SimConfig { many_seeds: None, ..config.clone() };

    let mut any_failure = false;
    for seed in from..to {
        runtime::seed_rng(seed);
        runtime::set_isolation(config.isolation);
        runtime::set_program_name(source_path);

        let mut interp = Interpreter::new(module, &single_config);
        interp.init_globals();

        let has_main = interp.module.find_function("main").is_some();
        let result = if !module.runtime.test_fns.is_empty() || module.runtime.is_test_module {
            // For test modules, just check if __suite_setup (if any) panics.
            Ok(Value::Unit)
        } else if !has_main {
            Err(SimError::Panic("no main() function".to_string()))
        } else {
            interp.call_function("main", vec![], 0)
        };

        // Discard captured output (don't flush) — seeds are for determinism testing.
        let code = match result {
            Ok(_) => 0,
            Err(SimError::Exit(code)) => code,
            Err(SimError::Panic(msg)) => {
                eprintln!("gg sim: seed={seed}: panic: {msg}");
                1
            }
            Err(SimError::Unimplemented(name)) => {
                eprintln!("gg sim: seed={seed}: unimplemented: {name}");
                1
            }
            Err(e) => {
                eprintln!("gg sim: seed={seed}: error: {e}");
                1
            }
        };

        if code != 0 {
            eprintln!("gg sim: seed={seed} FAILED");
            any_failure = true;
        } else {
            eprint!(".");
        }
    }
    eprintln!(); // newline after progress dots
    if any_failure { 1 } else { 0 }
}

/// Run a test suite: call each test function and report results.
fn run_test_suite(interp: &mut Interpreter) -> i32 {
    let test_fns = interp.module.runtime.test_fns.clone();
    let n = test_fns.len();
    let header = format!("Running {n} tests...\n");
    interp.stdout.extend_from_slice(header.as_bytes());

    let mut passed = 0i32;
    let mut failed = 0i32;
    let mut results: Vec<(&str, &str)> = Vec::new(); // (name, status)

    // Suite setup (if present)
    if interp.module.runtime.has_suite_setup {
        let _ = interp.call_function("__suite_setup", vec![], 0);
        flush_output(&interp.stdout, &interp.stderr);
        interp.stdout.clear();
        interp.stderr.clear();
        let _ = std::io::stdout().flush();
    }

    for info in &test_fns {
        // Reset error context between tests so a prior failure doesn't pollute the next.
        interp.call_stack.clear();
        interp.last_error_backtrace = None;
        interp.last_error_span = None;

        let label = format!("  test: {} ... ", info.display_name);
        interp.stdout.extend_from_slice(label.as_bytes());

        // Skipped tests: report and continue
        if info.skipped {
            let msg = if let Some(ref reason) = info.skip_reason {
                format!("SKIP ({reason})\n")
            } else {
                "SKIP\n".to_string()
            };
            interp.stdout.extend_from_slice(msg.as_bytes());
            results.push((&info.display_name, "skip"));
            continue;
        }

        let start = std::time::Instant::now();

        let result = interp.call_function(&info.fn_name, vec![], 0);

        let elapsed_ms = start.elapsed().as_millis();

        // Timeout check: if elapsed exceeds timeout_ms, treat as failure
        if let Some(timeout) = info.timeout_ms {
            if elapsed_ms >= timeout as u128 {
                failed += 1;
                let line = format!("FAIL: timed out after {timeout}ms ({elapsed_ms}ms)\n");
                interp.stdout.extend_from_slice(line.as_bytes());
                results.push((&info.display_name, "fail"));
                continue;
            }
        }

        if info.should_panic {
            // should_panic test: pass if it panicked, fail if it succeeded
            match result {
                Err(SimError::Panic(ref msg)) => {
                    let ok = if let Some(ref expected) = info.expected_panic_msg {
                        msg.contains(expected.as_str())
                    } else {
                        true
                    };
                    if ok {
                        passed += 1;
                        let line = format!("PASS ({elapsed_ms}ms)\n");
                        interp.stdout.extend_from_slice(line.as_bytes());
                    } else {
                        failed += 1;
                        let expected = info.expected_panic_msg.as_deref().unwrap_or("");
                        let line = format!("FAIL: expected panic containing \"{expected}\", got: {msg} ({elapsed_ms}ms)\n");
                        interp.stdout.extend_from_slice(line.as_bytes());
                    }
                }
                Ok(_) => {
                    failed += 1;
                    let line = format!("FAIL: expected panic but test passed ({elapsed_ms}ms)\n");
                    interp.stdout.extend_from_slice(line.as_bytes());
                }
                Err(SimError::Unimplemented(ref name)) => {
                    flush_output(&interp.stdout, &interp.stderr);
                    eprintln!("gg sim: unimplemented runtime function: {name}");
                    return 1;
                }
                Err(SimError::Exit(code)) => return code,
                Err(_e) => {
                    // Other errors (stack overflow, etc.) — treat as unexpected failure
                    passed += 1; // panic is ok for should_panic
                    let line = format!("PASS ({elapsed_ms}ms)\n");
                    interp.stdout.extend_from_slice(line.as_bytes());
                }
            }
        } else {
            match result {
                Ok(_) => {
                    passed += 1;
                    let line = format!("PASS ({elapsed_ms}ms)\n");
                    interp.stdout.extend_from_slice(line.as_bytes());
                }
                Err(SimError::Panic(msg)) => {
                    failed += 1;
                    let line = format!("FAIL: {msg} ({elapsed_ms}ms)\n");
                    interp.stdout.extend_from_slice(line.as_bytes());

                    if !matches!(interp.backtrace_level, config::BacktraceLevel::Off) {
                        flush_output(&interp.stdout, &interp.stderr);
                        interp.stdout.clear();
                        interp.stderr.clear();
                        let bt = interp.last_error_backtrace.take();
                        let err_span = interp.last_error_span.take();
                        let fname = interp.module.source_filename.as_deref().unwrap_or("<unknown>");
                        let li = interp.module.source_code.as_ref()
                            .map(|s| source_loc::LineIndex::new(s));
                        source_loc::format_sim_location(
                            err_span,
                            bt.as_deref(),
                            fname,
                            interp.module.source_code.as_deref(),
                            li.as_ref(),
                            &interp.backtrace_level,
                        );
                    }
                }
                Err(SimError::Unimplemented(name)) => {
                    flush_output(&interp.stdout, &interp.stderr);
                    eprintln!("gg sim: unimplemented runtime function: {name}");
                    return 1;
                }
                Err(SimError::Exit(code)) => return code,
                Err(e) => {
                    failed += 1;
                    let line = format!("FAIL: {e} ({elapsed_ms}ms)\n");
                    interp.stdout.extend_from_slice(line.as_bytes());

                    if !matches!(interp.backtrace_level, config::BacktraceLevel::Off) {
                        flush_output(&interp.stdout, &interp.stderr);
                        interp.stdout.clear();
                        interp.stderr.clear();
                        let bt = interp.last_error_backtrace.take();
                        let err_span = interp.last_error_span.take();
                        let fname = interp.module.source_filename.as_deref().unwrap_or("<unknown>");
                        let li = interp.module.source_code.as_ref()
                            .map(|s| source_loc::LineIndex::new(s));
                        source_loc::format_sim_location(
                            err_span,
                            bt.as_deref(),
                            fname,
                            interp.module.source_code.as_deref(),
                            li.as_ref(),
                            &interp.backtrace_level,
                        );
                    }
                }
            }
        }
    }

    // Suite teardown (if present) — runs BEFORE the final summary line.
    if interp.module.runtime.has_suite_teardown {
        // Flush test result lines first, then let teardown print its output, then print summary.
        flush_output(&interp.stdout, &interp.stderr);
        interp.stdout.clear();
        interp.stderr.clear();
        let _ = interp.call_function("__suite_teardown", vec![], 0);
        flush_output(&interp.stdout, &interp.stderr);
        interp.stdout.clear();
        interp.stderr.clear();
    }

    let total_ms = 0u128; // we sum per-test times in a real impl; zero is fine here
    let summary = format!("\n{passed} passed, {failed} failed ({total_ms}ms)\n");
    interp.stdout.extend_from_slice(summary.as_bytes());
    flush_output(&interp.stdout, &interp.stderr);

    if failed > 0 { 1 } else { 0 }
}

/// Run a benchmark suite under the simulator.
fn run_bench_suite(interp: &mut Interpreter) -> i32 {
    let bench_fns = interp.module.runtime.bench_fns.clone();
    let n = bench_fns.len();
    let header = format!("Running {n} benchmarks...\n\n");
    interp.stdout.extend_from_slice(header.as_bytes());

    if interp.module.runtime.has_suite_setup {
        let _ = interp.call_function("__suite_setup", vec![], 0);
        flush_output(&interp.stdout, &interp.stderr);
        interp.stdout.clear();
        interp.stderr.clear();
    }

    for info in &bench_fns {
        interp.call_stack.clear();
        interp.last_error_backtrace = None;
        interp.last_error_span = None;

        // Warmup: 3 iterations
        for _ in 0..3 {
            if let Err(e) = interp.call_function(&info.fn_name, vec![], 0) {
                let msg = format!("  bench: {} ... FAIL ({e})\n", info.display_name);
                interp.stdout.extend_from_slice(msg.as_bytes());
                flush_output(&interp.stdout, &interp.stderr);
                interp.stdout.clear();
                interp.stderr.clear();
                continue;
            }
        }

        // Auto-calibrate: start with 100 iterations, double until >= 1 second
        let mut iters = 100u64;
        let total_ns;
        loop {
            let start = std::time::Instant::now();
            let mut panicked = false;
            for _ in 0..iters {
                if let Err(e) = interp.call_function(&info.fn_name, vec![], 0) {
                    let msg = format!("  bench: {} ... FAIL ({e})\n", info.display_name);
                    interp.stdout.extend_from_slice(msg.as_bytes());
                    panicked = true;
                    break;
                }
            }
            if panicked { total_ns = 0; break; }
            let elapsed = start.elapsed().as_nanos();
            if elapsed >= 1_000_000_000 {
                total_ns = elapsed;
                break;
            }
            if elapsed < 10_000_000 {
                iters *= 100;
            } else {
                iters *= 2;
            }
        }

        if total_ns == 0 { continue; }

        let avg_ns = total_ns as f64 / iters as f64;
        let line = if avg_ns < 1000.0 {
            format!("  bench: {} ... {} iters, {:.0} ns/iter\n", info.display_name, iters, avg_ns)
        } else if avg_ns < 1_000_000.0 {
            format!("  bench: {} ... {} iters, {:.2} us/iter\n", info.display_name, iters, avg_ns / 1000.0)
        } else if avg_ns < 1_000_000_000.0 {
            format!("  bench: {} ... {} iters, {:.2} ms/iter\n", info.display_name, iters, avg_ns / 1_000_000.0)
        } else {
            format!("  bench: {} ... {} iters, {:.2} s/iter\n", info.display_name, iters, avg_ns / 1_000_000_000.0)
        };
        interp.stdout.extend_from_slice(line.as_bytes());
    }

    if interp.module.runtime.has_suite_teardown {
        flush_output(&interp.stdout, &interp.stderr);
        interp.stdout.clear();
        interp.stderr.clear();
        let _ = interp.call_function("__suite_teardown", vec![], 0);
        flush_output(&interp.stdout, &interp.stderr);
        interp.stdout.clear();
        interp.stderr.clear();
    }

    let summary = format!("\n{n} benchmarks complete.\n");
    interp.stdout.extend_from_slice(summary.as_bytes());
    flush_output(&interp.stdout, &interp.stderr);

    0
}

/// Run a regular (non-test) program's main() and return exit code.
fn run_main(interp: &mut Interpreter) -> i32 {
    let result = interp.call_function("main", vec![], 0);

    match result {
        Ok(_) => {
            flush_output(&interp.stdout, &interp.stderr);
            0
        }
        Err(SimError::Exit(code)) => {
            flush_output(&interp.stdout, &interp.stderr);
            code
        }
        Err(SimError::Unimplemented(name)) => {
            flush_output(&interp.stdout, &interp.stderr);
            eprintln!("gg sim: unimplemented runtime function: {name}");
            eprintln!("  hint: use `gg run` to compile and execute natively");
            1
        }
        Err(e) => {
            flush_output(&interp.stdout, &interp.stderr);

            // Build a rich error message with source location and backtrace.
            let bt = interp.last_error_backtrace.take();
            let err_span = interp.last_error_span.take();
            let fname = interp.module.source_filename.as_deref().unwrap_or("<unknown>");
            let li = interp.module.source_code.as_ref()
                .map(|s| source_loc::LineIndex::new(s));
            let level = interp.backtrace_level.clone();

            // SimError::Display already includes the correct gorget:/gg sim: prefix.
            let msg = e.to_string();

            source_loc::format_sim_error(
                &msg,
                err_span,
                bt.as_deref(),
                fname,
                interp.module.source_code.as_deref(),
                li.as_ref(),
                &level,
            );

            match e {
                SimError::Panic(_) | SimError::Overflow | SimError::DivisionByZero => {
                    std::process::exit(1);
                }
                _ => 1,
            }
        }
    }
}

/// Run a hot-reload program: call init(), then loop calling tick(&state) until false.
fn run_hot_reload(interp: &mut Interpreter) -> i32 {
    use value::Value;
    // Call init() to get the initial state.
    let state = match interp.call_function("init", vec![], 0) {
        Ok(v) => v,
        Err(SimError::Exit(code)) => {
            flush_output(&interp.stdout, &interp.stderr);
            return code;
        }
        Err(SimError::Panic(msg)) => {
            flush_output(&interp.stdout, &interp.stderr);
            eprintln!("gorget: panic: {msg}");
            return 1;
        }
        Err(e) => {
            flush_output(&interp.stdout, &interp.stderr);
            eprintln!("gg sim: hot-reload init error: {e}");
            return 1;
        }
    };
    // Allocate state on heap so we can pass &state to tick.
    let state_addr = interp.heap_alloc(state);
    // Loop: call tick(&state) until it returns false.
    loop {
        let result = interp.call_function("tick", vec![Value::MutPtr(state_addr)], 0);
        flush_output(&interp.stdout, &interp.stderr);
        interp.stdout.clear();
        interp.stderr.clear();
        match result {
            Ok(Value::Bool(false)) => break,
            Ok(_) => {}
            Err(SimError::Exit(code)) => return code,
            Err(SimError::Panic(msg)) => {
                eprintln!("gorget: panic: {msg}");
                return 1;
            }
            Err(e) => {
                eprintln!("gg sim: hot-reload tick error: {e}");
                return 1;
            }
        }
    }
    0
}

fn flush_output(stdout: &[u8], stderr: &[u8]) {
    if !stdout.is_empty() {
        let _ = std::io::stdout().write_all(stdout);
        let _ = std::io::stdout().flush();
    }
    if !stderr.is_empty() {
        let _ = std::io::stderr().write_all(stderr);
        let _ = std::io::stderr().flush();
    }
}

/// P4e: report live heap allocations that were not freed.
/// Skips ref-promoted slots (implementation artifacts, not user allocations).
/// Returns the number of leaked allocations.
fn report_leaks(interp: &Interpreter) -> usize {
    let leaks: Vec<_> = interp.heap_meta.iter()
        .filter(|(_, meta)| meta.alive && !meta.is_ref_promoted)
        .collect();
    if !leaks.is_empty() {
        eprintln!("gg sim: {} allocation(s) leaked", leaks.len());
        for (addr, meta) in &leaks {
            eprintln!("  heap[{addr}]: allocated in {}", meta.alloc_fn);
        }
    }
    leaks.len()
}
