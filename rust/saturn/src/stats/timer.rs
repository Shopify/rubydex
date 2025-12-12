use std::sync::Mutex;
use std::time::{Duration, Instant};

/// Generates a global timer for measuring performance across different phases of execution in the main thread.
///
/// This macro creates:
/// - A `Timer` struct with fields for each defined phase
/// - A global `TIMER` static for tracking measurements
/// - A `time_it!` macro to measure and record individual phase durations
///
/// The timer is only created when running with the `--stats` flag.
///
/// Usage:
/// 1. To add a new phase, add an entry to the `make_timer!` invocation at the bottom of this file
/// 2. Wrap code blocks with `time_it!(phase_name, { ... })` to measure them
macro_rules! make_timer {
    (
        $(
            $phase:ident, $label:literal;
        )*
    ) => {
        #[derive(Clone)]
        pub struct Timer {
            start_time: Instant,
            $(
                pub $phase: Duration,
            )*
        }

        pub static TIMER: Mutex<Option<Timer>> = Mutex::new(None);

        impl Default for Timer {
            fn default() -> Self {
                Self::new()
            }
        }

        impl Timer {
            #[must_use]
            pub fn new() -> Self {
                Self {
                    start_time: Instant::now(),
                    $(
                        $phase: Duration::ZERO,
                    )*
                }
            }

            pub fn set_global_timer(timer: Timer) {
                *TIMER.lock().unwrap() = Some(timer);
            }

            pub fn print_breakdown() {
                if let Some(ref timer) = *TIMER.lock().unwrap() {
                    macro_rules! format_breakdown {
                        ($name:expr, $duration:expr, $total:expr) => {
                            format!(
                                "{:<16} {:8.3}s ({:5.1}%)",
                                $name,
                                $duration.as_secs_f64(),
                                $duration.as_secs_f64() * 100.0 / $total.as_secs_f64()
                            )
                        };
                    }

                    let total_duration = timer.start_time.elapsed();
                    let mut accounted_time = Duration::ZERO;
                    $(
                        accounted_time += timer.$phase;
                    )*
                    let cleanup = total_duration - accounted_time;

                    println!();
                    println!("Timing breakdown");

                    $(
                        if timer.$phase != Duration::ZERO {
                            println!("  {}", format_breakdown!($label, timer.$phase, total_duration));
                        }
                    )*

                    println!("  {}", format_breakdown!("Cleanup", cleanup, total_duration));
                    println!("  Total:           {:8.3}s", total_duration.as_secs_f64());
                    println!();
                }
            }
        }

        #[macro_export]
        macro_rules! time_it {
            $(
                ($phase, $body:block) => {
                    {
                        let timer_enabled = {
                            let guard = $crate::stats::timer::TIMER.lock().unwrap();
                            guard.is_some()
                        };

                        if timer_enabled {
                            let start = std::time::Instant::now();
                            let result = $body;
                            let elapsed = start.elapsed();

                            if let Some(ref mut timer) = *$crate::stats::timer::TIMER.lock().unwrap() {
                                timer.$phase = elapsed;
                            }
                            result
                        } else {
                            $body
                        }
                    }
                };
            )*
        }

        pub use time_it;
    };
}

make_timer! {
    setup, "Initialization";
    listing1, "Listing (old)";
    listing2, "Listing (new)";
    listing3, "Listing (ignore)";
    indexing, "Indexing";
    resolution, "Resolution";
    querying, "Querying";
    integrity_check, "Integrity Check";
}
