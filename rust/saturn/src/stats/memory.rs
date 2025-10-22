use std::io;

#[cfg(unix)]
use libc::{RUSAGE_SELF, getrusage, rusage};

/// Memory statistics for the current process
#[derive(Debug, Clone, Copy)]
pub struct MemoryStats {
    /// Maximum resident set size in bytes
    pub max_rss_bytes: i64,
}

impl MemoryStats {
    /// Get current memory statistics using getrusage
    ///
    /// # Errors
    ///
    /// Returns an error if the `getrusage` system call fails.
    #[cfg(unix)]
    pub fn current() -> Result<Self, io::Error> {
        unsafe {
            let mut usage = std::mem::MaybeUninit::<rusage>::uninit();
            let result = getrusage(RUSAGE_SELF, usage.as_mut_ptr());

            if result == 0 {
                let usage = usage.assume_init();
                // On macOS and BSD, ru_maxrss is in bytes
                // On Linux, ru_maxrss is in kilobytes
                let max_rss_bytes = if cfg!(target_os = "linux") {
                    usage.ru_maxrss * 1024
                } else {
                    usage.ru_maxrss
                };

                Ok(Self { max_rss_bytes })
            } else {
                Err(io::Error::last_os_error())
            }
        }
    }

    /// Get current memory statistics (not supported on non-Unix platforms)
    ///
    /// # Errors
    ///
    /// Returns an error on non-Unix platforms where memory statistics are not supported.
    #[cfg(not(unix))]
    pub fn current() -> Result<Self, io::Error> {
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "Memory statistics not supported on this platform",
        ))
    }

    /// Print memory statistics in a human-readable format
    #[allow(clippy::cast_precision_loss)]
    pub fn print(&self) {
        let mrss_mb = self.max_rss_bytes as f64 / 1024.0 / 1024.0;

        println!("Maximum RSS: {} bytes ({:.2} MB)", self.max_rss_bytes, mrss_mb);
        println!();
    }

    /// Retrieve and print memory usage statistics
    pub fn print_memory_usage() {
        match Self::current() {
            Ok(stats) => stats.print(),
            Err(e) => eprintln!("Warning: Could not retrieve memory statistics: {e}"),
        }
    }
}
