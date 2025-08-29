use std::time::Instant;

#[derive(Debug)]
pub struct Timer {
    start: Option<Instant>,
    elapsed: u128, // microseconds
}

impl Timer {
    #[must_use]
    pub fn new() -> Self {
        Self {
            start: None,
            elapsed: 0,
        }
    }

    pub fn start(&mut self) {
        self.start = Some(Instant::now());
    }

    /// # Panics
    ///
    /// Panics if the timer was not started.
    pub fn stop(&mut self) {
        self.elapsed += self.start.take().unwrap().elapsed().as_micros();
    }

    #[must_use]
    pub fn elapsed(&self) -> u128 {
        self.elapsed
    }

    pub fn add(&mut self, other: u128) {
        self.elapsed += other;
    }
}

impl Default for Timer {
    fn default() -> Self {
        Self::new()
    }
}
