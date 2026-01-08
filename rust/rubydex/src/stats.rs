pub mod memory;
pub mod timer;

/// Helper function to compute percentage
#[allow(clippy::cast_precision_loss)]
#[must_use]
pub fn percentage(numerator: usize, denominator: usize) -> f64 {
    (numerator as f64 / denominator as f64) * 100.0
}
