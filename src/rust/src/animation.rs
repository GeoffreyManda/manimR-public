// manimR Rust Backend - Animation Mathematics
// Easing functions and interpolation

use std::f64::consts::PI;

/// Easing type enumeration
#[derive(Clone, Copy, Debug)]
pub enum EasingType {
    Linear,
    Smooth,
    InQuad,
    OutQuad,
    InOutQuad,
    InCubic,
    OutCubic,
    InOutCubic,
    InSine,
    OutSine,
    InOutSine,
    InExpo,
    OutExpo,
    InOutExpo,
    InCirc,
    OutCirc,
    InBack,
    OutBack,
    InOutBack,
    InBounce,
    OutBounce,
    InElastic,
    OutElastic,
}

impl EasingType {
    /// Convert string to easing type
    pub fn from_str(s: &str) -> Self {
        match s {
            "linear" => EasingType::Linear,
            "smooth" => EasingType::Smooth,
            "in_quad" => EasingType::InQuad,
            "out_quad" => EasingType::OutQuad,
            "in_out_quad" => EasingType::InOutQuad,
            "in_cubic" => EasingType::InCubic,
            "out_cubic" => EasingType::OutCubic,
            "in_out_cubic" => EasingType::InOutCubic,
            "in_sine" => EasingType::InSine,
            "out_sine" => EasingType::OutSine,
            "in_out_sine" => EasingType::InOutSine,
            "in_expo" => EasingType::InExpo,
            "out_expo" => EasingType::OutExpo,
            "in_out_expo" => EasingType::InOutExpo,
            "in_circ" => EasingType::InCirc,
            "out_circ" => EasingType::OutCirc,
            "in_back" => EasingType::InBack,
            "out_back" => EasingType::OutBack,
            "in_out_back" => EasingType::InOutBack,
            "in_bounce" => EasingType::InBounce,
            "out_bounce" => EasingType::OutBounce,
            "in_elastic" => EasingType::InElastic,
            "out_elastic" => EasingType::OutElastic,
            _ => EasingType::Smooth,
        }
    }
}

/// Easing function wrapper
#[derive(Clone, Copy)]
pub struct EasingFunction {
    easing_type: EasingType,
}

impl EasingFunction {
    pub fn new(easing_type: EasingType) -> Self {
        Self { easing_type }
    }

    pub fn from_str(s: &str) -> Self {
        Self::new(EasingType::from_str(s))
    }

    /// Apply easing to a value t in [0, 1]
    pub fn apply(&self, t: f64) -> f64 {
        let t = t.clamp(0.0, 1.0);

        match self.easing_type {
            EasingType::Linear => t,
            EasingType::Smooth => t * t * (3.0 - 2.0 * t),
            EasingType::InQuad => t * t,
            EasingType::OutQuad => {
                let u = 1.0 - t;
                1.0 - u * u
            }
            EasingType::InOutQuad => {
                if t < 0.5 {
                    2.0 * t * t
                } else {
                    let u = -2.0 * t + 2.0;
                    1.0 - u * u / 2.0
                }
            }
            EasingType::InCubic => t * t * t,
            EasingType::OutCubic => {
                let u = 1.0 - t;
                1.0 - u * u * u
            }
            EasingType::InOutCubic => {
                if t < 0.5 {
                    4.0 * t * t * t
                } else {
                    let u = -2.0 * t + 2.0;
                    1.0 - u * u * u / 2.0
                }
            }
            EasingType::InSine => 1.0 - (t * PI / 2.0).cos(),
            EasingType::OutSine => (t * PI / 2.0).sin(),
            EasingType::InOutSine => -(PI * t).cos() / 2.0 + 0.5,
            EasingType::InExpo => {
                if t == 0.0 {
                    0.0
                } else {
                    2.0_f64.powf(10.0 * (t - 1.0))
                }
            }
            EasingType::OutExpo => {
                if t == 1.0 {
                    1.0
                } else {
                    1.0 - 2.0_f64.powf(-10.0 * t)
                }
            }
            EasingType::InOutExpo => {
                if t == 0.0 {
                    0.0
                } else if t == 1.0 {
                    1.0
                } else if t < 0.5 {
                    2.0_f64.powf(20.0 * t - 10.0) / 2.0
                } else {
                    (2.0 - 2.0_f64.powf(-20.0 * t + 10.0)) / 2.0
                }
            }
            EasingType::InCirc => 1.0 - (1.0 - t * t).sqrt(),
            EasingType::OutCirc => {
                let u = t - 1.0;
                (1.0 - u * u).sqrt()
            }
            EasingType::InBack => {
                const C1: f64 = 1.70158;
                const C3: f64 = C1 + 1.0;
                C3 * t * t * t - C1 * t * t
            }
            EasingType::OutBack => {
                const C1: f64 = 1.70158;
                const C3: f64 = C1 + 1.0;
                let u = t - 1.0;
                1.0 + C3 * u * u * u + C1 * u * u
            }
            EasingType::InOutBack => {
                const C1: f64 = 1.70158;
                const C2: f64 = C1 * 1.525;
                if t < 0.5 {
                    ((2.0 * t).powi(2) * ((C2 + 1.0) * 2.0 * t - C2)) / 2.0
                } else {
                    let u = 2.0 * t - 2.0;
                    (u.powi(2) * ((C2 + 1.0) * u + C2) + 2.0) / 2.0
                }
            }
            EasingType::InBounce => 1.0 - Self::out_bounce(1.0 - t),
            EasingType::OutBounce => Self::out_bounce(t),
            EasingType::InElastic => {
                const C4: f64 = 2.0 * PI / 3.0;
                if t == 0.0 {
                    0.0
                } else if t == 1.0 {
                    1.0
                } else {
                    -(2.0_f64.powf(10.0 * t - 10.0)) * ((t * 10.0 - 10.75) * C4).sin()
                }
            }
            EasingType::OutElastic => {
                const C4: f64 = 2.0 * PI / 3.0;
                if t == 0.0 {
                    0.0
                } else if t == 1.0 {
                    1.0
                } else {
                    2.0_f64.powf(-10.0 * t) * ((t * 10.0 - 0.75) * C4).sin() + 1.0
                }
            }
        }
    }

    fn out_bounce(t: f64) -> f64 {
        const N1: f64 = 7.5625;
        const D1: f64 = 2.75;

        if t < 1.0 / D1 {
            N1 * t * t
        } else if t < 2.0 / D1 {
            let u = t - 1.5 / D1;
            N1 * u * u + 0.75
        } else if t < 2.5 / D1 {
            let u = t - 2.25 / D1;
            N1 * u * u + 0.9375
        } else {
            let u = t - 2.625 / D1;
            N1 * u * u + 0.984375
        }
    }
}

/// Smoothstep function (Hermite interpolation)
pub fn smoothstep(edge0: f64, edge1: f64, x: f64) -> f64 {
    let t = ((x - edge0) / (edge1 - edge0)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

/// Smootherstep function (higher order smoothstep)
pub fn smootherstep(edge0: f64, edge1: f64, x: f64) -> f64 {
    let t = ((x - edge0) / (edge1 - edge0)).clamp(0.0, 1.0);
    t * t * t * (t * (t * 6.0 - 15.0) + 10.0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_linear() {
        let easing = EasingFunction::new(EasingType::Linear);
        assert!((easing.apply(0.0) - 0.0).abs() < 1e-10);
        assert!((easing.apply(0.5) - 0.5).abs() < 1e-10);
        assert!((easing.apply(1.0) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_smooth() {
        let easing = EasingFunction::new(EasingType::Smooth);
        assert!((easing.apply(0.0) - 0.0).abs() < 1e-10);
        assert!((easing.apply(1.0) - 1.0).abs() < 1e-10);
        // Smooth should have zero derivative at endpoints
        assert!(easing.apply(0.01) > 0.0);
    }

    #[test]
    fn test_from_str() {
        assert!(matches!(EasingType::from_str("smooth"), EasingType::Smooth));
        assert!(matches!(EasingType::from_str("invalid"), EasingType::Smooth)); // default
    }
}
