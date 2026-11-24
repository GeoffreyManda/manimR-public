// manimR Rust Backend - Rendering Engine
// Frame state computation and animation application

use crate::animation::{EasingFunction, EasingType};
use extendr_api::prelude::*;
use nalgebra::Vector3;
use palette::{LinSrgb, Srgb};

/// Object state for rendering
#[derive(Clone)]
pub struct ObjectState {
    pub id: String,
    pub object_type: String,
    pub position: Vector3<f64>,
    pub rotation: f64,
    pub scale: f64,
    pub color: (f64, f64, f64),
    pub opacity: f64,
    pub visible: bool,

    // Type-specific fields
    pub text: Option<String>,
    pub font_size: f64,
    pub latex: Option<String>,
    pub shape_type: Option<String>,
    pub width: f64,
    pub height: f64,
    pub radius: f64,
    pub fill_color: Option<(f64, f64, f64)>,
    pub fill_opacity: f64,
    pub stroke_width: f64,
}

impl ObjectState {
    pub fn from_list(list: &List) -> Self {
        let id = list
            .get("id")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown")
            .to_string();
        let object_type = list
            .get("type")
            .and_then(|v| v.as_str())
            .unwrap_or("unknown")
            .to_string();

        let position = if let Some(pos) = list.get("position") {
            if let Some(slice) = pos.as_real_slice() {
                Vector3::new(
                    slice.get(0).copied().unwrap_or(0.0),
                    slice.get(1).copied().unwrap_or(0.0),
                    slice.get(2).copied().unwrap_or(0.0),
                )
            } else {
                Vector3::new(0.0, 0.0, 0.0)
            }
        } else {
            Vector3::new(0.0, 0.0, 0.0)
        };

        let color = list
            .get("color")
            .and_then(|v| v.as_str())
            .map(hex_to_rgb)
            .unwrap_or((1.0, 1.0, 1.0));

        Self {
            id,
            object_type,
            position,
            rotation: list
                .get("rotation")
                .and_then(|v| v.as_real())
                .unwrap_or(0.0),
            scale: list.get("scale").and_then(|v| v.as_real()).unwrap_or(1.0),
            color,
            opacity: list.get("opacity").and_then(|v| v.as_real()).unwrap_or(1.0),
            visible: false, // Start invisible
            text: list.get("text").and_then(|v| v.as_str()).map(String::from),
            font_size: list.get("size").and_then(|v| v.as_real()).unwrap_or(48.0),
            latex: list.get("latex").and_then(|v| v.as_str()).map(String::from),
            shape_type: list
                .get("shape_type")
                .and_then(|v| v.as_str())
                .map(String::from),
            width: list.get("width").and_then(|v| v.as_real()).unwrap_or(1.0),
            height: list.get("height").and_then(|v| v.as_real()).unwrap_or(1.0),
            radius: list.get("radius").and_then(|v| v.as_real()).unwrap_or(0.5),
            fill_color: list
                .get("fill_color")
                .and_then(|v| v.as_str())
                .map(hex_to_rgb),
            fill_opacity: list
                .get("fill_opacity")
                .and_then(|v| v.as_real())
                .unwrap_or(0.5),
            stroke_width: list
                .get("stroke_width")
                .and_then(|v| v.as_real())
                .unwrap_or(1.0),
        }
    }

    pub fn to_list(&self) -> List {
        let mut list = list!(
            id = r!(self.id.clone()),
            type = r!(self.object_type.clone()),
            position = r!(vec![self.position.x, self.position.y, self.position.z]),
            rotation = r!(self.rotation),
            scale = r!(self.scale),
            color = r!(rgb_to_hex(self.color.0, self.color.1, self.color.2)),
            opacity = r!(self.opacity),
            visible = r!(self.visible),
        );

        if let Some(ref text) = self.text {
            list.set_named("text", r!(text.clone())).ok();
            list.set_named("font_size", r!(self.font_size)).ok();
        }

        if let Some(ref latex) = self.latex {
            list.set_named("latex", r!(latex.clone())).ok();
        }

        if let Some(ref shape) = self.shape_type {
            list.set_named("shape_type", r!(shape.clone())).ok();
            list.set_named("width", r!(self.width)).ok();
            list.set_named("height", r!(self.height)).ok();
            list.set_named("radius", r!(self.radius)).ok();
            list.set_named("stroke_width", r!(self.stroke_width)).ok();
        }

        list
    }

    pub fn apply_animation(&mut self, anim: &AnimationSpec, time: f64) {
        let start = anim.start_time;
        let end = start + anim.duration;

        if time < start {
            return;
        }

        let progress = if time >= end {
            1.0
        } else {
            (time - start) / anim.duration
        };

        let eased = anim.easing.apply(progress.clamp(0.0, 1.0));

        match anim.anim_type.as_str() {
            "show" => {
                self.visible = true;
                self.opacity = 1.0;
            }
            "write_in" => {
                self.visible = true;
                self.opacity = eased;
            }
            "fade_in" => {
                self.visible = true;
                self.opacity = eased;
            }
            "fade_out" => {
                self.opacity = 1.0 - eased;
                if progress >= 1.0 {
                    self.visible = false;
                }
            }
            "grow_from_center" => {
                self.visible = true;
                self.scale = eased;
                self.opacity = 1.0;
            }
            "shrink_to_center" => {
                self.scale = 1.0 - eased;
                if progress >= 1.0 {
                    self.visible = false;
                }
            }
            "move_to" => {
                if let Some(target) = anim.target_position {
                    self.position = lerp_vec3(&self.position, &target, eased);
                }
            }
            "shift" => {
                if let Some(shift) = anim.shift_vector {
                    self.position += shift * eased;
                }
            }
            "rotate" => {
                if let Some(angle) = anim.rotation_angle {
                    self.rotation += angle * eased;
                }
            }
            "scale" => {
                if let Some(target_scale) = anim.target_scale {
                    self.scale = lerp(1.0, target_scale, eased);
                }
            }
            "change_color" => {
                if let Some(target) = anim.target_color {
                    self.color = lerp_color(&self.color, &target, eased);
                }
            }
            _ => {}
        }
    }
}

/// Animation specification
pub struct AnimationSpec {
    pub anim_type: String,
    pub object_id: String,
    pub start_time: f64,
    pub duration: f64,
    pub easing: EasingFunction,
    pub target_position: Option<Vector3<f64>>,
    pub shift_vector: Option<Vector3<f64>>,
    pub target_scale: Option<f64>,
    pub rotation_angle: Option<f64>,
    pub target_color: Option<(f64, f64, f64)>,
}

impl AnimationSpec {
    pub fn from_list(list: &List) -> Self {
        let anim_type = list
            .get("type")
            .and_then(|v| v.as_str())
            .unwrap_or("show")
            .to_string();
        let object_id = list
            .get("object_id")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let start_time = list
            .get("start_time")
            .and_then(|v| v.as_real())
            .unwrap_or(0.0);
        let duration = list
            .get("duration")
            .and_then(|v| v.as_real())
            .unwrap_or(1.0);
        let easing_name = list
            .get("easing")
            .and_then(|v| v.as_str())
            .unwrap_or("smooth");

        let mut spec = Self {
            anim_type,
            object_id,
            start_time,
            duration,
            easing: EasingFunction::from_str(easing_name),
            target_position: None,
            shift_vector: None,
            target_scale: None,
            rotation_angle: None,
            target_color: None,
        };

        // Parse optional fields
        if let Some(pos) = list.get("target_position") {
            if let Some(slice) = pos.as_real_slice() {
                spec.target_position = Some(Vector3::new(
                    slice.get(0).copied().unwrap_or(0.0),
                    slice.get(1).copied().unwrap_or(0.0),
                    slice.get(2).copied().unwrap_or(0.0),
                ));
            }
        }

        if let Some(shift) = list.get("shift_vector") {
            if let Some(slice) = shift.as_real_slice() {
                spec.shift_vector = Some(Vector3::new(
                    slice.get(0).copied().unwrap_or(0.0),
                    slice.get(1).copied().unwrap_or(0.0),
                    slice.get(2).copied().unwrap_or(0.0),
                ));
            }
        }

        if let Some(factor) = list.get("factor") {
            spec.target_scale = factor.as_real();
        }

        if let Some(angle) = list.get("angle") {
            spec.rotation_angle = angle.as_real();
        }

        if let Some(color) = list.get("color") {
            if let Some(hex) = color.as_str() {
                spec.target_color = Some(hex_to_rgb(hex));
            }
        }

        spec
    }
}

/// Frame state containing all visible objects
pub struct FrameState {
    pub time: f64,
    pub objects: Vec<ObjectState>,
}

impl FrameState {
    pub fn new(time: f64) -> Self {
        Self {
            time,
            objects: Vec::new(),
        }
    }

    pub fn compute(
        base_objects: &[ObjectState],
        animations: &[AnimationSpec],
        time: f64,
    ) -> Self {
        let mut state = Self::new(time);

        for obj in base_objects {
            let mut frame_obj = obj.clone();

            // Apply all relevant animations
            for anim in animations {
                if anim.object_id == obj.id {
                    frame_obj.apply_animation(anim, time);
                }
            }

            // Only add if visible
            if frame_obj.visible && frame_obj.opacity > 0.001 {
                state.objects.push(frame_obj);
            }
        }

        state
    }
}

// Helper functions
fn hex_to_rgb(hex: &str) -> (f64, f64, f64) {
    if hex.len() >= 7 && hex.starts_with('#') {
        if let (Ok(r), Ok(g), Ok(b)) = (
            u8::from_str_radix(&hex[1..3], 16),
            u8::from_str_radix(&hex[3..5], 16),
            u8::from_str_radix(&hex[5..7], 16),
        ) {
            return (r as f64 / 255.0, g as f64 / 255.0, b as f64 / 255.0);
        }
    }
    (1.0, 1.0, 1.0) // Default white
}

fn rgb_to_hex(r: f64, g: f64, b: f64) -> String {
    format!(
        "#{:02X}{:02X}{:02X}",
        (r.clamp(0.0, 1.0) * 255.0) as u8,
        (g.clamp(0.0, 1.0) * 255.0) as u8,
        (b.clamp(0.0, 1.0) * 255.0) as u8
    )
}

fn lerp(start: f64, end: f64, t: f64) -> f64 {
    start + (end - start) * t
}

fn lerp_vec3(a: &Vector3<f64>, b: &Vector3<f64>, t: f64) -> Vector3<f64> {
    a * (1.0 - t) + b * t
}

fn lerp_color(c1: &(f64, f64, f64), c2: &(f64, f64, f64), t: f64) -> (f64, f64, f64) {
    let c1_lin: LinSrgb<f64> = Srgb::new(c1.0, c1.1, c1.2).into_linear();
    let c2_lin: LinSrgb<f64> = Srgb::new(c2.0, c2.1, c2.2).into_linear();

    let interpolated = c1_lin * (1.0 - t) + c2_lin * t;
    let srgb: Srgb<f64> = interpolated.into();

    (srgb.red, srgb.green, srgb.blue)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_object_state_apply_show() {
        let mut state = ObjectState {
            id: "test".to_string(),
            object_type: "text".to_string(),
            position: Vector3::new(0.0, 0.0, 0.0),
            rotation: 0.0,
            scale: 1.0,
            color: (1.0, 1.0, 1.0),
            opacity: 0.0,
            visible: false,
            text: Some("test".to_string()),
            font_size: 48.0,
            latex: None,
            shape_type: None,
            width: 1.0,
            height: 1.0,
            radius: 0.5,
            fill_color: None,
            fill_opacity: 0.5,
            stroke_width: 1.0,
        };

        let anim = AnimationSpec {
            anim_type: "show".to_string(),
            object_id: "test".to_string(),
            start_time: 0.0,
            duration: 1.0,
            easing: EasingFunction::new(EasingType::Linear),
            target_position: None,
            shift_vector: None,
            target_scale: None,
            rotation_angle: None,
            target_color: None,
        };

        state.apply_animation(&anim, 0.5);
        assert!(state.visible);
        assert_eq!(state.opacity, 1.0);
    }
}
