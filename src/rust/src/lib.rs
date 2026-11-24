// manimR Rust Backend - Main Library
// High-performance mathematical animation engine with parallel rendering

use extendr_api::prelude::*;
use nalgebra::{Vector2, Vector3, Matrix3};
use palette::{Srgb, LinSrgb};
use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::Mutex;

pub mod animation;
pub mod geometry;
pub mod render;

use animation::{EasingFunction, EasingType};
use geometry::{ShapeType, VertexCache};
use render::{FrameState, AnimationSpec};

/// Global vertex cache for geometry reuse
lazy_static::lazy_static! {
    static ref GEOMETRY_CACHE: Mutex<VertexCache> = Mutex::new(VertexCache::new());
}

/// Convert hex color string to RGB components
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
    (0.0, 0.0, 0.0)
}

/// Convert RGB components to hex color string
fn rgb_to_hex(r: f64, g: f64, b: f64) -> String {
    format!(
        "#{:02X}{:02X}{:02X}",
        (r.clamp(0.0, 1.0) * 255.0) as u8,
        (g.clamp(0.0, 1.0) * 255.0) as u8,
        (b.clamp(0.0, 1.0) * 255.0) as u8
    )
}

/// Linear interpolation between two values
fn lerp(start: f64, end: f64, t: f64) -> f64 {
    start + (end - start) * t.clamp(0.0, 1.0)
}

/// Vectorized easing function (Rust Backend)
/// @export
#[extendr]
fn rust_easing_vectorized(t: &[f64], easing_name: &str) -> Vec<f64> {
    let easing = EasingFunction::from_str(easing_name);

    t.par_iter()
        .map(|&time| easing.apply(time.clamp(0.0, 1.0)))
        .collect()
}

/// Vectorized linear interpolation (Rust Backend)
/// @export
#[extendr]
fn rust_lerp_vectorized(start: &[f64], end: &[f64], t: &[f64]) -> Vec<f64> {
    let n = start.len().max(end.len()).max(t.len());

    (0..n)
        .into_par_iter()
        .map(|i| {
            let s = start[i % start.len()];
            let e = end[i % end.len()];
            let ti = t[i % t.len()].clamp(0.0, 1.0);
            s + (e - s) * ti
        })
        .collect()
}

/// Color interpolation (Rust Backend)
/// @export
#[extendr]
fn rust_color_interpolate(color1: &str, color2: &str, t: &[f64]) -> Vec<String> {
    let (r1, g1, b1) = hex_to_rgb(color1);
    let (r2, g2, b2) = hex_to_rgb(color2);

    let c1: LinSrgb<f64> = Srgb::new(r1, g1, b1).into_linear();
    let c2: LinSrgb<f64> = Srgb::new(r2, g2, b2).into_linear();

    t.par_iter()
        .map(|&ti| {
            let t_clamped = ti.clamp(0.0, 1.0);
            let interpolated = c1 * (1.0 - t_clamped) + c2 * t_clamped;
            let srgb: Srgb<f64> = interpolated.into();
            rgb_to_hex(srgb.red, srgb.green, srgb.blue)
        })
        .collect()
}

/// Generate shape geometry (Rust Backend)
/// @export
#[extendr]
fn rust_generate_geometry(shape_type: &str, params: List) -> Robj {
    let shape = match shape_type {
        "circle" => {
            let radius = params.get("radius")
                .and_then(|v| v.as_real())
                .unwrap_or(1.0);
            let n_points = params.get("n_points")
                .and_then(|v| v.as_integer())
                .unwrap_or(100) as usize;
            ShapeType::Circle { radius, n_points }
        }
        "ellipse" => {
            let rx = params.get("rx")
                .and_then(|v| v.as_real())
                .unwrap_or(1.0);
            let ry = params.get("ry")
                .and_then(|v| v.as_real())
                .unwrap_or(0.5);
            let n_points = params.get("n_points")
                .and_then(|v| v.as_integer())
                .unwrap_or(100) as usize;
            ShapeType::Ellipse { rx, ry, n_points }
        }
        "rectangle" => {
            let width = params.get("width")
                .and_then(|v| v.as_real())
                .unwrap_or(2.0);
            let height = params.get("height")
                .and_then(|v| v.as_real())
                .unwrap_or(1.0);
            ShapeType::Rectangle { width, height }
        }
        "polygon" => {
            let n_sides = params.get("n_sides")
                .and_then(|v| v.as_integer())
                .unwrap_or(5) as usize;
            let radius = params.get("radius")
                .and_then(|v| v.as_real())
                .unwrap_or(1.0);
            ShapeType::Polygon { n_sides, radius }
        }
        "star" => {
            let n_points = params.get("n_points")
                .and_then(|v| v.as_integer())
                .unwrap_or(5) as usize;
            let outer_radius = params.get("outer_radius")
                .and_then(|v| v.as_real())
                .unwrap_or(1.0);
            let inner_radius = params.get("inner_radius")
                .and_then(|v| v.as_real())
                .unwrap_or(0.4);
            ShapeType::Star { n_points, outer_radius, inner_radius }
        }
        _ => ShapeType::Circle { radius: 1.0, n_points: 100 },
    };

    // Generate vertices
    let vertices = shape.generate_vertices();

    // Apply center offset if provided
    let final_vertices = if let Some(center) = params.get("center") {
        if let Some(center_vec) = center.as_real_slice() {
            if center_vec.len() >= 2 {
                let cx = center_vec[0];
                let cy = center_vec[1];
                vertices
                    .into_iter()
                    .map(|v| Vector2::new(v.x + cx, v.y + cy))
                    .collect()
            } else {
                vertices
            }
        } else {
            vertices
        }
    } else {
        vertices
    };

    // Convert to R matrix (2 x n)
    let n = final_vertices.len();
    let mut x_coords = Vec::with_capacity(n);
    let mut y_coords = Vec::with_capacity(n);

    for v in final_vertices {
        x_coords.push(v.x);
        y_coords.push(v.y);
    }

    let result = List::from_values(&[
        r!(x_coords),
        r!(y_coords),
    ]);

    result.into()
}

/// Transform vertices (Rust Backend)
/// @export
#[extendr]
fn rust_transform_vertices(vertices: &[f64], transform: &[f64]) -> Vec<f64> {
    if transform.len() < 9 {
        return vertices.to_vec();
    }

    let n = vertices.len() / 2;
    let mut result = Vec::with_capacity(vertices.len());

    let m = Matrix3::new(
        transform[0], transform[3], transform[6],
        transform[1], transform[4], transform[7],
        transform[2], transform[5], transform[8],
    );

    for i in 0..n {
        let x = vertices[i * 2];
        let y = vertices[i * 2 + 1];
        let v = Vector3::new(x, y, 1.0);
        let transformed = m * v;

        let w = transformed.z;
        if w.abs() > 1e-10 {
            result.push(transformed.x / w);
            result.push(transformed.y / w);
        } else {
            result.push(transformed.x);
            result.push(transformed.y);
        }
    }

    result
}

/// Batch transform vertices (Rust Backend)
/// @export
#[extendr]
fn rust_batch_transform_vertices(
    vertices: &[f64],
    translations: &[f64],
    rotations: &[f64],
    scales: &[f64],
) -> Vec<f64> {
    let n_vertices = vertices.len() / 2;
    let n_frames = translations.len() / 2;

    let mut result = vec![0.0; n_vertices * 2 * n_frames];

    result.par_chunks_mut(n_vertices * 2)
        .enumerate()
        .for_each(|(f, frame_data)| {
            let tx = translations[f * 2];
            let ty = translations[f * 2 + 1];
            let angle = rotations[f];
            let sx = scales[f * 2];
            let sy = scales[f * 2 + 1];

            let c = angle.cos();
            let s = angle.sin();

            let m00 = c * sx;
            let m01 = -s * sy;
            let m10 = s * sx;
            let m11 = c * sy;

            for v in 0..n_vertices {
                let x = vertices[v * 2];
                let y = vertices[v * 2 + 1];

                frame_data[v * 2] = m00 * x + m01 * y + tx;
                frame_data[v * 2 + 1] = m10 * x + m11 * y + ty;
            }
        });

    result
}

/// Clear geometry cache
/// @export
#[extendr]
fn rust_clear_geometry_cache() {
    if let Ok(mut cache) = GEOMETRY_CACHE.lock() {
        cache.clear();
    }
}

/// Calculate animation progress for batch of animations
/// @export
#[extendr]
fn rust_calculate_animation_progress(
    times: &[f64],
    start_times: &[f64],
    durations: &[f64],
    easing_names: &[&str],
) -> List {
    let n_anims = start_times.len();
    let n_times = times.len();

    let results: Vec<Vec<f64>> = (0..n_anims)
        .into_par_iter()
        .map(|a| {
            let start = start_times[a];
            let duration = durations[a];
            let end = start + duration;
            let easing = EasingFunction::from_str(easing_names[a]);

            times
                .iter()
                .map(|&t| {
                    if t < start {
                        0.0
                    } else if t >= end {
                        1.0
                    } else {
                        let raw = (t - start) / duration;
                        easing.apply(raw.clamp(0.0, 1.0))
                    }
                })
                .collect()
        })
        .collect();

    List::from_values(results.iter().map(|v| r!(v.clone())))
}

/// Render frame batch (Rust Backend)
/// @export
#[extendr]
fn rust_render_frame_batch(
    object_data: List,
    animation_data: List,
    frame_times: &[f64],
    width: i32,
    height: i32,
) -> List {
    // Parse base objects
    let mut base_objects: Vec<render::ObjectState> = Vec::new();

    for obj in object_data.values() {
        if let Some(obj_list) = obj.as_list() {
            let state = render::ObjectState::from_list(&obj_list);
            base_objects.push(state);
        }
    }

    // Parse animations
    let mut animations: Vec<AnimationSpec> = Vec::new();

    for anim in animation_data.values() {
        if let Some(anim_list) = anim.as_list() {
            let spec = AnimationSpec::from_list(&anim_list);
            animations.push(spec);
        }
    }

    // Render frames in parallel
    let frame_results: Vec<List> = frame_times
        .par_iter()
        .map(|&time| {
            let mut frame_objects: HashMap<String, List> = HashMap::new();

            for obj in &base_objects {
                let mut frame_state = obj.clone();

                // Apply animations
                for anim in &animations {
                    if anim.object_id == obj.id {
                        frame_state.apply_animation(anim, time);
                    }
                }

                // Only include visible objects
                if frame_state.visible && frame_state.opacity > 0.001 {
                    frame_objects.insert(obj.id.clone(), frame_state.to_list());
                }
            }

            List::from_values(frame_objects.values().cloned())
        })
        .collect();

    List::from_values(frame_results)
}

/// Compute bounding box
/// @export
#[extendr]
fn rust_compute_bbox(points: &[f64]) -> List {
    if points.len() < 2 {
        return list!(xlim = r!([0.0, 0.0]), ylim = r!([0.0, 0.0]));
    }

    let n = points.len() / 2;

    let (min_x, max_x, min_y, max_y) = (1..n).fold(
        (points[0], points[0], points[1], points[1]),
        |(min_x, max_x, min_y, max_y), i| {
            let x = points[i * 2];
            let y = points[i * 2 + 1];
            (
                min_x.min(x),
                max_x.max(x),
                min_y.min(y),
                max_y.max(y),
            )
        },
    );

    list!(
        xlim = r!([min_x, max_x]),
        ylim = r!([min_y, max_y]),
    )
}

/// Generate arc
/// @export
#[extendr]
fn rust_generate_arc(radius: f64, start_angle: f64, end_angle: f64, n_points: i32) -> Robj {
    let n = n_points as usize;
    let angle_range = end_angle - start_angle;

    let vertices: Vec<Vector2<f64>> = (0..=n)
        .map(|i| {
            let theta = start_angle + angle_range * (i as f64) / (n as f64);
            Vector2::new(
                radius * theta.cos(),
                radius * theta.sin(),
            )
        })
        .collect();

    let x_coords: Vec<f64> = vertices.iter().map(|v| v.x).collect();
    let y_coords: Vec<f64> = vertices.iter().map(|v| v.y).collect();

    List::from_values(&[r!(x_coords), r!(y_coords)]).into()
}

/// Generate Bezier curve
/// @export
#[extendr]
fn rust_generate_bezier(
    p0_x: f64, p0_y: f64,
    p1_x: f64, p1_y: f64,
    p2_x: f64, p2_y: f64,
    p3_x: f64, p3_y: f64,
    n_segments: i32,
) -> Robj {
    let p0 = Vector2::new(p0_x, p0_y);
    let p1 = Vector2::new(p1_x, p1_y);
    let p2 = Vector2::new(p2_x, p2_y);
    let p3 = Vector2::new(p3_x, p3_y);

    let n = n_segments as usize;

    let vertices: Vec<Vector2<f64>> = (0..=n)
        .map(|i| {
            let t = (i as f64) / (n as f64);
            let u = 1.0 - t;

            // Cubic Bezier formula
            p0 * u * u * u +
            p1 * 3.0 * u * u * t +
            p2 * 3.0 * u * t * t +
            p3 * t * t * t
        })
        .collect();

    let x_coords: Vec<f64> = vertices.iter().map(|v| v.x).collect();
    let y_coords: Vec<f64> = vertices.iter().map(|v| v.y).collect();

    List::from_values(&[r!(x_coords), r!(y_coords)]).into()
}

/// Benchmark helper
/// @export
#[extendr]
fn rust_benchmark_easing(n: i32) -> f64 {
    let n = n as usize;
    let easing = EasingFunction::from_str("smooth");

    let start = std::time::Instant::now();

    let _: Vec<f64> = (0..n)
        .into_par_iter()
        .map(|i| {
            let t = (i as f64) / (n as f64);
            easing.apply(t)
        })
        .collect();

    start.elapsed().as_secs_f64()
}

// Macro to generate exports
extendr_module! {
    mod manimR_rust;
    fn rust_easing_vectorized;
    fn rust_lerp_vectorized;
    fn rust_color_interpolate;
    fn rust_generate_geometry;
    fn rust_transform_vertices;
    fn rust_batch_transform_vertices;
    fn rust_clear_geometry_cache;
    fn rust_calculate_animation_progress;
    fn rust_render_frame_batch;
    fn rust_compute_bbox;
    fn rust_generate_arc;
    fn rust_generate_bezier;
    fn rust_benchmark_easing;
}
