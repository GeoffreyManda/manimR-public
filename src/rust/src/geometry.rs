// manimR Rust Backend - Geometry Generation
// Shape tessellation and vertex operations

use nalgebra::Vector2;
use std::collections::HashMap;
use std::f64::consts::PI;

/// Shape type enumeration
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ShapeType {
    Circle { radius: i32, n_points: i32 },
    Ellipse { rx: i32, ry: i32, n_points: i32 },
    Rectangle { width: i32, height: i32 },
    RoundedRectangle { width: i32, height: i32, corner_radius: i32, n_corner: i32 },
    Polygon { n_sides: i32, radius: i32 },
    Star { n_points: i32, outer_radius: i32, inner_radius: i32 },
}

impl ShapeType {
    /// Generate vertices for the shape
    pub fn generate_vertices(&self) -> Vec<Vector2<f64>> {
        match *self {
            ShapeType::Circle { radius, n_points } => {
                Self::generate_circle(radius as f64, n_points as usize)
            }
            ShapeType::Ellipse { rx, ry, n_points } => {
                Self::generate_ellipse(rx as f64, ry as f64, n_points as usize)
            }
            ShapeType::Rectangle { width, height } => {
                Self::generate_rectangle(width as f64, height as f64)
            }
            ShapeType::RoundedRectangle { width, height, corner_radius, n_corner } => {
                Self::generate_rounded_rectangle(
                    width as f64,
                    height as f64,
                    corner_radius as f64,
                    n_corner as usize,
                )
            }
            ShapeType::Polygon { n_sides, radius } => {
                Self::generate_polygon(n_sides as usize, radius as f64)
            }
            ShapeType::Star { n_points, outer_radius, inner_radius } => {
                Self::generate_star(
                    n_points as usize,
                    outer_radius as f64,
                    inner_radius as f64,
                )
            }
        }
    }

    fn generate_circle(radius: f64, n_points: usize) -> Vec<Vector2<f64>> {
        (0..n_points)
            .map(|i| {
                let theta = 2.0 * PI * (i as f64) / (n_points as f64);
                Vector2::new(radius * theta.cos(), radius * theta.sin())
            })
            .collect()
    }

    fn generate_ellipse(rx: f64, ry: f64, n_points: usize) -> Vec<Vector2<f64>> {
        (0..n_points)
            .map(|i| {
                let theta = 2.0 * PI * (i as f64) / (n_points as f64);
                Vector2::new(rx * theta.cos(), ry * theta.sin())
            })
            .collect()
    }

    fn generate_rectangle(width: f64, height: f64) -> Vec<Vector2<f64>> {
        let w = width / 2.0;
        let h = height / 2.0;
        vec![
            Vector2::new(-w, -h),
            Vector2::new(w, -h),
            Vector2::new(w, h),
            Vector2::new(-w, h),
        ]
    }

    fn generate_rounded_rectangle(
        width: f64,
        height: f64,
        corner_radius: f64,
        n_corner: usize,
    ) -> Vec<Vector2<f64>> {
        let mut vertices = Vec::new();
        let w = width / 2.0 - corner_radius;
        let h = height / 2.0 - corner_radius;

        // Four corners
        for corner in 0..4 {
            let (cx, cy, start_angle) = match corner {
                0 => (w, h, 0.0),           // Top-right
                1 => (-w, h, PI / 2.0),     // Top-left
                2 => (-w, -h, PI),          // Bottom-left
                _ => (w, -h, 3.0 * PI / 2.0), // Bottom-right
            };

            for i in 0..=n_corner {
                let theta = start_angle + (PI / 2.0) * (i as f64) / (n_corner as f64);
                vertices.push(Vector2::new(
                    cx + corner_radius * theta.cos(),
                    cy + corner_radius * theta.sin(),
                ));
            }
        }

        vertices
    }

    fn generate_polygon(n_sides: usize, radius: f64) -> Vec<Vector2<f64>> {
        (0..n_sides)
            .map(|i| {
                let theta = 2.0 * PI * (i as f64) / (n_sides as f64) - PI / 2.0;
                Vector2::new(radius * theta.cos(), radius * theta.sin())
            })
            .collect()
    }

    fn generate_star(n_points: usize, outer_radius: f64, inner_radius: f64) -> Vec<Vector2<f64>> {
        (0..(n_points * 2))
            .map(|i| {
                let theta = PI * (i as f64) / (n_points as f64) - PI / 2.0;
                let r = if i % 2 == 0 { outer_radius } else { inner_radius };
                Vector2::new(r * theta.cos(), r * theta.sin())
            })
            .collect()
    }
}

/// Cache for shape geometry to avoid regeneration
pub struct VertexCache {
    cache: HashMap<ShapeType, Vec<Vector2<f64>>>,
    max_size: usize,
}

impl VertexCache {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            max_size: 1000,
        }
    }

    pub fn get(&self, shape: &ShapeType) -> Option<&Vec<Vector2<f64>>> {
        self.cache.get(shape)
    }

    pub fn get_or_insert(&mut self, shape: ShapeType) -> &Vec<Vector2<f64>> {
        if self.cache.len() >= self.max_size && !self.cache.contains_key(&shape) {
            // Simple eviction: clear half the cache
            let keys_to_remove: Vec<_> = self
                .cache
                .keys()
                .take(self.max_size / 2)
                .cloned()
                .collect();
            for key in keys_to_remove {
                self.cache.remove(&key);
            }
        }

        self.cache
            .entry(shape.clone())
            .or_insert_with(|| shape.generate_vertices())
    }

    pub fn clear(&mut self) {
        self.cache.clear();
    }
}

impl Default for VertexCache {
    fn default() -> Self {
        Self::new()
    }
}

/// Generate arc vertices
pub fn generate_arc(
    radius: f64,
    start_angle: f64,
    end_angle: f64,
    n_points: usize,
) -> Vec<Vector2<f64>> {
    let angle_range = end_angle - start_angle;

    (0..=n_points)
        .map(|i| {
            let theta = start_angle + angle_range * (i as f64) / (n_points as f64);
            Vector2::new(radius * theta.cos(), radius * theta.sin())
        })
        .collect()
}

/// Generate cubic Bezier curve
pub fn generate_bezier(
    p0: Vector2<f64>,
    p1: Vector2<f64>,
    p2: Vector2<f64>,
    p3: Vector2<f64>,
    n_segments: usize,
) -> Vec<Vector2<f64>> {
    (0..=n_segments)
        .map(|i| {
            let t = (i as f64) / (n_segments as f64);
            let u = 1.0 - t;

            // Cubic Bezier formula
            p0 * u * u * u + p1 * 3.0 * u * u * t + p2 * 3.0 * u * t * t + p3 * t * t * t
        })
        .collect()
}

/// Generate quadratic Bezier curve
pub fn generate_bezier_quadratic(
    p0: Vector2<f64>,
    p1: Vector2<f64>,
    p2: Vector2<f64>,
    n_segments: usize,
) -> Vec<Vector2<f64>> {
    (0..=n_segments)
        .map(|i| {
            let t = (i as f64) / (n_segments as f64);
            let u = 1.0 - t;

            // Quadratic Bezier formula
            p0 * u * u + p1 * 2.0 * u * t + p2 * t * t
        })
        .collect()
}

/// Apply transformation to vertices
pub fn transform_vertices(vertices: &[Vector2<f64>], matrix: &nalgebra::Matrix3<f64>) -> Vec<Vector2<f64>> {
    vertices
        .iter()
        .map(|v| {
            let v3 = Vector2::new(v.x, v.y).to_homogeneous();
            let transformed = matrix * v3;
            Vector2::new(transformed.x / transformed.z, transformed.y / transformed.z)
        })
        .collect()
}

/// Compute bounding box for vertices
pub fn compute_bbox(vertices: &[Vector2<f64>]) -> Option<(f64, f64, f64, f64)> {
    if vertices.is_empty() {
        return None;
    }

    let (min_x, max_x, min_y, max_y) = vertices.iter().skip(1).fold(
        (vertices[0].x, vertices[0].x, vertices[0].y, vertices[0].y),
        |(min_x, max_x, min_y, max_y), v| {
            (min_x.min(v.x), max_x.max(v.x), min_y.min(v.y), max_y.max(v.y))
        },
    );

    Some((min_x, max_x, min_y, max_y))
}

/// Compute centroid of vertices
pub fn compute_centroid(vertices: &[Vector2<f64>]) -> Option<Vector2<f64>> {
    if vertices.is_empty() {
        return None;
    }

    let sum = vertices.iter().fold(Vector2::new(0.0, 0.0), |acc, v| acc + v);
    Some(sum / (vertices.len() as f64))
}

/// Subdivide a polyline into segments of approximately equal length
pub fn subdivide_polyline(vertices: &[Vector2<f64>], n_segments: usize) -> Vec<Vector2<f64>> {
    if vertices.len() < 2 || n_segments == 0 {
        return vertices.to_vec();
    }

    // Calculate total length
    let total_length: f64 = vertices.windows(2).map(|w| (w[1] - w[0]).magnitude()).sum();

    let segment_length = total_length / (n_segments as f64);
    let mut result = vec![vertices[0]];
    let mut current_length = 0.0;
    let mut current_vertex_idx = 0;

    for _ in 1..n_segments {
        let target_length = current_length + segment_length;

        // Find the segment containing the target point
        while current_vertex_idx < vertices.len() - 1 {
            let segment_start = vertices[current_vertex_idx];
            let segment_end = vertices[current_vertex_idx + 1];
            let seg_len = (segment_end - segment_start).magnitude();

            if current_length + seg_len >= target_length {
                // Interpolate within this segment
                let t = (target_length - current_length) / seg_len;
                result.push(segment_start + (segment_end - segment_start) * t);
                current_length = target_length;
                break;
            }

            current_length += seg_len;
            current_vertex_idx += 1;
        }
    }

    result.push(*vertices.last().unwrap());
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_circle_vertices() {
        let vertices = ShapeType::generate_circle(1.0, 4);
        assert_eq!(vertices.len(), 4);
    }

    #[test]
    fn test_rectangle_vertices() {
        let vertices = ShapeType::generate_rectangle(2.0, 1.0);
        assert_eq!(vertices.len(), 4);
    }

    #[test]
    fn test_bbox() {
        let vertices = vec![
            Vector2::new(0.0, 0.0),
            Vector2::new(1.0, 0.0),
            Vector2::new(1.0, 1.0),
            Vector2::new(0.0, 1.0),
        ];
        let bbox = compute_bbox(&vertices).unwrap();
        assert_eq!(bbox, (0.0, 1.0, 0.0, 1.0));
    }

    #[test]
    fn test_centroid() {
        let vertices = vec![
            Vector2::new(0.0, 0.0),
            Vector2::new(2.0, 0.0),
            Vector2::new(2.0, 2.0),
            Vector2::new(0.0, 2.0),
        ];
        let centroid = compute_centroid(&vertices).unwrap();
        assert!((centroid.x - 1.0).abs() < 1e-10);
        assert!((centroid.y - 1.0).abs() < 1e-10);
    }
}
