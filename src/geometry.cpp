// manimR C++ Backend - Geometry Generation and Operations
// Efficient shape tessellation and vertex transformations

#include "manimr_types.h"
#include <unordered_map>

using namespace manimr;

// ============================================================================
// Geometry Cache
// ============================================================================

// Simple LRU cache for geometry to avoid recomputing
class GeometryCache {
private:
    struct CacheEntry {
        std::vector<double> vertices;
        size_t access_count = 0;
    };

    std::unordered_map<std::string, CacheEntry> cache;
    size_t max_size = 1000;

public:
    std::vector<double> get(const std::string& key) {
        auto it = cache.find(key);
        if (it != cache.end()) {
            it->second.access_count++;
            return it->second.vertices;
        }
        return {};
    }

    void put(const std::string& key, const std::vector<double>& vertices) {
        if (cache.size() >= max_size) {
            // Evict least used entry
            auto min_it = cache.begin();
            for (auto it = cache.begin(); it != cache.end(); ++it) {
                if (it->second.access_count < min_it->second.access_count) {
                    min_it = it;
                }
            }
            cache.erase(min_it);
        }
        cache[key] = {vertices, 1};
    }

    void clear() {
        cache.clear();
    }
};

// Global geometry cache
static GeometryCache g_geometry_cache;

// ============================================================================
// Shape Generation
// ============================================================================

std::vector<double> generate_circle_vertices(double radius, int n_points, double center_x, double center_y) {
    std::vector<double> vertices;
    vertices.reserve(n_points * 2);

    for (int i = 0; i < n_points; ++i) {
        double theta = TWO_PI * i / n_points;
        vertices.push_back(center_x + radius * std::cos(theta));
        vertices.push_back(center_y + radius * std::sin(theta));
    }

    return vertices;
}

std::vector<double> generate_ellipse_vertices(double rx, double ry, int n_points, double center_x, double center_y) {
    std::vector<double> vertices;
    vertices.reserve(n_points * 2);

    for (int i = 0; i < n_points; ++i) {
        double theta = TWO_PI * i / n_points;
        vertices.push_back(center_x + rx * std::cos(theta));
        vertices.push_back(center_y + ry * std::sin(theta));
    }

    return vertices;
}

std::vector<double> generate_rectangle_vertices(double width, double height, double center_x, double center_y) {
    double w = width / 2.0;
    double h = height / 2.0;

    // 4 corners, 2 coordinates each = 8 values
    return {
        center_x - w, center_y - h,
        center_x + w, center_y - h,
        center_x + w, center_y + h,
        center_x - w, center_y + h
    };
}

std::vector<double> generate_rounded_rectangle_vertices(double width, double height,
                                                         double corner_radius, int n_corner_points,
                                                         double center_x, double center_y) {
    std::vector<double> vertices;

    double w = width / 2.0 - corner_radius;
    double h = height / 2.0 - corner_radius;

    // Generate 4 corners as arcs
    for (int corner = 0; corner < 4; ++corner) {
        double cx, cy, start_angle;
        switch (corner) {
            case 0: // Top-right
                cx = center_x + w; cy = center_y + h; start_angle = 0;
                break;
            case 1: // Top-left
                cx = center_x - w; cy = center_y + h; start_angle = PI / 2;
                break;
            case 2: // Bottom-left
                cx = center_x - w; cy = center_y - h; start_angle = PI;
                break;
            default: // Bottom-right
                cx = center_x + w; cy = center_y - h; start_angle = 3 * PI / 2;
        }

        for (int i = 0; i <= n_corner_points; ++i) {
            double theta = start_angle + (PI / 2) * i / n_corner_points;
            vertices.push_back(cx + corner_radius * std::cos(theta));
            vertices.push_back(cy + corner_radius * std::sin(theta));
        }
    }

    return vertices;
}

std::vector<double> generate_arc_vertices(double radius, double start_angle, double end_angle,
                                           int n_points, double center_x, double center_y) {
    std::vector<double> vertices;
    vertices.reserve((n_points + 1) * 2);

    double angle_range = end_angle - start_angle;

    for (int i = 0; i <= n_points; ++i) {
        double theta = start_angle + angle_range * i / n_points;
        vertices.push_back(center_x + radius * std::cos(theta));
        vertices.push_back(center_y + radius * std::sin(theta));
    }

    return vertices;
}

std::vector<double> generate_line_vertices(double x1, double y1, double x2, double y2) {
    return {x1, y1, x2, y2};
}

std::vector<double> generate_arrow_vertices(double x1, double y1, double x2, double y2,
                                             double head_length, double head_angle) {
    std::vector<double> vertices;

    // Shaft
    vertices.push_back(x1);
    vertices.push_back(y1);
    vertices.push_back(x2);
    vertices.push_back(y2);

    // Arrowhead
    double angle = std::atan2(y2 - y1, x2 - x1);
    double head_left = angle + PI - head_angle;
    double head_right = angle + PI + head_angle;

    vertices.push_back(x2 + head_length * std::cos(head_left));
    vertices.push_back(y2 + head_length * std::sin(head_left));
    vertices.push_back(x2);
    vertices.push_back(y2);
    vertices.push_back(x2 + head_length * std::cos(head_right));
    vertices.push_back(y2 + head_length * std::sin(head_right));

    return vertices;
}

std::vector<double> generate_polygon_vertices(int n_sides, double radius, double center_x, double center_y) {
    std::vector<double> vertices;
    vertices.reserve(n_sides * 2);

    for (int i = 0; i < n_sides; ++i) {
        double theta = TWO_PI * i / n_sides - PI / 2; // Start at top
        vertices.push_back(center_x + radius * std::cos(theta));
        vertices.push_back(center_y + radius * std::sin(theta));
    }

    return vertices;
}

std::vector<double> generate_star_vertices(int n_points, double outer_radius, double inner_radius,
                                            double center_x, double center_y) {
    std::vector<double> vertices;
    vertices.reserve(n_points * 4);

    for (int i = 0; i < n_points * 2; ++i) {
        double theta = PI * i / n_points - PI / 2;
        double r = (i % 2 == 0) ? outer_radius : inner_radius;
        vertices.push_back(center_x + r * std::cos(theta));
        vertices.push_back(center_y + r * std::sin(theta));
    }

    return vertices;
}

std::vector<double> generate_bezier_curve(const std::vector<double>& control_points, int n_segments) {
    std::vector<double> vertices;
    vertices.reserve((n_segments + 1) * 2);

    int n_control = control_points.size() / 2;

    for (int i = 0; i <= n_segments; ++i) {
        double t = static_cast<double>(i) / n_segments;

        // De Casteljau's algorithm
        std::vector<double> points = control_points;

        for (int level = 1; level < n_control; ++level) {
            for (int j = 0; j < (n_control - level) * 2; j += 2) {
                points[j] = (1 - t) * points[j] + t * points[j + 2];
                points[j + 1] = (1 - t) * points[j + 1] + t * points[j + 3];
            }
        }

        vertices.push_back(points[0]);
        vertices.push_back(points[1]);
    }

    return vertices;
}

// ============================================================================
// Transformation Functions
// ============================================================================

void transform_vertices_inplace(std::vector<double>& vertices, const Transform2D& transform) {
    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (size_t i = 0; i < vertices.size(); i += 2) {
        double x = vertices[i];
        double y = vertices[i + 1];

        Vec2 result = transform.apply({x, y});
        vertices[i] = result[0];
        vertices[i + 1] = result[1];
    }
}

// ============================================================================
// R Interface Functions
// ============================================================================

//' Generate Shape Geometry (C++ Backend)
//'
//' Generates vertex coordinates for various shapes.
//'
//' @param shape_type Shape type name
//' @param params List of shape parameters
//' @return Matrix of vertex coordinates (2 x n_vertices)
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix cpp_generate_geometry(std::string shape_type, List params) {
    std::vector<double> vertices;

    // Check cache first
    std::string cache_key = shape_type;
    if (params.containsElementNamed("radius")) {
        cache_key += "_r" + std::to_string(as<double>(params["radius"]));
    }
    if (params.containsElementNamed("n_points")) {
        cache_key += "_n" + std::to_string(as<int>(params["n_points"]));
    }

    vertices = g_geometry_cache.get(cache_key);

    if (vertices.empty()) {
        // Generate geometry
        if (shape_type == "circle") {
            double radius = params.containsElementNamed("radius") ? as<double>(params["radius"]) : 1.0;
            int n_points = params.containsElementNamed("n_points") ? as<int>(params["n_points"]) : 100;
            vertices = generate_circle_vertices(radius, n_points, 0.0, 0.0);
        }
        else if (shape_type == "ellipse") {
            double rx = params.containsElementNamed("rx") ? as<double>(params["rx"]) : 1.0;
            double ry = params.containsElementNamed("ry") ? as<double>(params["ry"]) : 0.5;
            int n_points = params.containsElementNamed("n_points") ? as<int>(params["n_points"]) : 100;
            vertices = generate_ellipse_vertices(rx, ry, n_points, 0.0, 0.0);
        }
        else if (shape_type == "rectangle") {
            double width = params.containsElementNamed("width") ? as<double>(params["width"]) : 2.0;
            double height = params.containsElementNamed("height") ? as<double>(params["height"]) : 1.0;
            vertices = generate_rectangle_vertices(width, height, 0.0, 0.0);
        }
        else if (shape_type == "rounded_rectangle") {
            double width = params.containsElementNamed("width") ? as<double>(params["width"]) : 2.0;
            double height = params.containsElementNamed("height") ? as<double>(params["height"]) : 1.0;
            double corner_radius = params.containsElementNamed("corner_radius") ? as<double>(params["corner_radius"]) : 0.1;
            int n_corner = params.containsElementNamed("n_corner") ? as<int>(params["n_corner"]) : 10;
            vertices = generate_rounded_rectangle_vertices(width, height, corner_radius, n_corner, 0.0, 0.0);
        }
        else if (shape_type == "polygon") {
            int n_sides = params.containsElementNamed("n_sides") ? as<int>(params["n_sides"]) : 5;
            double radius = params.containsElementNamed("radius") ? as<double>(params["radius"]) : 1.0;
            vertices = generate_polygon_vertices(n_sides, radius, 0.0, 0.0);
        }
        else if (shape_type == "star") {
            int n_points = params.containsElementNamed("n_points") ? as<int>(params["n_points"]) : 5;
            double outer_r = params.containsElementNamed("outer_radius") ? as<double>(params["outer_radius"]) : 1.0;
            double inner_r = params.containsElementNamed("inner_radius") ? as<double>(params["inner_radius"]) : 0.4;
            vertices = generate_star_vertices(n_points, outer_r, inner_r, 0.0, 0.0);
        }

        // Cache the result
        g_geometry_cache.put(cache_key, vertices);
    }

    // Apply position offset if provided
    if (params.containsElementNamed("center")) {
        NumericVector center = params["center"];
        double cx = center[0];
        double cy = center[1];

        #ifdef _OPENMP
        #pragma omp parallel for schedule(static)
        #endif
        for (size_t i = 0; i < vertices.size(); i += 2) {
            vertices[i] += cx;
            vertices[i + 1] += cy;
        }
    }

    // Convert to R matrix (2 x n_vertices)
    int n_vertices = vertices.size() / 2;
    NumericMatrix result(2, n_vertices);

    for (int i = 0; i < n_vertices; ++i) {
        result(0, i) = vertices[i * 2];
        result(1, i) = vertices[i * 2 + 1];
    }

    return result;
}

//' Transform Vertices (C++ Backend)
//'
//' Applies a 2D transformation matrix to a set of vertices.
//'
//' @param vertices 2 x N matrix of vertex coordinates
//' @param transform 3x3 transformation matrix (homogeneous coordinates)
//' @return Transformed vertices (2 x N matrix)
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix cpp_transform_vertices(NumericMatrix vertices, NumericMatrix transform) {
    int n_vertices = vertices.ncol();
    NumericMatrix result(2, n_vertices);

    // Extract transformation matrix elements
    double m00 = transform(0, 0), m01 = transform(0, 1), m02 = transform(0, 2);
    double m10 = transform(1, 0), m11 = transform(1, 1), m12 = transform(1, 2);
    double m20 = transform(2, 0), m21 = transform(2, 1), m22 = transform(2, 2);

    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (int i = 0; i < n_vertices; ++i) {
        double x = vertices(0, i);
        double y = vertices(1, i);

        double xp = m00 * x + m01 * y + m02;
        double yp = m10 * x + m11 * y + m12;
        double wp = m20 * x + m21 * y + m22;

        result(0, i) = xp / wp;
        result(1, i) = yp / wp;
    }

    return result;
}

//' Batch Transform Vertices (C++ Backend)
//'
//' Applies multiple transformations to vertices in batch.
//' Useful for animating object positions across frames.
//'
//' @param vertices 2 x N matrix of base vertex coordinates
//' @param translations N_frames x 2 matrix of translations
//' @param rotations N_frames vector of rotation angles
//' @param scales N_frames x 2 matrix of scale factors
//' @return 3D array of transformed vertices (2 x N x N_frames)
//' @keywords internal
// [[Rcpp::export]]
NumericVector cpp_batch_transform_vertices(NumericMatrix vertices,
                                            NumericMatrix translations,
                                            NumericVector rotations,
                                            NumericMatrix scales) {
    int n_vertices = vertices.ncol();
    int n_frames = translations.nrow();

    // Result as 3D array: dimensions [2, n_vertices, n_frames]
    NumericVector result(Dimension(2, n_vertices, n_frames));

    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic)
    #endif
    for (int f = 0; f < n_frames; ++f) {
        // Build transformation matrix
        double tx = translations(f, 0);
        double ty = translations(f, 1);
        double angle = rotations[f];
        double sx = scales(f, 0);
        double sy = scales(f, 1);

        double c = std::cos(angle);
        double s = std::sin(angle);

        // Combined: translate * rotate * scale
        double m00 = c * sx, m01 = -s * sy, m02 = tx;
        double m10 = s * sx, m11 = c * sy, m12 = ty;

        for (int v = 0; v < n_vertices; ++v) {
            double x = vertices(0, v);
            double y = vertices(1, v);

            result[2 * n_vertices * f + 2 * v] = m00 * x + m01 * y + m02;
            result[2 * n_vertices * f + 2 * v + 1] = m10 * x + m11 * y + m12;
        }
    }

    return result;
}

//' Clear Geometry Cache
//'
//' Clears the internal geometry cache to free memory.
//'
//' @keywords internal
// [[Rcpp::export]]
void cpp_clear_geometry_cache() {
    g_geometry_cache.clear();
}

//' Generate Arc Path
//'
//' Generates points along a circular arc.
//'
//' @param radius Arc radius
//' @param start_angle Start angle in radians
//' @param end_angle End angle in radians
//' @param n_points Number of points to generate
//' @return 2 x n_points matrix
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix cpp_generate_arc(double radius, double start_angle, double end_angle, int n_points) {
    std::vector<double> vertices = generate_arc_vertices(radius, start_angle, end_angle, n_points, 0.0, 0.0);

    NumericMatrix result(2, n_points + 1);
    for (int i = 0; i <= n_points; ++i) {
        result(0, i) = vertices[i * 2];
        result(1, i) = vertices[i * 2 + 1];
    }

    return result;
}

//' Generate Bezier Curve
//'
//' Generates points along a cubic Bezier curve.
//'
//' @param p0 Start point (x, y)
//' @param p1 Control point 1 (x, y)
//' @param p2 Control point 2 (x, y)
//' @param p3 End point (x, y)
//' @param n_segments Number of segments
//' @return 2 x (n_segments+1) matrix
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix cpp_generate_bezier(NumericVector p0, NumericVector p1,
                                   NumericVector p2, NumericVector p3,
                                   int n_segments) {
    std::vector<double> control_points = {
        p0[0], p0[1],
        p1[0], p1[1],
        p2[0], p2[1],
        p3[0], p3[1]
    };

    std::vector<double> vertices = generate_bezier_curve(control_points, n_segments);

    NumericMatrix result(2, n_segments + 1);
    for (int i = 0; i <= n_segments; ++i) {
        result(0, i) = vertices[i * 2];
        result(1, i) = vertices[i * 2 + 1];
    }

    return result;
}
