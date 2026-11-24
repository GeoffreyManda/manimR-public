// manimR C++ Backend - Type Definitions and Utilities
// Provides common types, constants, and inline utility functions
// for high-performance mathematical animation computations

#ifndef MANIMR_TYPES_H
#define MANIMR_TYPES_H

#include <Rcpp.h>
#include <vector>
#include <array>
#include <cmath>
#include <algorithm>

// Conditionally include OpenMP
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// ============================================================================
// Constants
// ============================================================================

namespace manimr {

constexpr double PI = 3.14159265358979323846;
constexpr double TWO_PI = 2.0 * PI;
constexpr double EPSILON = 1e-10;

// ============================================================================
// Type Aliases
// ============================================================================

using Vec2 = std::array<double, 2>;
using Vec3 = std::array<double, 3>;
using ColorRGB = std::array<double, 3>;  // RGB in [0, 1]
using ColorRGBA = std::array<double, 4>; // RGBA in [0, 1]

// ============================================================================
// Inline Utility Functions
// ============================================================================

inline double clamp(double value, double min_val, double max_val) {
    return std::max(min_val, std::min(value, max_val));
}

inline double lerp(double start, double end, double t) {
    return start + (end - start) * t;
}

inline Vec2 lerp_vec2(const Vec2& start, const Vec2& end, double t) {
    return {lerp(start[0], end[0], t), lerp(start[1], end[1], t)};
}

inline Vec3 lerp_vec3(const Vec3& start, const Vec3& end, double t) {
    return {
        lerp(start[0], end[0], t),
        lerp(start[1], end[1], t),
        lerp(start[2], end[2], t)
    };
}

// ============================================================================
// Color Operations
// ============================================================================

inline ColorRGB hex_to_rgb(const std::string& hex) {
    ColorRGB rgb = {0.0, 0.0, 0.0};
    if (hex.length() >= 7 && hex[0] == '#') {
        int r = std::stoi(hex.substr(1, 2), nullptr, 16);
        int g = std::stoi(hex.substr(3, 2), nullptr, 16);
        int b = std::stoi(hex.substr(5, 2), nullptr, 16);
        rgb = {r / 255.0, g / 255.0, b / 255.0};
    }
    return rgb;
}

inline std::string rgb_to_hex(const ColorRGB& rgb) {
    char hex[8];
    std::snprintf(hex, sizeof(hex), "#%02X%02X%02X",
                  static_cast<unsigned int>(clamp(rgb[0], 0.0, 1.0) * 255) & 0xFFu,
                  static_cast<unsigned int>(clamp(rgb[1], 0.0, 1.0) * 255) & 0xFFu,
                  static_cast<unsigned int>(clamp(rgb[2], 0.0, 1.0) * 255) & 0xFFu);
    return std::string(hex);
}

inline ColorRGB lerp_color(const ColorRGB& c1, const ColorRGB& c2, double t) {
    return {
        lerp(c1[0], c2[0], t),
        lerp(c1[1], c2[1], t),
        lerp(c1[2], c2[2], t)
    };
}

// ============================================================================
// Transformation Matrix (3x3 for 2D homogeneous coordinates)
// ============================================================================

class Transform2D {
public:
    std::array<double, 9> m; // Column-major: [m00, m10, m20, m01, m11, m21, m02, m12, m22]

    Transform2D() : m({1, 0, 0, 0, 1, 0, 0, 0, 1}) {}

    static Transform2D identity() { return Transform2D(); }

    static Transform2D translation(double tx, double ty) {
        Transform2D t;
        t.m[6] = tx;
        t.m[7] = ty;
        return t;
    }

    static Transform2D scale(double sx, double sy) {
        Transform2D t;
        t.m[0] = sx;
        t.m[4] = sy;
        return t;
    }

    static Transform2D rotation(double angle_rad) {
        Transform2D t;
        double c = std::cos(angle_rad);
        double s = std::sin(angle_rad);
        t.m[0] = c;  t.m[3] = -s;
        t.m[1] = s;  t.m[4] = c;
        return t;
    }

    Vec2 apply(const Vec2& v) const {
        double x = m[0] * v[0] + m[3] * v[1] + m[6];
        double y = m[1] * v[0] + m[4] * v[1] + m[7];
        double w = m[2] * v[0] + m[5] * v[1] + m[8];
        return {x / w, y / w};
    }

    Transform2D operator*(const Transform2D& other) const {
        Transform2D result;
        for (int i = 0; i < 3; ++i) {
            for (int j = 0; j < 3; ++j) {
                result.m[i + j*3] = 0;
                for (int k = 0; k < 3; ++k) {
                    result.m[i + j*3] += m[i + k*3] * other.m[k + j*3];
                }
            }
        }
        return result;
    }
};

// ============================================================================
// Easing Function Type and Declaration
// ============================================================================

// Function pointer type for easing functions
using EasingFunc = double (*)(double);

// Get easing function by name (implementation in animation_math.cpp)
EasingFunc get_easing_function(const std::string& name);

} // namespace manimr

#endif // MANIMR_TYPES_H
