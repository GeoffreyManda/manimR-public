// manimR C++ Backend - Animation Mathematics
// High-performance easing and interpolation functions with OpenMP parallelization

#include "manimr_types.h"

using namespace manimr;

// ============================================================================
// Easing Functions (in manimr namespace)
// ============================================================================

namespace manimr {

inline double ease_linear(double t) {
    return t;
}

inline double ease_smooth(double t) {
    // manim's smooth: t^2 * (3 - 2*t)
    return t * t * (3.0 - 2.0 * t);
}

inline double ease_in_quad(double t) {
    return t * t;
}

inline double ease_out_quad(double t) {
    double u = 1.0 - t;
    return 1.0 - u * u;
}

inline double ease_in_out_quad(double t) {
    if (t < 0.5) {
        return 2.0 * t * t;
    } else {
        double u = -2.0 * t + 2.0;
        return 1.0 - u * u / 2.0;
    }
}

inline double ease_in_cubic(double t) {
    return t * t * t;
}

inline double ease_out_cubic(double t) {
    double u = 1.0 - t;
    return 1.0 - u * u * u;
}

inline double ease_in_out_cubic(double t) {
    if (t < 0.5) {
        return 4.0 * t * t * t;
    } else {
        double u = -2.0 * t + 2.0;
        return 1.0 - u * u * u / 2.0;
    }
}

inline double ease_in_sine(double t) {
    return 1.0 - std::cos(t * PI / 2.0);
}

inline double ease_out_sine(double t) {
    return std::sin(t * PI / 2.0);
}

inline double ease_in_out_sine(double t) {
    return -(std::cos(PI * t) - 1.0) / 2.0;
}

inline double ease_in_expo(double t) {
    return t == 0.0 ? 0.0 : std::pow(2.0, 10.0 * (t - 1.0));
}

inline double ease_out_expo(double t) {
    return t == 1.0 ? 1.0 : 1.0 - std::pow(2.0, -10.0 * t);
}

inline double ease_in_out_expo(double t) {
    if (t == 0.0) return 0.0;
    if (t == 1.0) return 1.0;
    if (t < 0.5) {
        return std::pow(2.0, 20.0 * t - 10.0) / 2.0;
    } else {
        return (2.0 - std::pow(2.0, -20.0 * t + 10.0)) / 2.0;
    }
}

inline double ease_in_circ(double t) {
    return 1.0 - std::sqrt(1.0 - t * t);
}

inline double ease_out_circ(double t) {
    double u = t - 1.0;
    return std::sqrt(1.0 - u * u);
}

inline double ease_in_back(double t) {
    const double c1 = 1.70158;
    const double c3 = c1 + 1.0;
    return c3 * t * t * t - c1 * t * t;
}

inline double ease_out_back(double t) {
    const double c1 = 1.70158;
    const double c3 = c1 + 1.0;
    double u = t - 1.0;
    return 1.0 + c3 * u * u * u + c1 * u * u;
}

inline double ease_in_out_back(double t) {
    const double c1 = 1.70158;
    const double c2 = c1 * 1.525;
    if (t < 0.5) {
        return (std::pow(2.0 * t, 2.0) * ((c2 + 1.0) * 2.0 * t - c2)) / 2.0;
    } else {
        double u = 2.0 * t - 2.0;
        return (std::pow(u, 2.0) * ((c2 + 1.0) * u + c2) + 2.0) / 2.0;
    }
}

inline double ease_out_bounce(double t) {
    const double n1 = 7.5625;
    const double d1 = 2.75;

    if (t < 1.0 / d1) {
        return n1 * t * t;
    } else if (t < 2.0 / d1) {
        double u = t - 1.5 / d1;
        return n1 * u * u + 0.75;
    } else if (t < 2.5 / d1) {
        double u = t - 2.25 / d1;
        return n1 * u * u + 0.9375;
    } else {
        double u = t - 2.625 / d1;
        return n1 * u * u + 0.984375;
    }
}

inline double ease_in_bounce(double t) {
    return 1.0 - ease_out_bounce(1.0 - t);
}

inline double ease_in_elastic(double t) {
    const double c4 = (2.0 * PI) / 3.0;
    if (t == 0.0) return 0.0;
    if (t == 1.0) return 1.0;
    return -std::pow(2.0, 10.0 * t - 10.0) * std::sin((t * 10.0 - 10.75) * c4);
}

inline double ease_out_elastic(double t) {
    const double c4 = (2.0 * PI) / 3.0;
    if (t == 0.0) return 0.0;
    if (t == 1.0) return 1.0;
    return std::pow(2.0, -10.0 * t) * std::sin((t * 10.0 - 0.75) * c4) + 1.0;
}

EasingFunc get_easing_function(const std::string& name) {
    if (name == "linear") return ease_linear;
    if (name == "smooth") return ease_smooth;
    if (name == "in_quad") return ease_in_quad;
    if (name == "out_quad") return ease_out_quad;
    if (name == "in_out_quad") return ease_in_out_quad;
    if (name == "in_cubic") return ease_in_cubic;
    if (name == "out_cubic") return ease_out_cubic;
    if (name == "in_out_cubic") return ease_in_out_cubic;
    if (name == "in_sine") return ease_in_sine;
    if (name == "out_sine") return ease_out_sine;
    if (name == "in_out_sine") return ease_in_out_sine;
    if (name == "in_expo") return ease_in_expo;
    if (name == "out_expo") return ease_out_expo;
    if (name == "in_out_expo") return ease_in_out_expo;
    if (name == "in_circ") return ease_in_circ;
    if (name == "out_circ") return ease_out_circ;
    if (name == "in_back") return ease_in_back;
    if (name == "out_back") return ease_out_back;
    if (name == "in_out_back") return ease_in_out_back;
    if (name == "in_bounce") return ease_in_bounce;
    if (name == "out_bounce") return ease_out_bounce;
    if (name == "in_elastic") return ease_in_elastic;
    if (name == "out_elastic") return ease_out_elastic;
    return ease_smooth; // default
}

} // namespace manimr

// ============================================================================
// R Interface Functions (global scope for Rcpp export)
// ============================================================================

//' Vectorized Easing Function (C++ Backend)
//'
//' Applies an easing function to a vector of time values.
//' Uses OpenMP for parallelization when available.
//'
//' @param t Numeric vector of time values in \[0, 1\]
//' @param easing_name Name of easing function
//' @return Numeric vector of eased values
//' @keywords internal
// [[Rcpp::export]]
NumericVector cpp_easing_vectorized(NumericVector t, std::string easing_name) {
    manimr::EasingFunc easing = manimr::get_easing_function(easing_name);
    int n = t.size();
    NumericVector result(n);

    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (int i = 0; i < n; ++i) {
        result[i] = easing(manimr::clamp(t[i], 0.0, 1.0));
    }

    return result;
}

//' Vectorized Linear Interpolation (C++ Backend)
//'
//' Performs linear interpolation between start and end values.
//'
//' @param start Start value(s)
//' @param end End value(s)
//' @param t Interpolation parameter(s) in \[0, 1\]
//' @return Interpolated value(s)
//' @keywords internal
// [[Rcpp::export]]
NumericVector cpp_lerp_vectorized(NumericVector start, NumericVector end, NumericVector t) {
    int n = std::max({start.size(), end.size(), t.size()});
    NumericVector result(n);

    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (int i = 0; i < n; ++i) {
        double s = start[i % start.size()];
        double e = end[i % end.size()];
        double ti = manimr::clamp(t[i % t.size()], 0.0, 1.0);
        result[i] = s + (e - s) * ti;
    }

    return result;
}

//' Color Interpolation (C++ Backend)
//'
//' Interpolates between two colors in RGB space.
//'
//' @param color1 First color as hex string
//' @param color2 Second color as hex string
//' @param t Interpolation parameter(s) in \[0, 1\]
//' @return Hex color string(s)
//' @keywords internal
// [[Rcpp::export]]
CharacterVector cpp_color_interpolate(std::string color1, std::string color2, NumericVector t) {
    manimr::ColorRGB c1 = manimr::hex_to_rgb(color1);
    manimr::ColorRGB c2 = manimr::hex_to_rgb(color2);
    int n = t.size();
    CharacterVector result(n);

    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (int i = 0; i < n; ++i) {
        manimr::ColorRGB interpolated = manimr::lerp_color(c1, c2, manimr::clamp(t[i], 0.0, 1.0));
        result[i] = manimr::rgb_to_hex(interpolated);
    }

    return result;
}

//' Batch Ease Calculation for Animations
//'
//' Calculates eased progress values for multiple animation keyframes.
//' Optimized for the animation timeline computation.
//'
//' @param times NumericVector of times
//' @param start_times NumericVector of animation start times
//' @param durations NumericVector of animation durations
//' @param easing_names CharacterVector of easing function names
//' @return List containing progress values for each animation
//' @keywords internal
// [[Rcpp::export]]
List cpp_calculate_animation_progress(NumericVector times,
                                       NumericVector start_times,
                                       NumericVector durations,
                                       CharacterVector easing_names) {
    int n_times = times.size();
    int n_anims = start_times.size();

    List result(n_anims);

    for (int a = 0; a < n_anims; ++a) {
        double start = start_times[a];
        double duration = durations[a];
        double end = start + duration;
        manimr::EasingFunc easing = manimr::get_easing_function(as<std::string>(easing_names[a]));

        NumericVector progress(n_times);

        #ifdef _OPENMP
        #pragma omp parallel for schedule(static)
        #endif
        for (int i = 0; i < n_times; ++i) {
            double t = times[i];
            if (t < start) {
                progress[i] = 0.0;
            } else if (t >= end) {
                progress[i] = 1.0;
            } else {
                double raw_progress = (t - start) / duration;
                progress[i] = easing(manimr::clamp(raw_progress, 0.0, 1.0));
            }
        }

        result[a] = progress;
    }

    return result;
}

//' Smooth Step Function (GPU-friendly)
//'
//' Hermite interpolation between two edges.
//'
//' @param edge0 Lower edge
//' @param edge1 Upper edge
//' @param x Input value
//' @return Smooth stepped value
//' @keywords internal
// [[Rcpp::export]]
NumericVector cpp_smoothstep(double edge0, double edge1, NumericVector x) {
    int n = x.size();
    NumericVector result(n);

    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (int i = 0; i < n; ++i) {
        double t = manimr::clamp((x[i] - edge0) / (edge1 - edge0), 0.0, 1.0);
        result[i] = t * t * (3.0 - 2.0 * t);
    }

    return result;
}
