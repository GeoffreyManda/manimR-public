// manimR C++ Backend - Core Rendering Engine
// Parallel frame generation and object state computation

#include "manimr_types.h"
#include <functional>

using namespace manimr;

// ============================================================================
// Object State Structure
// ============================================================================

struct ObjectState {
    std::string id;
    std::string type;
    Vec3 position;
    double rotation;
    double scale;
    ColorRGB color;
    double opacity;
    bool visible;

    // Type-specific data
    std::string text;
    double font_size;
    std::string font_family;

    std::string latex;

    std::string shape_type;
    double width;
    double height;
    double radius;
    ColorRGB fill_color;
    double fill_opacity;
    double stroke_width;
};

struct AnimationSpec {
    std::string type;
    std::string object_id;
    double start_time;
    double duration;
    std::string easing_name;

    // Animation-specific parameters
    Vec3 target_position;
    Vec3 shift_vector;
    double target_scale;
    double rotation_angle;
    ColorRGB target_color;
};

// ============================================================================
// Frame Rendering
// ============================================================================

void apply_animation(ObjectState& state, const AnimationSpec& anim, double time) {
    double start = anim.start_time;
    double end = start + anim.duration;

    if (time < start) return;

    double progress = (time >= end) ? 1.0 : (time - start) / anim.duration;

    EasingFunc easing = get_easing_function(anim.easing_name);
    progress = easing(clamp(progress, 0.0, 1.0));

    if (anim.type == "show") {
        state.visible = true;
        state.opacity = 1.0;
    }
    else if (anim.type == "write_in") {
        state.visible = true;
        state.opacity = progress;
    }
    else if (anim.type == "fade_in") {
        state.visible = true;
        state.opacity = progress;
    }
    else if (anim.type == "fade_out") {
        state.opacity = 1.0 - progress;
        if (progress >= 1.0) state.visible = false;
    }
    else if (anim.type == "grow_from_center") {
        state.visible = true;
        state.scale = progress;
        state.opacity = 1.0;
    }
    else if (anim.type == "shrink_to_center") {
        state.scale = 1.0 - progress;
        if (progress >= 1.0) state.visible = false;
    }
    else if (anim.type == "move_to") {
        state.position = lerp_vec3(state.position, anim.target_position, progress);
    }
    else if (anim.type == "shift") {
        state.position[0] += anim.shift_vector[0] * progress;
        state.position[1] += anim.shift_vector[1] * progress;
        state.position[2] += anim.shift_vector[2] * progress;
    }
    else if (anim.type == "rotate") {
        state.rotation += anim.rotation_angle * progress;
    }
    else if (anim.type == "scale") {
        state.scale = lerp(1.0, anim.target_scale, progress);
    }
    else if (anim.type == "change_color") {
        state.color = lerp_color(state.color, anim.target_color, progress);
    }
}

// ============================================================================
// R Interface - Frame Batch Rendering
// ============================================================================

//' Render Frame Batch (C++ Backend)
//'
//' Renders multiple frames in parallel using OpenMP.
//'
//' @param object_data List of object data frames
//' @param animation_data List of animation specifications
//' @param frame_times NumericVector of frame times
//' @param width Frame width
//' @param height Frame height
//' @return List of frame states
//' @keywords internal
// [[Rcpp::export]]
List cpp_render_frame_batch(List object_data, List animation_data,
                             NumericVector frame_times,
                             int width, int height) {
    int n_frames = frame_times.size();
    int n_objects = object_data.size();
    int n_animations = animation_data.size();

    // Parse animations
    std::vector<AnimationSpec> anims;
    for (int i = 0; i < n_animations; ++i) {
        List anim_list = animation_data[i];
        AnimationSpec anim;
        anim.type = as<std::string>(anim_list["type"]);
        anim.object_id = as<std::string>(anim_list["object_id"]);
        anim.start_time = as<double>(anim_list["start_time"]);
        anim.duration = as<double>(anim_list["duration"]);
        anim.easing_name = as<std::string>(anim_list["easing"]);

        if (anim_list.containsElementNamed("target_position")) {
            NumericVector pos = anim_list["target_position"];
            anim.target_position = {pos[0], pos[1], pos.size() > 2 ? pos[2] : 0.0};
        }
        if (anim_list.containsElementNamed("shift_vector")) {
            NumericVector shift = anim_list["shift_vector"];
            anim.shift_vector = {shift[0], shift[1], shift.size() > 2 ? shift[2] : 0.0};
        }
        if (anim_list.containsElementNamed("factor")) {
            anim.target_scale = as<double>(anim_list["factor"]);
        }
        if (anim_list.containsElementNamed("angle")) {
            anim.rotation_angle = as<double>(anim_list["angle"]);
        }
        if (anim_list.containsElementNamed("color")) {
            std::string color_hex = as<std::string>(anim_list["color"]);
            anim.target_color = hex_to_rgb(color_hex);
        }

        anims.push_back(anim);
    }

    // Parse base object states
    std::vector<ObjectState> base_objects;
    for (int i = 0; i < n_objects; ++i) {
        List obj = object_data[i];
        ObjectState state;
        state.id = as<std::string>(obj["id"]);
        state.type = as<std::string>(obj["type"]);

        NumericVector pos = obj["position"];
        state.position = {pos[0], pos[1], pos.size() > 2 ? pos[2] : 0.0};

        state.rotation = obj.containsElementNamed("rotation") ? as<double>(obj["rotation"]) : 0.0;
        state.scale = obj.containsElementNamed("scale") ? as<double>(obj["scale"]) : 1.0;

        std::string color_hex = as<std::string>(obj["color"]);
        state.color = hex_to_rgb(color_hex);

        state.opacity = obj.containsElementNamed("opacity") ? as<double>(obj["opacity"]) : 1.0;
        state.visible = false; // Start invisible

        // Type-specific
        if (state.type == "text" && obj.containsElementNamed("text")) {
            state.text = as<std::string>(obj["text"]);
            state.font_size = obj.containsElementNamed("size") ? as<double>(obj["size"]) : 48.0;
        }
        if (state.type == "latex" && obj.containsElementNamed("latex")) {
            state.latex = as<std::string>(obj["latex"]);
            state.font_size = obj.containsElementNamed("size") ? as<double>(obj["size"]) : 1.0;
        }
        if (state.type == "shape") {
            state.shape_type = obj.containsElementNamed("shape_type") ? as<std::string>(obj["shape_type"]) : "circle";
            state.radius = obj.containsElementNamed("radius") ? as<double>(obj["radius"]) : 0.5;
            state.width = obj.containsElementNamed("width") ? as<double>(obj["width"]) : 1.0;
            state.height = obj.containsElementNamed("height") ? as<double>(obj["height"]) : 1.0;
            state.stroke_width = obj.containsElementNamed("stroke_width") ? as<double>(obj["stroke_width"]) : 1.0;
        }

        base_objects.push_back(state);
    }

    // Compute frame states in parallel. This phase is pure C++: the R API
    // is single-threaded and must not be touched from OpenMP workers.
    std::vector<double> times(frame_times.begin(), frame_times.end());
    std::vector<std::vector<ObjectState>> all_frame_states(n_frames);

    #ifdef _OPENMP
    #pragma omp parallel for schedule(dynamic)
    #endif
    for (int f = 0; f < n_frames; ++f) {
        double time = times[f];

        // Copy base states for this frame
        std::vector<ObjectState> frame_states = base_objects;

        // Apply animations
        for (const auto& anim : anims) {
            for (auto& state : frame_states) {
                if (state.id == anim.object_id) {
                    apply_animation(state, anim, time);
                }
            }
        }

        all_frame_states[f] = std::move(frame_states);
    }

    // Convert to R lists serially on the main thread
    List frame_results(n_frames);
    for (int f = 0; f < n_frames; ++f) {
        List frame_objects;
        for (const auto& state : all_frame_states[f]) {
            if (state.visible && state.opacity > 0.001) {
                List obj_state;
                obj_state["id"] = state.id;
                obj_state["type"] = state.type;
                obj_state["position"] = NumericVector::create(state.position[0], state.position[1], state.position[2]);
                obj_state["rotation"] = state.rotation;
                obj_state["scale"] = state.scale;
                obj_state["color"] = rgb_to_hex(state.color);
                obj_state["opacity"] = state.opacity;
                obj_state["visible"] = state.visible;

                if (state.type == "text") {
                    obj_state["text"] = state.text;
                    obj_state["font_size"] = state.font_size;
                }
                if (state.type == "latex") {
                    obj_state["latex"] = state.latex;
                    obj_state["font_size"] = state.font_size;
                }
                if (state.type == "shape") {
                    obj_state["shape_type"] = state.shape_type;
                    obj_state["radius"] = state.radius;
                    obj_state["width"] = state.width;
                    obj_state["height"] = state.height;
                    obj_state["stroke_width"] = state.stroke_width;
                }

                frame_objects[state.id] = obj_state;
            }
        }
        frame_results[f] = frame_objects;
    }

    return frame_results;
}

//' Compute Bounding Box (C++ Backend)
//'
//' Computes the axis-aligned bounding box for a set of points.
//'
//' @param points 2 x N matrix of points
//' @return List with xlim and ylim
//' @keywords internal
// [[Rcpp::export]]
List cpp_compute_bbox(NumericMatrix points) {
    int n = points.ncol();

    if (n == 0) {
        return List::create(
            _["xlim"] = NumericVector::create(0, 0),
            _["ylim"] = NumericVector::create(0, 0)
        );
    }

    double min_x = points(0, 0), max_x = points(0, 0);
    double min_y = points(1, 0), max_y = points(1, 0);

    for (int i = 1; i < n; ++i) {
        min_x = std::min(min_x, points(0, i));
        max_x = std::max(max_x, points(0, i));
        min_y = std::min(min_y, points(1, i));
        max_y = std::max(max_y, points(1, i));
    }

    return List::create(
        _["xlim"] = NumericVector::create(min_x, max_x),
        _["ylim"] = NumericVector::create(min_y, max_y)
    );
}

//' Parallel Frame Interpolation
//'
//' Interpolates between keyframes for smooth animation.
//'
//' @param keyframes List of keyframe states
//' @param key_times NumericVector of keyframe times
//' @param target_times NumericVector of target times
//' @param easing Easing function name
//' @return List of interpolated states
//' @keywords internal
// [[Rcpp::export]]
List cpp_interpolate_keyframes(List keyframes, NumericVector key_times,
                                NumericVector target_times, std::string easing) {
    int n_keys = key_times.size();
    int n_targets = target_times.size();

    EasingFunc ease_fn = get_easing_function(easing);
    List result(n_targets);

    #ifdef _OPENMP
    #pragma omp parallel for schedule(static)
    #endif
    for (int i = 0; i < n_targets; ++i) {
        double t = target_times[i];

        // Find surrounding keyframes
        int k1 = 0, k2 = n_keys - 1;
        for (int k = 0; k < n_keys - 1; ++k) {
            if (t >= key_times[k] && t <= key_times[k + 1]) {
                k1 = k;
                k2 = k + 1;
                break;
            }
        }

        // Interpolation factor
        double local_t;
        if (key_times[k2] == key_times[k1]) {
            local_t = 0.0;
        } else {
            local_t = (t - key_times[k1]) / (key_times[k2] - key_times[k1]);
        }
        local_t = ease_fn(clamp(local_t, 0.0, 1.0));

        // Return interpolated frame (simplified - would merge keyframe data)
        #ifdef _OPENMP
        #pragma omp critical
        #endif
        {
            if (local_t < 0.5) {
                result[i] = keyframes[k1];
            } else {
                result[i] = keyframes[k2];
            }
        }
    }

    return result;
}

//' Optimize Animation Timeline
//'
//' Analyzes and optimizes animation timeline for efficient rendering.
//'
//' @param animations List of animation specifications
//' @return Optimized timeline information
//' @keywords internal
// [[Rcpp::export]]
List cpp_optimize_timeline(List animations) {
    int n = animations.size();

    double total_duration = 0.0;
    std::map<std::string, int> type_counts;

    for (int i = 0; i < n; ++i) {
        List anim = animations[i];
        double start = as<double>(anim["start_time"]);
        double duration = as<double>(anim["duration"]);
        total_duration = std::max(total_duration, start + duration);

        std::string type = as<std::string>(anim["type"]);
        type_counts[type]++;
    }

    return List::create(
        _["total_duration"] = total_duration,
        _["n_animations"] = n,
        _["type_counts"] = wrap(type_counts)
    );
}
