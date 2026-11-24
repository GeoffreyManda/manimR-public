#' Vector Math Utilities
#'
#' @description
#' Core mathematical utilities for Bezier curves, vector transformations,
#' and geometric interpolation.
#' @name vector_utils
#' @keywords internal
NULL

#' Evaluate a point on a cubic Bezier curve
#'
#' @param p0 Start point
#' @param p1 Handle 1
#' @param p2 Handle 2
#' @param p3 End point
#' @param t Parameter \[0,1\]
#' @return Point on curve at t
#' @keywords internal
bezier <- function(p0, p1, p2, p3, t) {
  mt <- 1 - t
  (mt^3 * p0) + (3 * mt^2 * t * p1) + (3 * mt * t^2 * p2) + (t^3 * p3)
}

#' Sample points along Bezier curves
#'
#' @description Converts a matrix of Bezier control points into a dense
#'   polyline by evaluating each curve segment at evenly spaced parameters.
#'   Supports both 4-point-per-curve (disjoint) and shared-endpoint formats.
#'
#' @param points Matrix of control points (N x 3)
#' @param precision Number of samples per curve segment
#' @return Matrix of sampled points (M x ncol)
#' @keywords internal
sample_bezier <- function(points, precision = 10) {
  if (is.null(points) || nrow(points) < 4) return(matrix(numeric(0), ncol = 3))

  n_cols <- ncol(points)

  # Detect format: disjoint (4 per curve) or shared (3 per curve + 1 start)
  is_shared <- FALSE
  if (nrow(points) %% 4 == 0) {
    n_curves <- nrow(points) / 4
  } else if (nrow(points) %% 3 == 1) {
    n_curves <- (nrow(points) - 1) / 3
    is_shared <- TRUE
  } else {
    n_curves <- floor(nrow(points) / 4)
  }

  if (n_curves < 1) return(matrix(numeric(0), ncol = n_cols))

  # Pre-calculate Bernstein basis
  t_vals <- seq(0, 1, length.out = precision)
  mt <- 1 - t_vals
  B_mat <- cbind(mt^3, 3 * mt^2 * t_vals, 3 * mt * t_vals^2, t_vals^3)

  total_points <- vector("list", n_curves * 2)
  idx <- 0

  for (i in 0:(n_curves - 1)) {
    start_idx <- if (is_shared) i * 3 + 1 else i * 4 + 1
    P_mat <- points[start_idx:(start_idx + 3), , drop = FALSE]

    # Insert path break if disjoint curves don't connect
    if (i > 0 && !is_shared) {
      prev_end <- points[start_idx - 1, ]
      if (sum((P_mat[1, ] - prev_end)^2) > 1e-4) {
        idx <- idx + 1
        total_points[[idx]] <- matrix(NA, nrow = 1, ncol = n_cols)
      }
    }

    idx <- idx + 1
    total_points[[idx]] <- B_mat %*% P_mat
  }

  do.call(rbind, total_points[seq_len(idx)])
}

#' Match curve counts between two point matrices
#'
#' @description Pads the shorter matrix with null curves (collapsed points)
#'   so both have the same number of 4-point curves.
#'
#' @param pts1 First point matrix (4 points per curve)
#' @param pts2 Second point matrix (4 points per curve)
#' @return List of two matched matrices
#' @keywords internal
match_curves <- function(pts1, pts2) {
  n1 <- nrow(pts1) / 4
  n2 <- nrow(pts2) / 4
  if (n1 == n2) return(list(pts1, pts2))

  pad_curves <- function(pts, target_n) {
    current_n <- nrow(pts) / 4
    if (current_n >= target_n) return(pts)
    diff <- target_n - current_n
    last_pt <- pts[nrow(pts), ]
    null_curve <- matrix(rep(last_pt, 4), nrow = 4, byrow = TRUE)
    extension <- do.call(rbind, replicate(diff, null_curve, simplify = FALSE))
    rbind(pts, extension)
  }

  target <- max(n1, n2)
  list(pad_curves(pts1, target), pad_curves(pts2, target))
}

#' Linear interpolation of matrices
#'
#' @param m1 Start matrix
#' @param m2 End matrix
#' @param alpha Interpolation factor \[0,1\]
#' @return Interpolated matrix
#' @keywords internal
interpolate_matrices <- function(m1, m2, alpha) {
  m1 * (1 - alpha) + m2 * alpha
}

#' Rotate points in 3D using Rodrigues' formula
#'
#' @param points Matrix (N x 3)
#' @param angle Angle in radians
#' @param axis Rotation axis vector (length 3)
#' @return Rotated matrix (N x 3)
#' @keywords internal
rotate_points_3d <- function(points, angle, axis = c(0, 0, 1)) {
  axis <- normalize(axis)
  ux <- axis[1]; uy <- axis[2]; uz <- axis[3]
  ca <- cos(angle)
  sa <- sin(angle)
  omc <- 1 - ca

  R <- matrix(c(
    ca + ux^2 * omc,      ux*uy*omc - uz*sa,   ux*uz*omc + uy*sa,
    uy*ux*omc + uz*sa,    ca + uy^2 * omc,     uy*uz*omc - ux*sa,
    uz*ux*omc - uy*sa,    uz*uy*omc + ux*sa,   ca + uz^2 * omc
  ), nrow = 3, byrow = TRUE)

  points %*% t(R)
}

#' Perspective projection from 3D to 2D
#'
#' @param points Matrix (N x 3)
#' @param focal_length Distance to camera
#' @return Matrix (N x 2)
#' @keywords internal
project_points <- function(points, focal_length = 5) {
  z <- points[, 3]
  scale <- focal_length / (focal_length - z)
  scale[scale < 0] <- 0
  cbind(points[, 1] * scale, points[, 2] * scale)
}

#' Split a cubic Bezier at parameter t
#'
#' @description Uses de Casteljau subdivision to split one cubic Bezier
#'   into two cubic Beziers at parameter t.
#'
#' @param p0,p1,p2,p3 Control points (each a numeric vector)
#' @param t Split parameter \[0,1\]
#' @return List with components `left` and `right`, each a 4-row matrix
#' @keywords internal
bezier_split_at <- function(p0, p1, p2, p3, t) {
  # De Casteljau subdivision
  q0 <- lerp(p0, p1, t)
  q1 <- lerp(p1, p2, t)
  q2 <- lerp(p2, p3, t)

  r0 <- lerp(q0, q1, t)
  r1 <- lerp(q1, q2, t)

  s0 <- lerp(r0, r1, t)

  list(
    left  = rbind(p0, q0, r0, s0),
    right = rbind(s0, r1, q2, p3)
  )
}

#' Compute the approximate arc length of Bezier curves
#'
#' @param points Matrix of control points (N x 3, 4 per curve)
#' @param precision Samples per curve for approximation
#' @return Total arc length (scalar)
#' @keywords internal
path_length <- function(points, precision = 20) {
  if (is.null(points) || nrow(points) < 4) return(0)
  sampled <- sample_bezier(points, precision)
  # Remove NA breaks
  valid <- !is.na(sampled[, 1])
  sampled <- sampled[valid, , drop = FALSE]
  if (nrow(sampled) < 2) return(0)
  diffs <- diff(sampled)
  sum(sqrt(rowSums(diffs^2)))
}

#' Get point at a proportion along the path
#'
#' @description Returns the point at a given proportion (0 to 1) along
#'   the total arc length of the Bezier path.
#'
#' @param points Matrix of control points (N x 3, 4 per curve)
#' @param proportion Value in \[0,1\]
#' @param precision Samples per curve
#' @return Numeric vector (the point)
#' @keywords internal
point_from_proportion <- function(points, proportion, precision = 50) {
  if (is.null(points) || nrow(points) < 4) return(c(0, 0, 0))
  proportion <- clamp(proportion, 0, 1)

  sampled <- sample_bezier(points, precision)
  valid <- !is.na(sampled[, 1])
  sampled <- sampled[valid, , drop = FALSE]
  if (nrow(sampled) < 2) return(sampled[1, ])

  # Compute cumulative arc length
  diffs <- diff(sampled)
  segment_lengths <- sqrt(rowSums(diffs^2))
  cum_length <- c(0, cumsum(segment_lengths))
  total <- cum_length[length(cum_length)]

  if (total < 1e-10) return(sampled[1, ])

  target <- proportion * total
  idx <- findInterval(target, cum_length)
  idx <- max(1, min(idx, length(cum_length) - 1))

  local_t <- if (segment_lengths[idx] > 1e-10) {
    (target - cum_length[idx]) / segment_lengths[idx]
  } else {
    0
  }

  lerp(sampled[idx, ], sampled[idx + 1, ], local_t)
}

#' Compute smooth Bezier handles for a sequence of anchor points
#'
#' @description Given a sequence of anchor points, computes control handles
#'   that create smooth (C1-continuous) cubic Bezier curves connecting them.
#'
#' @param anchors Matrix of anchor points (N x 3), N >= 2
#' @param closed Whether the path is closed (connects last to first)
#' @return Matrix of control points in 4-per-curve format
#' @keywords internal
get_smooth_handle_points <- function(anchors, closed = FALSE) {
  n <- nrow(anchors)
  if (n < 2) return(anchors)
  if (n == 2) {
    # Single line segment as Bezier
    p0 <- anchors[1, ]
    p3 <- anchors[2, ]
    p1 <- p0 + (p3 - p0) / 3
    p2 <- p0 + 2 * (p3 - p0) / 3
    return(rbind(p0, p1, p2, p3))
  }

  # Natural cubic spline through anchors
  # For each segment i: P0=anchors[i], P3=anchors[i+1]
  # We solve for handles P1, P2 using tridiagonal system
  n_curves <- n - 1
  d <- ncol(anchors)

  # Set up tridiagonal system for handle points
  # Using the approach: for smooth curves, tangent at anchor[i] is

  # proportional to (anchor[i+1] - anchor[i-1])
  handles1 <- matrix(0, nrow = n_curves, ncol = d)
  handles2 <- matrix(0, nrow = n_curves, ncol = d)

  for (i in seq_len(n_curves)) {
    p0 <- anchors[i, ]
    p3 <- anchors[i + 1, ]

    if (i == 1) {
      tangent_start <- p3 - p0
    } else {
      tangent_start <- (anchors[i + 1, ] - anchors[i - 1, ]) / 2
    }

    if (i == n_curves) {
      tangent_end <- p3 - p0
    } else {
      tangent_end <- (anchors[i + 2, ] - anchors[i, ]) / 2
    }

    handles1[i, ] <- p0 + tangent_start / 3
    handles2[i, ] <- p3 - tangent_end / 3
  }

  # Build 4-per-curve format
  result <- matrix(0, nrow = n_curves * 4, ncol = d)
  for (i in seq_len(n_curves)) {
    row_start <- (i - 1) * 4
    result[row_start + 1, ] <- anchors[i, ]
    result[row_start + 2, ] <- handles1[i, ]
    result[row_start + 3, ] <- handles2[i, ]
    result[row_start + 4, ] <- anchors[i + 1, ]
  }

  result
}
