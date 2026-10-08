# ============================================================
# GOOGLE FOLDABLE DISPLAY ENGINE
# R optimisation / telemetry / control prototype
# ============================================================

library(stats)

# ------------------------------------------------------------
# DEVICE STATE
# ------------------------------------------------------------

device <- new.env(parent = emptyenv())

device$hinge_angle <- 180.0       # degrees
device$target_angle <- 180.0

device$display_refresh <- 120.0   # Hz
device$touch_poll <- 240.0        # Hz

device$cpu_load <- 0.20
device$gpu_load <- 0.25
device$thermal <- 35.0            # Celsius

device$frame_time <- 1000 / device$display_refresh
device$touch_latency <- 4.0       # ms
device$render_latency <- 3.0      # ms

device$animation_velocity <- 0
device$animation_position <- 0

device$fold_state <- "OPEN"


# ------------------------------------------------------------
# HINGE STATE
# ------------------------------------------------------------

get_fold_state <- function(angle) {

  if (angle < 20) {
    "CLOSED"

  } else if (angle < 80) {
    "TENT"

  } else if (angle < 150) {
    "LAPTOP"

  } else if (angle < 175) {
    "NEAR_OPEN"

  } else {
    "OPEN"
  }
}


update_hinge <- function(angle) {

  angle <- max(0, min(180, angle))

  device$hinge_angle <- angle
  device$fold_state <- get_fold_state(angle)

  invisible(device$fold_state)
}


# ------------------------------------------------------------
# DISPLAY GEOMETRY
# ------------------------------------------------------------

calculate_display_layout <- function(angle) {

  state <- get_fold_state(angle)

  if (state == "CLOSED") {

    list(
      mode = "COVER",
      left_display = FALSE,
      right_display = FALSE,
      refresh = 60
    )

  } else if (state == "TENT") {

    list(
      mode = "TENT",
      left_display = TRUE,
      right_display = TRUE,
      refresh = 90
    )

  } else if (state == "LAPTOP") {

    list(
      mode = "DESKTOP",
      left_display = TRUE,
      right_display = TRUE,
      refresh = 120
    )

  } else {

    list(
      mode = "TABLET",
      left_display = TRUE,
      right_display = TRUE,
      refresh = 120
    )
  }
}


# ------------------------------------------------------------
# HINGE VELOCITY
# ------------------------------------------------------------

hinge_velocity <- function(previous_angle,
                           current_angle,
                           dt) {

  if (dt <= 0)
    return(0)

  (current_angle - previous_angle) / dt
}


# ------------------------------------------------------------
# ANIMATION PREDICTION
# ------------------------------------------------------------

predict_hinge_position <- function(
    angle,
    velocity,
    prediction_ms = 16
) {

  predicted <- angle +
    velocity * (prediction_ms / 1000)

  max(0, min(180, predicted))
}


# ------------------------------------------------------------
# TOUCH LATENCY MODEL
# ------------------------------------------------------------

estimate_touch_latency <- function(
    touch_poll,
    frame_rate,
    cpu_load,
    gpu_load
) {

  polling_latency <- 1000 / touch_poll / 2

  frame_latency <- 1000 / frame_rate / 2

  processing_latency <-
    1.5 +
    cpu_load * 4 +
    gpu_load * 3

  total <-
    polling_latency +
    frame_latency +
    processing_latency

  total
}


# ------------------------------------------------------------
# FRAME TIME
# ------------------------------------------------------------

calculate_frame_time <- function(refresh_rate) {

  1000 / refresh_rate
}


# ------------------------------------------------------------
# JANK ESTIMATOR
# ------------------------------------------------------------

estimate_jank <- function(
    frame_times,
    refresh_rate
) {

  budget <- 1000 / refresh_rate

  missed <- sum(frame_times > budget)

  missed / length(frame_times)
}


# ------------------------------------------------------------
# ADAPTIVE REFRESH RATE
# ------------------------------------------------------------

adaptive_refresh <- function(
    velocity,
    cpu_load,
    gpu_load,
    thermal
) {

  # Fast physical movement:
  # prioritise responsiveness.

  if (abs(velocity) > 80 &&
      thermal < 42) {

    return(120)
  }

  # High thermal load:
  # reduce display workload.

  if (thermal > 45) {

    return(60)
  }

  # Static UI.

  if (abs(velocity) < 2 &&
      cpu_load < 0.25 &&
      gpu_load < 0.30) {

    return(60)
  }

  90
}


# ------------------------------------------------------------
# FRAME PACING
# ------------------------------------------------------------

generate_frame_schedule <- function(
    duration_ms = 1000,
    refresh_rate = 120
) {

  frame_interval <- 1000 / refresh_rate

  seq(
    0,
    duration_ms,
    by = frame_interval
  )
}


# ------------------------------------------------------------
# ANIMATION EASING
# ------------------------------------------------------------

smoothstep <- function(x) {

  x <- max(0, min(1, x))

  x * x * (3 - 2 * x)
}


fold_animation <- function(
    start_angle,
    end_angle,
    duration_ms = 350,
    fps = 120
) {

  frames <- ceiling(
    duration_ms / (1000 / fps)
  )

  t <- seq(
    0,
    1,
    length.out = frames
  )

  eased <- smoothstep(t)

  start_angle +
    (end_angle - start_angle) * eased
}


# ------------------------------------------------------------
# PREDICTIVE DISPLAY TRANSITION
# ------------------------------------------------------------

transition_engine <- function(
    current_angle,
    previous_angle,
    dt
) {

  velocity <- hinge_velocity(
    previous_angle,
    current_angle,
    dt
  )

  predicted <- predict_hinge_position(
    current_angle,
    velocity
  )

  refresh <- adaptive_refresh(
    velocity,
    device$cpu_load,
    device$gpu_load,
    device$thermal
  )

  list(
    current_angle = current_angle,
    velocity = velocity,
    predicted_angle = predicted,
    refresh_rate = refresh
  )
}


# ------------------------------------------------------------
# THERMAL-AWARE PERFORMANCE CONTROL
# ------------------------------------------------------------

thermal_controller <- function(
    thermal,
    cpu_load,
    gpu_load
) {

  performance <- 1.0

  if (thermal > 40)
    performance <- performance * 0.95

  if (thermal > 43)
    performance <- performance * 0.85

  if (thermal > 46)
    performance <- performance * 0.70

  if (cpu_load > 0.85)
    performance <- performance * 0.90

  if (gpu_load > 0.85)
    performance <- performance * 0.90

  performance
}


# ------------------------------------------------------------
# COMPLETE PERFORMANCE MODEL
# ------------------------------------------------------------

foldable_performance <- function(
    hinge_angle,
    previous_angle,
    dt,
    cpu_load,
    gpu_load,
    thermal
) {

  update_hinge(hinge_angle)

  velocity <- hinge_velocity(
    previous_angle,
    hinge_angle,
    dt
  )

  refresh <- adaptive_refresh(
    velocity,
    cpu_load,
    gpu_load,
    thermal
  )

  frame_time <-
    calculate_frame_time(refresh)

  touch_latency <-
    estimate_touch_latency(
      device$touch_poll,
      refresh,
      cpu_load,
      gpu_load
    )

  predicted_angle <-
    predict_hinge_position(
      hinge_angle,
      velocity
    )

  performance <-
    thermal_controller(
      thermal,
      cpu_load,
      gpu_load
    )

  list(
    state = device$fold_state,
    hinge_angle = hinge_angle,
    hinge_velocity = velocity,
    predicted_angle = predicted_angle,
    refresh_rate = refresh,
    frame_budget_ms = frame_time,
    touch_latency_ms = touch_latency,
    performance_factor = performance
  )
}


# ------------------------------------------------------------
# EXAMPLE
# ------------------------------------------------------------

result <- foldable_performance(
  hinge_angle = 112,
  previous_angle = 108,
  dt = 0.016,
  cpu_load = 0.35,
  gpu_load = 0.42,
  thermal = 38
)

print(result)






performance_target <- function(
    refresh_rate,
    thermal,
    gpu_load
) {

  budget <- 1000 / refresh_rate

  if (thermal < 40 &&
      gpu_load < 0.70) {

    return(list(
      refresh = 120,
      frame_budget = budget,
      animation_quality = 1.0
    ))
  }

  if (thermal < 45) {

    return(list(
      refresh = 90,
      frame_budget = budget,
      animation_quality = 0.90
    ))
  }

  list(
    refresh = 60,
    frame_budget = 1000 / 60,
    animation_quality = 0.75
  )
}





