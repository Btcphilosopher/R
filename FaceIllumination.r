# Face Active Illumination System
# R prototype for low-light face authentication
#
# Pipeline:
# Ambient Light
#      ↓
# Illumination Controller
#      ↓
# IR / visible active illumination
#      ↓
# Camera frame
#      ↓
# Face / landmark quality analysis
#      ↓
# Exposure + illumination optimisation
#      ↓
# Face embedding / authentication model
#      ↓
# Accept / Reject

library(stats)

# ---------------------------------------------------------
# Configuration
# ---------------------------------------------------------

config <- list(
  target_brightness = 0.45,
  min_face_quality = 0.70,
  max_illumination = 1.0,
  min_illumination = 0.0,

  # Exposure limits
  min_exposure = 0.001,
  max_exposure = 0.050,

  # Avoid excessive illumination
  max_sensor_saturation = 0.95
)


# ---------------------------------------------------------
# Sensor state
# ---------------------------------------------------------

FaceSensorState <- function(
    ambient_lux = 100,
    exposure = 0.010,
    illumination = 0.0,
    brightness = 0.0,
    contrast = 0.0,
    sharpness = 0.0,
    saturation = 0.0,
    face_quality = 0.0
) {

  list(
    ambient_lux = ambient_lux,
    exposure = exposure,
    illumination = illumination,
    brightness = brightness,
    contrast = contrast,
    sharpness = sharpness,
    saturation = saturation,
    face_quality = face_quality
  )
}


# ---------------------------------------------------------
# Estimate required active illumination
# ---------------------------------------------------------

estimate_illumination <- function(state, config) {

  brightness_error <-
    config$target_brightness - state$brightness

  # More illumination required when ambient light is low.
  low_light_factor <-
    1 / (1 + log1p(state$ambient_lux))

  illumination <- 
    state$illumination +
    brightness_error * 2.0 +
    low_light_factor * 0.35

  illumination <- max(
    config$min_illumination,
    min(config$max_illumination, illumination)
  )

  illumination
}


# ---------------------------------------------------------
# Exposure controller
# ---------------------------------------------------------

estimate_exposure <- function(state, config) {

  error <-
    config$target_brightness - state$brightness

  new_exposure <-
    state$exposure * exp(error)

  max(
    config$min_exposure,
    min(config$max_exposure, new_exposure)
  )
}


# ---------------------------------------------------------
# Image quality model
# ---------------------------------------------------------

calculate_face_quality <- function(
    brightness,
    contrast,
    sharpness,
    saturation
) {

  brightness_score <-
    1 - abs(brightness - 0.45) / 0.45

  brightness_score <-
    max(0, min(1, brightness_score))

  contrast_score <-
    max(0, min(1, contrast))

  sharpness_score <-
    max(0, min(1, sharpness))

  saturation_penalty <-
    max(0, saturation - 0.80) * 4

  quality <-
    0.30 * brightness_score +
    0.25 * contrast_score +
    0.35 * sharpness_score -
    0.10 * saturation_penalty

  max(0, min(1, quality))
}


# ---------------------------------------------------------
# Active illumination optimisation
# ---------------------------------------------------------

optimise_illumination <- function(state, config) {

  candidates <- seq(
    config$min_illumination,
    config$max_illumination,
    length.out = 21
  )

  scores <- numeric(length(candidates))

  for (i in seq_along(candidates)) {

    illumination <- candidates[i]

    # Simple simulated sensor response.
    simulated_brightness <-
      state$brightness +
      illumination * 0.40

    simulated_brightness <-
      min(1, simulated_brightness)

    simulated_contrast <-
      min(1, state$contrast + illumination * 0.15)

    simulated_sharpness <-
      min(1, state$sharpness + illumination * 0.10)

    simulated_saturation <-
      min(
        1,
        state$saturation +
          illumination * 0.25
      )

    quality <- calculate_face_quality(
      simulated_brightness,
      simulated_contrast,
      simulated_sharpness,
      simulated_saturation
    )

    # Penalise unnecessary illumination.
    power_penalty <- illumination^2 * 0.08

    scores[i] <-
      quality - power_penalty
  }

  best <- which.max(scores)

  list(
    illumination = candidates[best],
    score = scores[best],
    candidates = candidates,
    scores = scores
  )
}


# ---------------------------------------------------------
# Authentication decision
# ---------------------------------------------------------

authenticate_face <- function(
    face_quality,
    embedding_similarity,
    config
) {

  quality_ok <-
    face_quality >= config$min_face_quality

  # Example threshold only.
  similarity_ok <-
    embedding_similarity >= 0.85

  authenticated <-
    quality_ok && similarity_ok

  list(
    authenticated = authenticated,
    quality_ok = quality_ok,
    similarity_ok = similarity_ok
  )
}


# ---------------------------------------------------------
# Main control loop
# ---------------------------------------------------------

face_active_illumination <- function(
    state,
    embedding_similarity = 0.90,
    config = config
) {

  # Step 1: optimise illumination
  illumination_result <-
    optimise_illumination(
      state,
      config
    )

  # Step 2: optimise exposure
  new_exposure <-
    estimate_exposure(
      state,
      config
    )

  # Step 3: estimate resulting image quality
  simulated_brightness <-
    min(
      1,
      state$brightness +
        illumination_result$illumination * 0.40
    )

  simulated_contrast <-
    min(
      1,
      state$contrast +
        illumination_result$illumination * 0.15
    )

  simulated_sharpness <-
    min(
      1,
      state$sharpness +
        illumination_result$illumination * 0.10
    )

  simulated_saturation <-
    min(
      1,
      state$saturation +
        illumination_result$illumination * 0.25
    )

  quality <-
    calculate_face_quality(
      simulated_brightness,
      simulated_contrast,
      simulated_sharpness,
      simulated_saturation
    )

  # Step 4: authentication
  authentication <-
    authenticate_face(
      quality,
      embedding_similarity,
      config
    )

  list(
    illumination =
      illumination_result$illumination,

    exposure =
      new_exposure,

    face_quality =
      quality,

    authentication =
      authentication,

    optimisation_score =
      illumination_result$score
  )
}


# ---------------------------------------------------------
# Example: very dark environment
# ---------------------------------------------------------

sensor <- FaceSensorState(
  ambient_lux = 2,
  exposure = 0.020,
  illumination = 0.10,
  brightness = 0.15,
  contrast = 0.35,
  sharpness = 0.40,
  saturation = 0.05
)


result <- face_active_illumination(
  sensor,
  embedding_similarity = 0.91
)

print(result)

