.onLoad <- function(libname, pkgname) {
  required_version <- "1.9"
  current_version <- utils::packageVersion("terra")

  if (current_version < required_version) {
    stop(sprintf(
      "Package 'terra' >= %s is required, but %s is installed. Please update 'terra'.",
      required_version, current_version
    ), call. = FALSE)
  }

  required_version <- "1.14"
  current_version <- utils::packageVersion("Evapotranspiration")

  if (current_version < required_version) {
    stop(sprintf(
      "Package 'Evapotranspiration' >= %s is required, but %s is installed. Please update 'Evapotranspiration'.",
      required_version, current_version
    ), call. = FALSE)
  }
}
