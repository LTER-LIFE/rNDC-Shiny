# What everything shares.

# A positive whole number from an environment variable, or `default`
env_count <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name)))
  if (is.na(value) || value < 1) default else value
}

same_na <- function(x, y) {
  (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && identical(x, y))
}

safe_filename <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)
