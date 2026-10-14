# Process-level cache shared by all sessions (one R process serves them all), so
# slowly-changing STAC lookups are done once rather than once per session.

# Process-level cache shared by all sessions (one R process serves them all), so
# slowly-changing STAC lookups are done once rather than once per session.
ndc_cache <- new.env()

ndc_cache_ttl <- 3600  # seconds

# A cache key from the parts that identify the question. A long key (the WKT of a complex polygon) is shortened with a
# hash: the names in an environment are limited to 10000 bytes.
cache_key <- function(...) {
  key <- paste(..., sep = "|")
  if (nchar(key) > 200) paste0(substr(key, 1, 60), "|", rlang::hash(key)) else key
}

cache_get <- function(key) {
  hit <- ndc_cache[[key]]
  if (!is.null(hit) && difftime(Sys.time(), hit$time, units = "secs") < ndc_cache_ttl) hit$value else NULL
}

ndc_cache_max_entries <- 500L  # every area makes new keys: without a limit the cache would only grow

# Remove the entries that have expired and, if there are still more than `max_entries`, the oldest ones
cache_prune <- function(max_entries = ndc_cache_max_entries) {
  keys <- ls(ndc_cache, all.names = TRUE)
  if (length(keys) == 0) return(invisible(NULL))
  age <- vapply(keys, function(k) as.numeric(difftime(Sys.time(), ndc_cache[[k]]$time, units = "secs")), numeric(1))
  expired <- keys[age >= ndc_cache_ttl]
  keep <- setdiff(keys, expired)
  excess <- length(keep) - max_entries
  if (excess > 0) expired <- c(expired, keep[order(age[keep], decreasing = TRUE)][seq_len(excess)])
  rm(list = expired, envir = ndc_cache)
  invisible(NULL)
}

cache_set <- function(key, value) {
  if (!is.null(value)) {
    ndc_cache[[key]] <- list(value = value, time = Sys.time())
    cache_prune()
  }
  invisible(value)
}
