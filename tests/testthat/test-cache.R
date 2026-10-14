# The cache of the lookups. (R/cache.R)

test_that("a long cache key is shortened with a hash, and a short one is kept", {
  expect_equal(cache_key("count", "lgn", "POLYGON((0 0))"), "count|lgn|POLYGON((0 0))")
  long <- cache_key("avail", "soil", strrep("1.123456 52.123456,", 3000))
  expect_lt(nchar(long), 200)
  expect_false(identical(long, cache_key("avail", "soil", strrep("1.123457 52.123456,", 3000))))  # another polygon
  local_clean_cache()
  cache_set(long, 1)  # a name of an environment may not be longer than 10000 bytes
  expect_equal(cache_get(long), 1)
})

test_that("the cache returns values until they expire", {
  local_clean_cache()
  expect_null(cache_get("k"))
  cache_set("k", 42)
  expect_equal(cache_get("k"), 42)
  cache_set("none", NULL)  # NULL is never cached
  expect_null(cache_get("none"))

  local_mocked_bindings(ndc_cache_ttl = -1)
  expect_null(cache_get("k"))
})

test_that("the cache drops expired entries and keeps at most the maximum number, oldest first", {
  local_clean_cache()
  local_mocked_bindings(ndc_cache_max_entries = 3L)
  for (k in c("a", "b", "c", "d")) {
    cache_set(k, k)
    Sys.sleep(0.01)  # distinct times
  }
  expect_setequal(ls(ndc_cache), c("b", "c", "d"))  # "a" was the oldest

  # expired entries go when something is added, also when the maximum is not reached
  local_mocked_bindings(ndc_cache_ttl = -1)
  cache_set("e", "e")
  expect_length(ls(ndc_cache), 0)
})
