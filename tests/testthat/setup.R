# The tests run in the app's own process, as in an interactive R session. The tests of the background mode
# (test-async.R) turn it on for themselves.
withr::local_envvar(NDC_ASYNC = "false", .local_envir = teardown_env())
