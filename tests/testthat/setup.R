# The tests run in the app's own process, as in an interactive R session. The tests of the background mode
# (test-job_manager.R and test-server_download.R) turn it on for themselves.
withr::local_envvar(NDC_ASYNC = "false", .local_envir = teardown_env())
