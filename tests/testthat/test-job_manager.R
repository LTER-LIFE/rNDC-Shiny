# The job manager: the queue, cancelling and failing, with a fake process. (R/job_manager.R)

test_that("jobs run up to the number of workers, and the others wait in order", {
  fake <- fake_manager(workers = 2)
  m <- fake$manager
  a <- job_submit(m, "f", list())
  b <- job_submit(m, "f", list())
  c <- job_submit(m, "f", list())
  d <- job_submit(m, "f", list())

  expect_equal(fake$rec$started, c(a, b))
  expect_equal(job_status(m, a)$state, "running")
  expect_equal(job_status(m, c)$state, "queued")
  expect_equal(c(job_status(m, c)$position, job_status(m, d)$position), c(1, 2))
  expect_equal(job_status(m, d)$queued, 2)
  expect_equal(job_status(m, a)$position, 0)

  # when a job ends, the next one starts, in the order of submission
  finish_fake_job(fake, a, value = "A")
  job_tick(m)
  expect_equal(fake$rec$started, c(a, b, c))
  expect_equal(job_status(m, d)$position, 1)
  expect_equal(job_status(m, a)$state, "done")  # finished jobs wait for their result to be collected
  expect_equal(job_collect(m, a)$value, "A")
  expect_null(job_status(m, a))  # and are then forgotten
})

test_that("too many waiting jobs are refused, but a free worker always takes one", {
  fake <- fake_manager(workers = 1, max_queue = 1)
  m <- fake$manager
  expect_false(is.null(job_submit(m, "f", list())))  # runs
  expect_false(is.null(job_submit(m, "f", list())))  # waits
  expect_null(job_submit(m, "f", list()))            # too many

  none <- fake_manager(workers = 1, max_queue = 0)$manager
  expect_false(is.null(job_submit(none, "f", list())))  # no queue is needed when a worker is free
  expect_null(job_submit(none, "f", list()))
})

test_that("a waiting job can be cancelled and the others move up", {
  fake <- fake_manager(workers = 1)
  m <- fake$manager
  a <- job_submit(m, "f", list())
  b <- job_submit(m, "f", list())
  c <- job_submit(m, "f", list())
  dir <- m$jobs[[b]]$dir

  expect_true(job_cancel(m, b))
  expect_equal(job_status(m, b)$state, "cancelled")
  expect_false(dir.exists(dir))
  expect_equal(job_status(m, c)$position, 1)
  expect_length(fake$rec$killed, 0)  # nothing was running for it
  expect_false(job_cancel(m, b))     # nothing left to cancel
  expect_equal(job_collect(m, b)$state, "cancelled")
})

test_that("a running job is killed, what it made is removed, and the next job takes its place", {
  fake <- fake_manager(workers = 1)
  m <- fake$manager
  made <- withr::local_tempfile()
  writeLines("zip", made)
  a <- job_submit(m, "f", list(), files = made)
  b <- job_submit(m, "f", list())
  dir <- m$jobs[[a]]$dir

  expect_true(job_cancel(m, a))
  expect_equal(fake$rec$killed, a)
  expect_false(file.exists(made))
  expect_false(dir.exists(dir))
  expect_equal(fake$rec$started, c(a, b))
  expect_equal(job_status(m, b)$state, "running")

  # a job that has ended cannot be cancelled any more
  finish_fake_job(fake, b, value = 1)
  job_tick(m)
  expect_false(job_cancel(m, b))
  expect_equal(job_collect(m, b)$value, 1)
})

test_that("a job that fails is reported with the reason", {
  fake <- fake_manager(workers = 2)
  m <- fake$manager
  a <- job_submit(m, "f", list())
  b <- job_submit(m, "f", list())

  finish_fake_job(fake, a, error = "it broke")
  finish_fake_job(fake, b, crash = TRUE)  # the process is gone and left nothing
  job_tick(m)
  expect_equal(job_status(m, a)$state, "failed")
  expect_equal(job_status(m, a)$error, "it broke")
  expect_equal(job_status(m, b)$state, "failed")
  expect_match(job_status(m, b)$error, "stopped unexpectedly.*stderr of the process")
  out <- job_collect(m, b)
  expect_equal(out$state, "failed")
  expect_null(out$value)

  # a process that cannot be started is a failure too, and does not block the queue
  broken <- job_manager(1, start = function(job) stop("no process"))
  id <- job_submit(broken, "f", list())
  expect_equal(job_status(broken, id)$state, "failed")
  expect_match(job_status(broken, id)$error, "could not be started: no process")
})

test_that("the progress of a running job is visible, and shutting down stops everything", {
  fake <- fake_manager(workers = 1)
  m <- fake$manager
  a <- job_submit(m, "f", list())
  b <- job_submit(m, "f", list())
  expect_null(job_status(m, a)$progress)
  job_progress_writer(m$jobs[[a]]$progress_file)(detail = "Weather (A)", value = 0.2)
  expect_equal(job_status(m, a)$progress, list(value = 0.2, detail = "Weather (A)"))
  expect_gte(job_status(m, a)$running, 0)

  dirs <- vapply(m$jobs, function(job) job$dir, "")
  job_shutdown(m)
  expect_equal(fake$rec$killed, a)
  expect_false(any(dir.exists(dirs)))
  expect_length(m$jobs, 0)
})
