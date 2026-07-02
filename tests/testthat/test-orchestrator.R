write_transfer_result <- function(target_name, value) {
  transfer_dir <- file.path(targets::tar_path_store(), "azure_transfers")
  dir.create(transfer_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(value, file.path(transfer_dir, paste0(target_name, ".rds")))
}


test_that("orchestrator evaluates locally when AZUREML_RUN_ID is set", {
  withr::with_envvar(c(AZUREML_RUN_ID = "some-run-id"), {
    result <- amltargets:::tar_aml_job_internal(
      target_name = "t",
      command_str = "42",
      deps = ~42,
      cluster = "c",
      resource_group = "rg",
      workspace = "ws",
      datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
      environment = "azureml:r-base:1",
      poll_interval = 1L
    )
    expect_equal(result, 42)
  })
})


test_that("orchestrator evaluates locally when option tar_azure_ml_local is TRUE", {
  withr::with_options(list(tar_azure_ml_local = TRUE), {
    result <- amltargets:::tar_aml_job_internal(
      target_name = "t",
      command_str = "100",
      deps = ~100,
      cluster = "c",
      resource_group = "rg",
      workspace = "ws",
      datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
      environment = "azureml:r-base:1",
      poll_interval = 1L
    )
    expect_equal(result, 100)
  })
})


test_that("orchestrator evaluates locally when TAR_AZURE_ML_LOCAL=true", {
  withr::with_envvar(c(TAR_AZURE_ML_LOCAL = "true"), {
    result <- amltargets:::tar_aml_job_internal(
      target_name = "t",
      command_str = "2 + 2",
      deps = ~ 2 + 2,
      cluster = "c",
      resource_group = "rg",
      workspace = "ws",
      datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
      environment = "azureml:r-base:1",
      poll_interval = 1L
    )
    expect_equal(result, 4)
  })
})


test_that("orchestrator submits a new job and writes token when none exists", {
  tmp_dir <- withr::local_tempdir()

  withr::with_dir(tmp_dir, {
    expect_false(amltargets:::token_exists("submit_target"))
    write_transfer_result("submit_target", "target_result")

    local_mocked_bindings(
      write_job_yaml = function(...) "fake.yml",
      submit_job = function(...) "run-new-job-001",
      get_job_status = function(...) "Completed",
      .package = "amltargets"
    )

    result <- amltargets:::tar_aml_job_internal(
      target_name = "submit_target",
      command_str = "1 + 1",
      deps = ~ 1 + 1,
      cluster = "cpu-cluster",
      resource_group = "my-rg",
      workspace = "my-ws",
      datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
      environment = "azureml:r-base:1",
      poll_interval = 1L
    )

    expect_equal(result, "target_result")
    expect_false(amltargets:::token_exists("submit_target"))
  })
})


test_that("orchestrator resumes from existing token without re-submitting", {
  tmp_dir <- withr::local_tempdir()

  withr::with_dir(tmp_dir, {
    amltargets:::write_token("resume_target", "run-existing-001")
    write_transfer_result("resume_target", "resumed_result")

    submit_called <- FALSE
    local_mocked_bindings(
      write_job_yaml = function(...) {
        stop("write_job_yaml must not be called on resume")
      },
      submit_job = function(...) {
        submit_called <<- TRUE
        stop("submit_job must not be called on resume")
      },
      get_job_status = function(...) "Completed",
      .package = "amltargets"
    )

    result <- amltargets:::tar_aml_job_internal(
      target_name = "resume_target",
      command_str = "1 + 1",
      deps = ~ 1 + 1,
      cluster = "cpu-cluster",
      resource_group = "my-rg",
      workspace = "my-ws",
      datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
      environment = "azureml:r-base:1",
      poll_interval = 1L
    )

    expect_equal(result, "resumed_result")
    expect_false(submit_called)
  })
})


test_that("orchestrator stops and deletes token when job fails", {
  tmp_dir <- withr::local_tempdir()

  withr::with_dir(tmp_dir, {
    local_mocked_bindings(
      write_job_yaml = function(...) "fake.yml",
      submit_job = function(...) "run-fail-001",
      get_job_status = function(...) "Failed",
      .package = "amltargets"
    )

    expect_error(
      amltargets:::tar_aml_job_internal(
        target_name = "fail_target",
        command_str = "stop('oops')",
        deps = ~ stop("oops"),
        cluster = "cpu-cluster",
        resource_group = "my-rg",
        workspace = "my-ws",
        datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
        environment = "azureml:r-base:1",
        poll_interval = 1L
      ),
      regexp = "Failed"
    )
    expect_false(amltargets:::token_exists("fail_target"))
  })
})


test_that("orchestrator stops and deletes token when job is canceled", {
  tmp_dir <- withr::local_tempdir()

  withr::with_dir(tmp_dir, {
    local_mocked_bindings(
      write_job_yaml = function(...) "fake.yml",
      submit_job = function(...) "run-cancel-001",
      get_job_status = function(...) "Canceled",
      .package = "amltargets"
    )

    expect_error(
      amltargets:::tar_aml_job_internal(
        target_name = "cancel_target",
        command_str = "1 + 1",
        deps = ~ 1 + 1,
        cluster = "cpu-cluster",
        resource_group = "my-rg",
        workspace = "my-ws",
        datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
        environment = "azureml:r-base:1",
        poll_interval = 1L
      ),
      regexp = "Canceled"
    )
    expect_false(amltargets:::token_exists("cancel_target"))
  })
})


test_that("orchestrator sleeps when Running, then completes cleanly", {
  tmp_dir <- withr::local_tempdir()
  call_count <- 0L
  sleep_called <- FALSE

  withr::with_dir(tmp_dir, {
    write_transfer_result("poll_target", "polled_result")

    local_mocked_bindings(
      write_job_yaml = function(...) "fake.yml",
      submit_job = function(...) "run-poll-001",
      get_job_status = function(...) {
        call_count <<- call_count + 1L
        if (call_count == 1L) "Running" else "Completed"
      },
      .package = "amltargets"
    )
    # Patch Sys.sleep in base so the test doesn't block
    local_mocked_bindings(
      Sys.sleep = function(x) {
        sleep_called <<- TRUE
        invisible(NULL)
      },
      .package = "base"
    )

    result <- amltargets:::tar_aml_job_internal(
      target_name = "poll_target",
      command_str = "1 + 1",
      deps = ~ 1 + 1,
      cluster = "cpu-cluster",
      resource_group = "my-rg",
      workspace = "my-ws",
      datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
      environment = "azureml:r-base:1",
      poll_interval = 1L
    )

    expect_equal(result, "polled_result")
    expect_equal(call_count, 2L)
    expect_true(sleep_called)
    expect_false(amltargets:::token_exists("poll_target"))
  })
})


test_that("orchestrator deletes transfer directory when it becomes empty", {
  tmp_dir <- withr::local_tempdir()

  withr::with_dir(tmp_dir, {
    write_transfer_result("cleanup_target", "cleanup_result")
    transfer_dir <- file.path(targets::tar_path_store(), "azure_transfers")
    expect_true(dir.exists(transfer_dir))

    local_mocked_bindings(
      write_job_yaml = function(...) "fake.yml",
      submit_job = function(...) "run-cleanup-001",
      get_job_status = function(...) "Completed",
      .package = "amltargets"
    )

    result <- amltargets:::tar_aml_job_internal(
      target_name = "cleanup_target",
      command_str = "1 + 1",
      deps = ~ 1 + 1,
      cluster = "cpu-cluster",
      resource_group = "my-rg",
      workspace = "my-ws",
      datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
      environment = "azureml:r-base:1",
      poll_interval = 1L
    )

    expect_equal(result, "cleanup_result")
    expect_false(file.exists(file.path(transfer_dir, "cleanup_target.rds")))
    expect_false(dir.exists(transfer_dir))
  })
})
