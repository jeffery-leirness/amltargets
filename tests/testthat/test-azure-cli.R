test_that("resolve_azure_arg returns supplied value when non-NULL", {
  result <- amltargets:::resolve_azure_arg(
    "my-rg",
    "AZURE_RESOURCE_GROUP",
    "resource_group"
  )
  expect_equal(result, "my-rg")
})

test_that("resolve_azure_arg falls back to env var when value is NULL", {
  withr::with_envvar(c(AZURE_RESOURCE_GROUP = "env-rg"), {
    result <- amltargets:::resolve_azure_arg(
      NULL,
      "AZURE_RESOURCE_GROUP",
      "resource_group"
    )
    expect_equal(result, "env-rg")
  })
})

test_that("resolve_azure_arg stops when neither value nor env var is set", {
  withr::with_envvar(c(AZURE_RESOURCE_GROUP = ""), {
    expect_error(
      amltargets:::resolve_azure_arg(
        NULL,
        "AZURE_RESOURCE_GROUP",
        "resource_group"
      ),
      regexp = "AZURE_RESOURCE_GROUP"
    )
  })
})

test_that("write_job_yaml sets outputs$cluster_targets_dir$mode to rw_mount", {
  path <- amltargets:::write_job_yaml(
    target_name = "my_target",
    cluster_command = "1 + 1",
    cluster = "cpu-cluster",
    datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
    environment = "azureml:r-base:1"
  )
  on.exit(unlink(path))

  parsed <- yaml::read_yaml(path)
  expect_equal(parsed$outputs$cluster_targets_dir$mode, "rw_mount")
  expect_equal(parsed$outputs$cluster_targets_dir$type, "uri_folder")
  expect_equal(
    parsed$outputs$cluster_targets_dir$path,
    "azureml://datastores/workspaceblobstore/paths/targets/"
  )
})


test_that("write_job_yaml command string prefixes ln -s symlink correctly", {
  path <- amltargets:::write_job_yaml(
    target_name = "cmd_target",
    cluster_command = "targets::tar_make()",
    cluster = "cpu-cluster",
    datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
    environment = "azureml:r-base:1"
  )
  on.exit(unlink(path))

  parsed <- yaml::read_yaml(path)
  expect_match(
    parsed$command,
    "ln -s ${{outputs.cluster_targets_dir}} _targets &&",
    fixed = TRUE
  )
  expect_match(parsed$command, "targets::tar_make()", fixed = TRUE)
})


test_that("write_job_yaml sets code field to current working directory", {
  path <- amltargets:::write_job_yaml(
    target_name = "code_target",
    cluster_command = "1 + 1",
    cluster = "cpu-cluster",
    datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
    environment = "azureml:r-base:1"
  )
  on.exit(unlink(path))

  parsed <- yaml::read_yaml(path)
  expect_equal(parsed$code, getwd())
})


test_that("write_job_yaml sets environment, compute, and experiment name from arguments", {
  path <- amltargets:::write_job_yaml(
    target_name = "struct_target",
    cluster_command = "1 + 1",
    cluster = "gpu-cluster",
    datastore_path = "azureml://datastores/workspaceblobstore/paths/targets/",
    environment = "azureml:r-tidymodels:2"
  )
  on.exit(unlink(path))

  parsed <- yaml::read_yaml(path)
  expect_equal(parsed$environment, "azureml:r-tidymodels:2")
  expect_equal(parsed$compute, "gpu-cluster")
  expect_equal(parsed$experiment_name, "struct_target")
})


test_that("submit_job returns the run ID on success", {
  local_mocked_bindings(
    run = function(...) {
      list(status = 0L, stdout = "run-abc-123\n", stderr = "")
    },
    .package = "processx"
  )
  run_id <- amltargets:::submit_job("fake.yml", "my-rg", "my-ws")
  expect_equal(run_id, "run-abc-123")
})


test_that("submit_job stops on non-zero exit code", {
  local_mocked_bindings(
    run = function(...) list(status = 1L, stdout = "", stderr = "auth error"),
    .package = "processx"
  )
  expect_error(
    amltargets:::submit_job("fake.yml", "my-rg", "my-ws"),
    regexp = "az ml job create failed"
  )
})


test_that("get_job_status parses Completed correctly", {
  json_out <- jsonlite::toJSON(list(status = "Completed"), auto_unbox = TRUE)
  local_mocked_bindings(
    run = function(...) list(status = 0L, stdout = json_out, stderr = ""),
    .package = "processx"
  )
  status <- amltargets:::get_job_status("run-abc-123", "my-rg", "my-ws")
  expect_equal(status, "Completed")
})


test_that("get_job_status parses Failed correctly", {
  json_out <- jsonlite::toJSON(list(status = "Failed"), auto_unbox = TRUE)
  local_mocked_bindings(
    run = function(...) list(status = 0L, stdout = json_out, stderr = ""),
    .package = "processx"
  )
  status <- amltargets:::get_job_status("run-abc-123", "my-rg", "my-ws")
  expect_equal(status, "Failed")
})


test_that("get_job_status stops on non-zero exit code", {
  local_mocked_bindings(
    run = function(...) list(status = 1L, stdout = "", stderr = "not found"),
    .package = "processx"
  )
  expect_error(
    amltargets:::get_job_status("run-abc-123", "my-rg", "my-ws"),
    regexp = "az ml job show failed"
  )
})


test_that("token helpers roundtrip correctly", {
  tmp_dir <- withr::local_tempdir()

  withr::with_dir(tmp_dir, {
    amltargets:::write_token("tok_target", "run-tok-001")
    expect_true(amltargets:::token_exists("tok_target"))
    expect_equal(amltargets:::read_token("tok_target"), "run-tok-001")
    amltargets:::delete_token("tok_target")
    expect_false(amltargets:::token_exists("tok_target"))
  })
})


test_that("delete_token removes azure_tokens directory when last token is deleted", {
  tmp_dir <- withr::local_tempdir()

  withr::with_dir(tmp_dir, {
    amltargets:::write_token("tok_target", "run-tok-001")
    token_dir <- dirname(amltargets:::token_path("tok_target"))
    expect_true(dir.exists(token_dir))

    amltargets:::delete_token("tok_target")

    expect_false(file.exists(amltargets:::token_path("tok_target")))
    expect_false(dir.exists(token_dir))
  })
})


test_that("delete_token preserves azure_tokens directory when other tokens remain", {
  tmp_dir <- withr::local_tempdir()

  withr::with_dir(tmp_dir, {
    amltargets:::write_token("tok_one", "run-001")
    amltargets:::write_token("tok_two", "run-002")

    token_dir <- dirname(amltargets:::token_path("tok_one"))
    expect_true(dir.exists(token_dir))

    amltargets:::delete_token("tok_one")

    expect_false(file.exists(amltargets:::token_path("tok_one")))
    expect_true(file.exists(amltargets:::token_path("tok_two")))
    expect_true(dir.exists(token_dir))
  })
})
