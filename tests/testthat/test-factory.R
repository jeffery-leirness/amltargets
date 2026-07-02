DS_PATH <- "azureml://datastores/workspaceblobstore/paths/targets/"
ENV_REF <- "azureml:r-base:1"

test_that("tar_aml_job picks up resource_group from env var", {
  withr::with_envvar(
    c(AZURE_RESOURCE_GROUP = "env-rg", AZURE_ML_WORKSPACE = "env-ws"),
    {
      t <- tar_aml_job(
        env_target,
        1 + 1,
        cluster        = "cpu-cluster",
        datastore_path = DS_PATH,
        environment    = ENV_REF
      )
      # The internal call embedded in the target should contain the env var values
      cmd_str <- deparse(t$command$expr)
      expect_match(paste(cmd_str, collapse = " "), "env-rg", fixed = TRUE)
      expect_match(paste(cmd_str, collapse = " "), "env-ws", fixed = TRUE)
    }
  )
})

test_that("tar_aml_job stops when resource_group is unset", {
  withr::with_envvar(
    c(AZURE_RESOURCE_GROUP = "", AZURE_ML_WORKSPACE = "my-ws"),
    {
      expect_error(
        tar_aml_job(
          t, 1 + 1,
          cluster        = "cpu-cluster",
          datastore_path = DS_PATH,
          environment    = ENV_REF
        ),
        regexp = "AZURE_RESOURCE_GROUP"
      )
    }
  )
})

test_that("tar_aml_job stops when workspace is unset", {
  withr::with_envvar(
    c(AZURE_RESOURCE_GROUP = "my-rg", AZURE_ML_WORKSPACE = ""),
    {
      expect_error(
        tar_aml_job(
          t, 1 + 1,
          cluster        = "cpu-cluster",
          datastore_path = DS_PATH,
          environment    = ENV_REF
        ),
        regexp = "AZURE_ML_WORKSPACE"
      )
    }
  )
})

test_that("tar_aml_job returns a tar_target object", {
  t <- tar_aml_job(
    my_target,
    1 + 1,
    cluster        = "cpu-cluster",
    datastore_path = DS_PATH,
    environment    = ENV_REF,
    resource_group = "my-rg",
    workspace      = "my-ws"
  )
  expect_s3_class(t, "tar_target")
})


test_that("tar_aml_job captures the target name correctly", {
  t <- tar_aml_job(
    heavy_model,
    train_model(data),
    cluster        = "gpu-cluster",
    datastore_path = DS_PATH,
    environment    = ENV_REF,
    resource_group = "my-rg",
    workspace      = "my-ws"
  )
  expect_equal(t$settings$name, "heavy_model")
})


test_that("tar_aml_job embeds deps formula in the command", {
  t <- tar_aml_job(
    result_target,
    sqrt(x) + y,
    cluster        = "cpu-cluster",
    datastore_path = DS_PATH,
    environment    = ENV_REF,
    resource_group = "my-rg",
    workspace      = "my-ws"
  )
  # The command call should reference the internal orchestrator
  cmd <- t$command$expr
  cmd_str <- deparse(cmd)
  expect_match(paste(cmd_str, collapse = " "), "tar_aml_job_internal")
})


test_that("tar_aml_job forwards ... to tar_target_raw (format)", {
  t <- tar_aml_job(
    formatted_target,
    1 + 1,
    cluster        = "cpu-cluster",
    datastore_path = DS_PATH,
    environment    = ENV_REF,
    resource_group = "my-rg",
    workspace      = "my-ws",
    format         = "rds"
  )
  expect_equal(t$settings$format, "rds")
})
