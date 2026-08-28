test_that("aml_parity mounts blob storage locally and sets parity env vars", {
  withr::local_envvar(c(
    AZUREML_RUN_ID = "",
    BLOB_STORAGE_PARITY_PATH = "",
    WORKSPACE_PARITY_PATH = ""
  ))

  mount_called <- FALSE
  local_mocked_bindings(
    mount_blob_storage = function(...) {
      mount_called <<- TRUE
      "/mnt/blob"
    },
    set_symlink = function(source_paths, link_dir) {
      list(
        blob_storage = fs::path(link_dir, "blob_storage"),
        workspaceworkingdirectory = fs::path(
          link_dir,
          "workspaceworkingdirectory"
        )
      )
    },
    .package = "amltools"
  )

  result <- aml_parity(account_name = "acct", container_name = "cont")

  expect_true(mount_called)
  expect_false(result$is_azure)
  expect_equal(Sys.getenv("BLOB_STORAGE_PARITY_PATH"), "data/blob_storage")
})


test_that("aml_parity uses native mounts and skips mounting on the cluster", {
  withr::local_envvar(c(
    AZUREML_RUN_ID = "run-123",
    BLOB_STORAGE_PATH = "/native/blob",
    WORKSPACE_PATH = "/native/ws"
  ))

  mount_called <- FALSE
  local_mocked_bindings(
    mount_blob_storage = function(...) {
      mount_called <<- TRUE
      "/should/not/happen"
    },
    set_symlink = function(source_paths, link_dir) {
      list(
        blob_storage = source_paths$blob_storage,
        workspaceworkingdirectory = source_paths$workspaceworkingdirectory
      )
    },
    .package = "amltools"
  )

  result <- aml_parity(account_name = "acct", container_name = "cont")

  expect_false(mount_called)
  expect_true(result$is_azure)
  expect_equal(result$link_path$blob_storage, "/native/blob")
})
