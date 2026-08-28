test_that("aml_init composes parity and store pull, returning cloud_store_base", {
  local_mocked_bindings(
    aml_parity = function(...) {
      list(
        is_azure = FALSE,
        link_path = list(blob_storage = "/mnt/blob")
      )
    },
    aml_store_pull = function(cloud_store_base, project) cloud_store_base,
    .package = "amltargets"
  )

  result <- aml_init(
    account_name = "acct",
    container_name = "cont",
    cloud_store_path = "pipeline/_targets",
    project = "region_a"
  )

  expect_false(result$is_azure)
  expect_equal(
    as.character(result$cloud_store_base),
    as.character(fs::path("/mnt/blob", "pipeline/_targets"))
  )
})
