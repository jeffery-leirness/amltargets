test_that("dir_ls_no_anchor excludes the .keep anchor", {
    tmp <- withr::local_tempdir()
    fs::file_touch(fs::path(tmp, ".keep"))
    fs::file_touch(fs::path(tmp, "real.txt"))

    result <- amltargets:::dir_ls_no_anchor(tmp)
    expect_equal(fs::path_file(result), "real.txt")
})


test_that("keep_anchor adds .keep to an empty directory", {
    tmp <- withr::local_tempdir()
    empty <- fs::path(tmp, "empty")

    amltargets:::keep_anchor(empty)
    expect_true(fs::file_exists(fs::path(empty, ".keep")))
})


test_that("keep_anchor removes .keep once a directory is non-empty", {
    tmp <- withr::local_tempdir()
    fs::file_touch(fs::path(tmp, ".keep"))
    fs::file_touch(fs::path(tmp, "data.rds"))

    amltargets:::keep_anchor(tmp)
    expect_false(fs::file_exists(fs::path(tmp, ".keep")))
})


test_that("aml_store_pull copies metadata and symlinks objects to the cloud", {
    tmp <- withr::local_tempdir()
    local_store <- fs::path(tmp, "_targets")
    cloud_base <- fs::path(tmp, "cloud")
    project <- "region_a"

    cloud_meta <- fs::path(cloud_base, project, "meta")
    cloud_objects <- fs::path(cloud_base, project, "objects")
    fs::dir_create(c(cloud_meta, cloud_objects))
    writeLines("meta-contents", fs::path(cloud_meta, "meta"))

    local_mocked_bindings(
        tar_config_get = function(...) as.character(local_store),
        .package = "targets"
    )

    result <- aml_store_pull(cloud_store_base = cloud_base, project = project)

    expect_equal(fs::path_abs(result), fs::path_abs(local_store))
    expect_true(fs::file_exists(fs::path(local_store, "meta", "meta")))
    local_objects <- fs::path(local_store, "objects")
    expect_true(fs::link_exists(local_objects))
    expect_equal(
        fs::path_abs(fs::link_path(local_objects)),
        fs::path_abs(cloud_objects)
    )
})


test_that("aml_store_push copies local metadata files to the cloud store", {
    tmp <- withr::local_tempdir()
    local_store <- fs::path(tmp, "_targets")
    cloud_base <- fs::path(tmp, "cloud")
    project <- "region_a"

    fs::dir_create(fs::path(local_store, "meta"))
    writeLines("m", fs::path(local_store, "meta", "meta"))

    local_mocked_bindings(
        tar_config_get = function(...) as.character(local_store),
        .package = "targets"
    )

    result <- aml_store_push(cloud_store_base = cloud_base, project = project)

    expect_true(result)
    expect_true(fs::file_exists(fs::path(cloud_base, project, "meta", "meta")))
})


test_that("aml_store_reset deletes the store and parity symlinks", {
    tmp <- withr::local_tempdir()

    withr::with_dir(tmp, {
        local_store <- fs::path(tmp, "_targets")
        fs::dir_create(local_store)

        fs::dir_create("data")
        target <- fs::path(tmp, "target")
        fs::dir_create(target)
        fs::link_create(target, fs::path("data", "blob_storage"))

        local_mocked_bindings(
            tar_config_get = function(...) as.character(local_store),
            .package = "targets"
        )

        expect_true(aml_store_reset(project = "region_a", link_dir = "data"))
        expect_false(fs::dir_exists(local_store))
        expect_false(fs::link_exists(fs::path("data", "blob_storage")))
    })
})


test_that("aml_store_reset preserves cloud objects behind the symlink", {
    tmp <- withr::local_tempdir()

    withr::with_dir(tmp, {
        cloud_objects <- fs::path(tmp, "cloud_objects")
        fs::dir_create(cloud_objects)
        writeLines("precious", fs::path(cloud_objects, "obj1.rds"))

        local_store <- fs::path(tmp, "_targets")
        fs::dir_create(local_store)
        fs::link_create(cloud_objects, fs::path(local_store, "objects"))

        local_mocked_bindings(
            tar_config_get = function(...) as.character(local_store),
            .package = "targets"
        )

        aml_store_reset(project = "region_a", link_dir = "data")

        expect_false(fs::dir_exists(local_store))
        expect_true(fs::dir_exists(cloud_objects))
        expect_true(fs::file_exists(fs::path(cloud_objects, "obj1.rds")))
    })
})


test_that("aml_store_verify returns TRUE for an intact store", {
    tmp <- withr::local_tempdir()
    local_store <- fs::path(tmp, "_targets")
    fs::dir_create(local_store)

    local_mocked_bindings(
        tar_config_get = function(...) as.character(local_store),
        .package = "targets"
    )

    expect_true(aml_store_verify(project = "region_a"))
})


test_that("aml_store_verify heals a broken objects symlink", {
    tmp <- withr::local_tempdir()
    local_store <- fs::path(tmp, "_targets")
    fs::dir_create(local_store)
    fs::link_create(
        fs::path(tmp, "does_not_exist"),
        fs::path(local_store, "objects")
    )

    reset_called <- FALSE
    local_mocked_bindings(
        tar_config_get = function(...) as.character(local_store),
        .package = "targets"
    )
    local_mocked_bindings(
        aml_store_reset = function(...) {
            reset_called <<- TRUE
            invisible(TRUE)
        },
        .package = "amltargets"
    )

    expect_false(aml_store_verify(project = "region_a"))
    expect_true(reset_called)
})


test_that("aml_store_sync remounts locally and re-pulls the store", {
    unmounted <- FALSE
    mounted <- FALSE

    withr::local_envvar(c(AZUREML_RUN_ID = ""))
    local_mocked_bindings(
        aml_store_reset = function(...) invisible(TRUE),
        aml_parity = function(...) invisible(NULL),
        aml_store_pull = function(cloud_store_base, project) "/local/_targets",
        .package = "amltargets"
    )
    local_mocked_bindings(
        unmount_blob_storage = function(...) {
            unmounted <<- TRUE
            invisible(NULL)
        },
        mount_blob_storage = function(...) {
            mounted <<- TRUE
            "/mnt/blob"
        },
        .package = "amltools"
    )

    result <- aml_store_sync(
        account_name = "acct",
        container_name = "cont",
        cloud_store_base = "/mnt/blob/pipeline",
        project = "region_a"
    )

    expect_equal(result, "/local/_targets")
    expect_true(unmounted)
    expect_true(mounted)
})


test_that("aml_store_sync skips remounting inside an Azure ML job", {
    mounted <- FALSE

    withr::local_envvar(c(AZUREML_RUN_ID = "run-123"))
    local_mocked_bindings(
        aml_store_reset = function(...) invisible(TRUE),
        aml_parity = function(...) invisible(NULL),
        aml_store_pull = function(cloud_store_base, project) "/local/_targets",
        .package = "amltargets"
    )
    local_mocked_bindings(
        mount_blob_storage = function(...) {
            mounted <<- TRUE
            "/mnt/blob"
        },
        .package = "amltools"
    )

    aml_store_sync(
        account_name = "acct",
        container_name = "cont",
        cloud_store_base = "/mnt/blob/pipeline",
        project = "region_a"
    )

    expect_false(mounted)
})
