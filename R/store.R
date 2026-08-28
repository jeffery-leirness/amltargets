sync_dir_keep_anchor <- function(dir_path) {
  if (!fs::dir_exists(dir_path)) {
    fs::dir_create(dir_path)
  }

  keep_file <- fs::path(dir_path, ".keep")
  non_keep <- fs::dir_ls(dir_path) |>
    (\(x) x[fs::path_file(x) != ".keep"])()

  if (length(non_keep) == 0) {
    if (!fs::file_exists(keep_file)) fs::file_touch(keep_file)
  } else if (fs::file_exists(keep_file)) {
    fs::file_delete(keep_file)
  }

  invisible(dir_path)
}

#' Initialize Local Targets Store from Cloud Metadata and Objects
#'
#' @param cloud_store_base Parent directory on Azure Blob storage.
#' @param project Target project/region ID. Defaults to TAR_PROJECT env var.
#'
#' @return Character path to local targets store.
#' @export
prepare_cloud_targets_store <- function(
  cloud_store_base,
  project = Sys.getenv("TAR_PROJECT")
) {
  targets_store <- targets::tar_config_get("store", project = project)
  cloud_store_dir <- fs::path(cloud_store_base, project)
  cloud_meta_dir <- fs::path(cloud_store_dir, "meta")
  cloud_objects_dir <- fs::path(cloud_store_dir, "objects")
  local_meta_dir <- fs::path(targets_store, "meta")
  local_objects_dir <- fs::path(targets_store, "objects")

  purrr::walk(
    c(cloud_store_dir, cloud_objects_dir, cloud_meta_dir),
    sync_dir_keep_anchor
  )

  fs::dir_create(targets_store)
  fs::dir_create(local_meta_dir)

  cli::cli_inform("Pulling metadata from cloud store...")
  cloud_meta_files <- fs::dir_ls(cloud_meta_dir, type = "file") |>
    (\(x) x[fs::path_file(x) != ".keep"])()

  if (length(cloud_meta_files) > 0) {
    fs::file_copy(cloud_meta_files, local_meta_dir, overwrite = TRUE)
  }

  local_meta_file <- fs::path(local_meta_dir, "meta")
  if (
    fs::file_exists(fs::path(cloud_meta_dir, "meta")) &&
      !fs::file_exists(local_meta_file)
  ) {
    cli::cli_abort(
      "Failed to pull targets metadata file from cloud store: {.path {local_meta_file}}"
    )
  }

  if (!fs::link_exists(local_objects_dir)) {
    if (fs::dir_exists(local_objects_dir)) {
      fs::dir_delete(local_objects_dir)
    }
    fs::link_create(fs::path_abs(cloud_objects_dir), local_objects_dir)
  }

  targets_store
}

#' Push Local Targets Metadata Back to Cloud Store
#'
#' @param cloud_store_base Parent directory on Azure Blob storage.
#' @param project Target project/region ID. Defaults to TAR_PROJECT env var.
#'
#' @return Invisible TRUE on success.
#' @export
push_cloud_targets_metadata <- function(
  cloud_store_base,
  project = Sys.getenv("TAR_PROJECT")
) {
  cli::cli_inform("Syncing metadata back to cloud store...")
  targets_store <- targets::tar_config_get("store", project = project)
  cloud_meta_dir <- fs::path(cloud_store_base, project, "meta")
  local_meta_files <- fs::dir_ls(
    fs::path(targets_store, "meta"),
    type = "file"
  )

  purrr::walk(local_meta_files, \(src_file) {
    dest_file <- fs::path(cloud_meta_dir, fs::path_file(src_file))
    # blobfuse requires delete-before-copy to avoid file-lock failures
    if (fs::file_exists(dest_file)) {
      fs::file_delete(dest_file)
    }
    fs::file_copy(src_file, dest_file)
  })

  sync_dir_keep_anchor(cloud_meta_dir)
  sync_dir_keep_anchor(fs::path(cloud_store_base, project, "objects"))

  cli::cli_inform("Pipeline run and metadata sync complete.")
  invisible(TRUE)
}
