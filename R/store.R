#' Pull the cloud targets store into the local session
#'
#' Copies target metadata from the cloud store and symlinks the local
#' `objects/` directory to the cloud object store, so `targets::tar_read()`
#' resolves cloud-built targets locally.
#'
#' @param cloud_store_base Character scalar. Parent directory of the targets
#'   store on Azure Blob storage.
#' @param project Character scalar. Target project/region ID. Defaults to the
#'   `TAR_PROJECT` environment variable.
#'
#' @return Character path to the local targets store (invisibly usable).
#' @export
aml_store_pull <- function(
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
    keep_anchor
  )

  fs::dir_create(targets_store)
  fs::dir_create(local_meta_dir)

  cli::cli_inform("Pulling metadata from cloud store...")
  cloud_meta_files <- dir_ls_no_anchor(cloud_meta_dir, type = "file")
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


#' Push local targets metadata back to the cloud store
#'
#' @inheritParams aml_store_pull
#'
#' @return Invisibly, `TRUE` on success.
#' @export
aml_store_push <- function(
  cloud_store_base,
  project = Sys.getenv("TAR_PROJECT")
) {
  cli::cli_inform("Syncing metadata back to cloud store...")
  targets_store <- targets::tar_config_get("store", project = project)
  cloud_meta_dir <- fs::path(cloud_store_base, project, "meta")
  fs::dir_create(cloud_meta_dir)
  local_meta_files <- fs::dir_ls(fs::path(targets_store, "meta"), type = "file")

  purrr::walk(local_meta_files, \(src_file) {
    dest_file <- fs::path(cloud_meta_dir, fs::path_file(src_file))
    # blobfuse requires delete-before-copy to avoid file-lock failures
    if (fs::file_exists(dest_file)) {
      fs::file_delete(dest_file)
    }
    fs::file_copy(src_file, dest_file)
  })

  keep_anchor(cloud_meta_dir)
  keep_anchor(fs::path(cloud_store_base, project, "objects"))

  cli::cli_inform("Pipeline run and metadata sync complete.")
  invisible(TRUE)
}


#' Refresh the local store with remote cluster results
#'
#' Resets the local cache, cycle-refreshes the Blobfuse mount to invalidate
#' stale VFS attribute caches (local compute instances only), re-establishes
#' parity symlinks, and re-pulls the cloud store.
#'
#' @param account_name Character scalar. Azure storage account name.
#' @param container_name Character scalar. Azure storage container name.
#' @param cloud_store_base Character scalar. Parent directory of the targets
#'   store on Azure Blob storage.
#' @param project Character scalar. Target project/region ID. Defaults to the
#'   `TAR_PROJECT` environment variable.
#' @param link_dir Character scalar. Parent directory containing parity
#'   symlinks. Defaults to `"data"`.
#'
#' @return Invisibly, the character path to the updated local targets store.
#' @export
aml_store_sync <- function(
  account_name,
  container_name,
  cloud_store_base,
  project = Sys.getenv("TAR_PROJECT"),
  link_dir = "data"
) {
  cli::cli_inform(
    "Synchronizing remote cluster results to local environment..."
  )
  is_azure <- nzchar(Sys.getenv("AZUREML_RUN_ID"))

  aml_store_reset(project = project, link_dir = link_dir)

  if (is_azure) {
    cli::cli_alert_info(
      "Inside Azure ML compute job; skipping storage remount."
    )
  } else {
    cli::cli_inform(
      "Local compute instance: cycle-refreshing Blobfuse mount..."
    )
    try(
      amltools::unmount_blob_storage(
        account_name = account_name,
        container_name = container_name
      ),
      silent = TRUE
    )
    amltools::mount_blob_storage(
      account_name = account_name,
      container_name = container_name
    )
  }

  aml_parity(
    account_name = account_name,
    container_name = container_name,
    link_dir = link_dir
  )
  targets_store <- aml_store_pull(
    cloud_store_base = cloud_store_base,
    project = project
  )

  cli::cli_alert_success(
    "Remote cluster result sync complete. Local store updated."
  )
  invisible(targets_store)
}


#' Reset the local targets cache and parity symlinks
#'
#' @param project Character scalar. Target project/region ID. Defaults to the
#'   `TAR_PROJECT` environment variable.
#' @param link_dir Character scalar. Parent directory containing parity
#'   symlinks. Defaults to `"data"`.
#'
#' @return Invisibly, `TRUE`.
#' @export
aml_store_reset <- function(
  project = Sys.getenv("TAR_PROJECT"),
  link_dir = "data"
) {
  cli::cli_inform(
    "Clearing local cache and symlinks for project {.val {project}}..."
  )
  targets_store <- targets::tar_config_get("store", project = project)

  if (fs::dir_exists(targets_store)) {
    # Detach the cloud-backed objects/ symlink before deleting the store so we
    # can never recurse through it into blob storage.
    objects_link <- fs::path(targets_store, "objects")
    if (fs::link_exists(objects_link)) {
      fs::link_delete(objects_link)
    }
    fs::dir_delete(targets_store)
    cli::cli_alert_success("Deleted targets store: {.path {targets_store}}")
  } else {
    cli::cli_alert_info("No targets store found at {.path {targets_store}}.")
  }

  if (fs::dir_exists(link_dir)) {
    symlinks <- fs::dir_ls(link_dir) |> purrr::keep(fs::is_link)
    if (length(symlinks) > 0) {
      fs::link_delete(symlinks)
      cli::cli_alert_success(
        "Deleted symlinks: {.val {fs::path_file(symlinks)}}"
      )
    } else {
      cli::cli_alert_info("No symlinks found in {.path {link_dir}} to delete.")
    }
  }

  invisible(TRUE)
}


#' Verify local store integrity and self-heal if corrupted
#'
#' Checks that the local `objects/` symlink resolves and that target metadata
#' is readable. If either check fails, the local cache is reset via
#' [aml_store_reset()].
#'
#' @param project Character scalar. Target project/region ID. Defaults to the
#'   `TAR_PROJECT` environment variable.
#'
#' @return Invisibly, `TRUE` if valid; `FALSE` after self-healing a corrupted
#'   store.
#' @export
aml_store_verify <- function(project = Sys.getenv("TAR_PROJECT")) {
  targets_store <- targets::tar_config_get("store", project = project)
  local_objects_dir <- fs::path(targets_store, "objects")
  local_meta_file <- fs::path(targets_store, "meta", "meta")

  symlink_broken <- fs::link_exists(local_objects_dir) &&
    !fs::file_exists(fs::link_path(local_objects_dir))

  meta_corrupted <- fs::file_exists(local_meta_file) &&
    inherits(
      try(targets::tar_meta(project = project), silent = TRUE),
      "try-error"
    )

  if (symlink_broken || meta_corrupted) {
    cli::cli_alert_warning(
      "Corrupted local target state or broken symlink; healing cache..."
    )
    aml_store_reset(project = project)
    return(invisible(FALSE))
  }

  cli::cli_alert_success("Local target store integrity verified.")
  invisible(TRUE)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' List directory entries, excluding the `.keep` anchor
#' @param path Character. Directory to list.
#' @param ... Passed to [fs::dir_ls()].
#' @return Character vector of paths.
#' @keywords internal
dir_ls_no_anchor <- function(path, ...) {
  fs::dir_ls(path, ...) |> purrr::discard(\(p) fs::path_file(p) == ".keep")
}


#' Maintain a `.keep` anchor so empty directories persist on blob storage
#' @param dir_path Character. Directory to anchor.
#' @return Invisibly, `dir_path`.
#' @keywords internal
keep_anchor <- function(dir_path) {
  if (!fs::dir_exists(dir_path)) {
    fs::dir_create(dir_path)
  }
  keep_file <- fs::path(dir_path, ".keep")

  if (length(dir_ls_no_anchor(dir_path)) == 0) {
    if (!fs::file_exists(keep_file)) fs::file_touch(keep_file)
  } else if (fs::file_exists(keep_file)) {
    fs::file_delete(keep_file)
  }

  invisible(dir_path)
}
