#' Reset Local Targets Cache and Parity Symlinks
#'
#' @param project Target project/region ID. Defaults to TAR_PROJECT env var.
#' @param link_dir Parent directory containing parity symlinks.
#'
#' @return Invisible TRUE on success.
#' @export
reset_local_targets_cache <- function(
  project = Sys.getenv("TAR_PROJECT"),
  link_dir = "data"
) {
  cli::cli_inform(
    "Executing clean sweep of local cache and symlinks for project: {.val {project}}..."
  )

  targets_store <- targets::tar_config_get("store", project = project)

  if (fs::dir_exists(targets_store)) {
    fs::dir_delete(targets_store)
    cli::cli_alert_success(
      "Deleted targets store directory: {.path {targets_store}}"
    )
  } else {
    cli::cli_alert_info(
      "No targets store found at {.path {targets_store}}."
    )
  }

  if (fs::dir_exists(link_dir)) {
    symlinks <- fs::dir_ls(link_dir) |> purrr::keep(fs::is_link)

    if (length(symlinks) > 0) {
      fs::link_delete(symlinks)
      cli::cli_alert_success(
        "Deleted symlinks: {.val {fs::path_file(symlinks)}}"
      )
    } else {
      cli::cli_alert_info(
        "No symlinks found in {.path {link_dir}} to delete."
      )
    }
  }

  cli::cli_inform(
    "Clean sweep complete. Re-run execution pipeline to initialize a fresh cache."
  )
  invisible(TRUE)
}

#' Verify Integrity of Targets Store and Cloud Symlinks
#'
#' @param cloud_store_base Parent directory on Azure Blob storage.
#' @param project Target project/region ID. Defaults to TAR_PROJECT env var.
#'
#' @return Invisible TRUE if valid; invisible FALSE after self-healing if corrupted.
#' @export
verify_cloud_targets_integrity <- function(
  cloud_store_base,
  project = Sys.getenv("TAR_PROJECT")
) {
  targets_store <- targets::tar_config_get("store", project = project)
  local_objects_dir <- fs::path(targets_store, "objects")
  local_meta_file <- fs::path(targets_store, "meta", "meta")

  symlink_broken <- fs::link_exists(local_objects_dir) &&
    !fs::file_exists(fs::link_path(local_objects_dir))

  meta_corrupted <- FALSE
  if (fs::file_exists(local_meta_file)) {
    meta_corrupted <- inherits(
      try(targets::tar_meta(project = project), silent = TRUE),
      "try-error"
    )
  }

  if (symlink_broken || meta_corrupted) {
    cli::cli_alert_warning(
      "Detected corrupted local targets state or broken symlink. Healing cache..."
    )
    reset_local_targets_cache(project = project)
    return(invisible(FALSE))
  }

  cli::cli_alert_success("Local target store integrity verified.")
  invisible(TRUE)
}
