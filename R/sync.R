#' Force Refresh Local Blob Mount and Sync Cluster Job Results
#'
#' Resets local target metadata, unmounts and remounts Azure Blob storage to
#' invalidate Blobfuse VFS attribute caches, and re-initializes parity links.
#'
#' @param account_name Azure storage account name.
#' @param container_name Azure storage container name.
#' @param cloud_store_base Parent directory path on Azure Blob storage.
#' @param project Target project/region ID. Defaults to TAR_PROJECT env var.
#' @param link_dir Parent directory containing parity symlinks.
#'
#' @return Invisible character path to the updated local targets store.
#' @export
sync_cluster_results <- function(
  account_name,
  container_name,
  cloud_store_base,
  project = Sys.getenv("TAR_PROJECT"),
  link_dir = "data"
) {
  cli::cli_inform(
    "Synchronizing remote cluster run results to local environment..."
  )

  is_azure <- Sys.getenv("AZUREML_RUN_ID") != ""

  reset_local_targets_cache(project = project, link_dir = link_dir)

  if (!is_azure) {
    cli::cli_inform(
      "Local Compute Instance detected: Cycle-refreshing Blobfuse mount..."
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
  } else {
    cli::cli_alert_info(
      "Running inside Azure ML compute job context. Skipping storage remount."
    )
  }

  parity <- setup_azure_parity(
    account_name = account_name,
    container_name = container_name,
    link_dir = link_dir
  )

  targets_store <- prepare_cloud_targets_store(
    cloud_store_base = cloud_store_base,
    project = project
  )

  cli::cli_alert_success(
    "Remote cluster result sync complete. Local store updated."
  )
  invisible(targets_store)
}
