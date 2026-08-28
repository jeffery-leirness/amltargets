#' Initialize Azure ML Targets Session
#'
#' Detects environment, mounts or resolves blob storage, establishes parity
#' symlinks, and pulls cloud metadata into the local targets store.
#'
#' @param account_name Azure storage account name.
#' @param container_name Azure storage container name.
#' @param cloud_store_path Path suffix under blob storage root to the targets store.
#' @param project Target project/region ID. Defaults to TAR_PROJECT env var.
#'
#' @return Named list: `is_azure`, `run_mode`, `cloud_store_base`.
#' @export
aml_init <- function(
  account_name,
  container_name,
  cloud_store_path,
  project = Sys.getenv("TAR_PROJECT")
) {
  parity <- setup_azure_parity(
    account_name = account_name,
    container_name = container_name
  )

  cloud_store_base <- fs::path(
    parity$link_path$blob_storage,
    cloud_store_path
  )

  prepare_cloud_targets_store(
    cloud_store_base = cloud_store_base,
    project = project
  )

  list(
    is_azure = parity$is_azure,
    run_mode = parity$run_mode,
    cloud_store_base = cloud_store_base
  )
}
