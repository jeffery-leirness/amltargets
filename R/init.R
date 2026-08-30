#' Initialize an Azure ML-backed targets session
#'
#' Top-level entry point that establishes blob storage parity (see
#' [aml_parity()]) and then pulls the cloud targets store into the local
#' session (see [aml_store_pull()]).
#'
#' @param account_name Character scalar. Azure storage account name.
#' @param container_name Character scalar. Azure storage container name.
#' @param cloud_store_path Character scalar. Path suffix under the blob storage
#'   root to the targets store parent directory.
#' @param project Character scalar. Target project/region ID. Defaults to the
#'   `TAR_PROJECT` environment variable.
#' @param blob_link_name Character scalar. Forwarded to [aml_parity()]. Defaults
#'   to `"blob_storage"`.
#' @param workspace_link_name Character scalar. Forwarded to [aml_parity()].
#'   Defaults to `"workspace"`.
#'
#' @return A named list with elements `is_azure` and `cloud_store_base`.
#' @export
aml_init <- function(
  account_name,
  container_name,
  cloud_store_path,
  project = Sys.getenv("TAR_PROJECT"),
  blob_link_name = "blob_storage",
  workspace_link_name = "workspace"
) {
  parity <- aml_parity(
    account_name = account_name,
    container_name = container_name,
    blob_link_name = blob_link_name,
    workspace_link_name = workspace_link_name
  )
  cloud_store_base <- fs::path(
    parity$link_path[[blob_link_name]],
    cloud_store_path
  )
  aml_store_pull(cloud_store_base = cloud_store_base, project = project)

  list(is_azure = parity$is_azure, cloud_store_base = cloud_store_base)
}
