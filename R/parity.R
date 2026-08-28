#' Detect Environment and Establish Azure Blob Storage Parity Symlinks
#'
#' @param account_name Azure storage account name.
#' @param container_name Azure storage container name.
#' @param link_dir Symlink target parent directory.
#'
#' @return Named list: `is_azure`, `run_mode`, `link_path`.
#' @export
setup_azure_parity <- function(
  account_name,
  container_name,
  link_dir = "data"
) {
  is_azure <- Sys.getenv("AZUREML_RUN_ID") != ""

  if (!is_azure) {
    cli::cli_inform("Local environment: Mounting Azure Blob Storage...")
    actual_blob_path <- amltools::mount_blob_storage(
      account_name = account_name,
      container_name = container_name
    )
    actual_workspace_path <- fs::path_abs(".")
    run_mode <- Sys.getenv("RUN_MODE", unset = "local")
  } else {
    cli::cli_inform("Azure ML compute: Using native datastore mounts...")
    actual_blob_path <- Sys.getenv("BLOB_STORAGE_PATH")
    actual_workspace_path <- Sys.getenv("WORKSPACE_PATH")
    run_mode <- Sys.getenv("RUN_MODE", unset = "cluster")
  }

  link_path <- amltools::set_symlink(
    source_paths = list(
      blob_storage = actual_blob_path,
      workspaceworkingdirectory = actual_workspace_path
    ),
    link_dir = link_dir
  )

  Sys.setenv(BLOB_STORAGE_PARITY_PATH = link_path$blob_storage)
  Sys.setenv(WORKSPACE_PARITY_PATH = link_path$workspaceworkingdirectory)
  Sys.setenv(RUN_MODE = run_mode)

  list(
    is_azure = is_azure,
    run_mode = run_mode,
    link_path = link_path
  )
}
