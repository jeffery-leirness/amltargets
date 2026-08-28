#' Establish Azure Blob Storage parity symlinks
#'
#' Detects whether the session is running inside an Azure ML compute job or on a
#' local compute instance, resolves the blob storage and workspace paths for
#' that environment, and creates stable parity symlinks so both environments
#' reference the same locations. Local sessions mount the blob container via
#' \pkg{amltools}; cluster sessions use the native datastore mounts exposed
#' through environment variables.
#'
#' Side effects: sets the `BLOB_STORAGE_PARITY_PATH` and
#' `WORKSPACE_PARITY_PATH` environment variables.
#'
#' @param account_name Character scalar. Azure storage account name.
#' @param container_name Character scalar. Azure storage container name.
#' @param link_dir Character scalar. Parent directory for the parity symlinks.
#'   Defaults to `"data"`.
#'
#' @return A named list with elements `is_azure` (logical) and `link_path`
#'   (named list of created symlink paths).
#' @export
aml_parity <- function(account_name, container_name, link_dir = "data") {
  is_azure <- nzchar(Sys.getenv("AZUREML_RUN_ID"))

  if (is_azure) {
    cli::cli_inform("Azure ML compute: using native datastore mounts.")
    blob_path <- Sys.getenv("BLOB_STORAGE_PATH")
    workspace_path <- Sys.getenv("WORKSPACE_PATH")
  } else {
    cli::cli_inform("Local environment: mounting Azure Blob Storage...")
    blob_path <- amltools::mount_blob_storage(
      account_name = account_name,
      container_name = container_name
    )
    workspace_path <- fs::path_abs(".")
  }

  link_path <- amltools::set_symlink(
    source_paths = list(
      blob_storage = blob_path,
      workspaceworkingdirectory = workspace_path
    ),
    link_dir = link_dir
  )

  Sys.setenv(
    BLOB_STORAGE_PARITY_PATH = link_path$blob_storage,
    WORKSPACE_PARITY_PATH = link_path$workspaceworkingdirectory
  )

  list(is_azure = is_azure, link_path = link_path)
}
