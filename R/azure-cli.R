#' Resolve an Azure configuration argument with an env var fallback
#'
#' Returns `value` if non-NULL and non-empty, otherwise reads `env_var`.
#' Stops with an informative message if neither is available.
#'
#' @param value The argument value supplied by the user (or `NULL`).
#' @param env_var Character. Environment variable name to check as fallback.
#' @param arg_name Character. Argument name used in the error message.
#'
#' @return Character scalar.
#' @keywords internal
resolve_azure_arg <- function(value, env_var, arg_name) {
  if (!is.null(value) && nzchar(value)) {
    return(value)
  }

  env_val <- Sys.getenv(env_var, unset = "")
  if (nzchar(env_val)) {
    return(env_val)
  }

  stop(
    "Argument `",
    arg_name,
    "` is not set and environment variable `",
    env_var,
    "` is not defined. ",
    "Supply the argument directly or set `",
    env_var,
    "` in your environment.",
    call. = FALSE
  )
}


#' Write an Azure ML Command Job YAML file
#'
#' Assembles an Azure ML Command Job YAML with split-storage support and writes
#' it to a temporary file. The generated job mounts a blob datastore as a
#' read-write output (`cluster_targets_dir`) and symlinks it to `_targets/`
#' in the cluster container before running R.
#'
#' @param target_name Character. The targets target name (used as display name).
#' @param cluster_command Character. The R expression string to run via
#'   `Rscript -e`.
#' @param cluster Character. Azure ML compute cluster name.
#' @param datastore_path Character. Azure ML datastore URI for the targets
#'   cache, e.g.
#'   `"azureml://datastores/workspaceblobstore/paths/pipeline/_targets/"`.
#' @param environment Character. Pre-formed Azure ML environment reference, e.g.
#'   `"azureml:r-tidyverse@latest"`.
#'
#' @return Path to the written YAML file (character scalar).
#' @keywords internal
write_job_yaml <- function(
  target_name,
  cluster_command,
  cluster,
  datastore_path,
  environment
) {
  job <- list(
    `$schema` = "https://azuremlschemas.azureedge.net/latest/commandJob.schema.json",
    type = "command",
    experiment_name = target_name,
    code = getwd(),
    environment = environment,
    compute = cluster,
    outputs = list(
      cluster_targets_dir = list(
        type = "uri_folder",
        path = datastore_path,
        mode = "rw_mount"
      )
    ),
    command = paste0(
      "ln -s ${{outputs.cluster_targets_dir}} _targets && ",
      cluster_command
    )
  )

  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(job, yaml_path)
  yaml_path
}


#' Submit an Azure ML Command Job
#'
#' Calls `az ml job create` via processx and returns the resulting Run ID.
#'
#' @param yaml_path Character. Path to the job YAML file.
#' @param resource_group Character. Azure resource group name.
#' @param workspace Character. Azure ML workspace name.
#'
#' @return Run ID (character scalar).
#' @keywords internal
submit_job <- function(yaml_path, resource_group, workspace) {
  result <- processx::run(
    command = "az",
    args = c(
      "ml",
      "job",
      "create",
      "--file",
      yaml_path,
      "--resource-group",
      resource_group,
      "--workspace-name",
      workspace,
      "--query",
      "name",
      "--output",
      "tsv"
    ),
    error_on_status = FALSE
  )

  if (result$status != 0L) {
    stop(
      "az ml job create failed (exit ",
      result$status,
      "):\n",
      result$stderr,
      call. = FALSE
    )
  }

  trimws(result$stdout)
}


#' Get the status of an Azure ML job
#'
#' Calls `az ml job show` via processx and returns the job status string.
#'
#' @param run_id Character. The Azure ML Run ID.
#' @param resource_group Character. Azure resource group name.
#' @param workspace Character. Azure ML workspace name.
#'
#' @return One of `"Running"`, `"Preparing"`, `"Completed"`, `"Failed"`,
#'   `"Canceled"`, or `"Unknown"` (character scalar).
#' @keywords internal
get_job_status <- function(run_id, resource_group, workspace) {
  result <- processx::run(
    command = "az",
    args = c(
      "ml",
      "job",
      "show",
      "--name",
      run_id,
      "--resource-group",
      resource_group,
      "--workspace-name",
      workspace,
      "--output",
      "json"
    ),
    error_on_status = FALSE
  )

  if (result$status != 0L) {
    stop(
      "az ml job show failed (exit ",
      result$status,
      "):\n",
      result$stderr,
      call. = FALSE
    )
  }

  parsed <- jsonlite::fromJSON(result$stdout)
  status <- parsed[["status"]]

  if (is.null(status) || !nzchar(status)) "Unknown" else status
}


# ---------------------------------------------------------------------------
# Token file helpers
# ---------------------------------------------------------------------------

#' Path to the JSON token file for a target
#' @param target_name Character. Target name.
#' @return Character file path inside the active targets store.
#' @keywords internal
token_path <- function(target_name) {
  file.path(
    targets::tar_path_store(),
    "azure_tokens",
    paste0(target_name, ".json")
  )
}


#' Write a job token file
#' @param target_name Character. Target name.
#' @param run_id Character. Azure ML Run ID.
#' @return Invisibly, the path written.
#' @keywords internal
write_token <- function(target_name, run_id) {
  path <- token_path(target_name)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(run_id = run_id), path, auto_unbox = TRUE)
  invisible(path)
}


#' Read the Run ID from a token file
#' @param target_name Character. Target name.
#' @return Run ID (character scalar).
#' @keywords internal
read_token <- function(target_name) {
  parsed <- jsonlite::fromJSON(token_path(target_name))
  parsed[["run_id"]]
}


#' Check whether a token file exists
#' @param target_name Character. Target name.
#' @return Logical scalar.
#' @keywords internal
token_exists <- function(target_name) {
  file.exists(token_path(target_name))
}


#' Delete a token file
#' @param target_name Character. Target name.
#' @return Invisibly, the result of `unlink()`.
#' @keywords internal
delete_token <- function(target_name) {
  path <- token_path(target_name)
  token_dir <- dirname(path)
  result <- unlink(path)

  if (
    dir.exists(token_dir) &&
      length(list.files(token_dir, all.files = TRUE, no.. = TRUE)) == 0L
  ) {
    unlink(token_dir, recursive = TRUE, force = TRUE)
  }

  invisible(result)
}
