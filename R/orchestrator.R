#' Internal orchestrator for Azure ML target execution
#'
#' Called by the target factory during `tar_make()`. Handles three execution
#' modes: cluster/local evaluation, job submission, and status polling.
#' Not intended to be called by users directly.
#'
#' @param target_name Character. The target name string.
#' @param command_str Character. The R expression string.
#' @param deps Formula. Embeds the raw command expression for dependency
#'   tracking (`~ <command>`).
#' @param cluster Character. Azure ML compute cluster name.
#' @param datastore_path Character. Azure ML datastore URI for the targets
#'   cache, e.g.
#'   `"azureml://datastores/workspaceblobstore/paths/pipeline/_targets/"`.
#' @param environment Character. Pre-formed Azure ML environment reference,
#'   e.g. `"azureml:r-tidyverse@latest"`.
#' @param resource_group Character. Azure resource group name.
#' @param workspace Character. Azure ML workspace name.
#' @param poll_interval Integer. Seconds between status polls.
#'
#' @return The target value, read from the shared `_targets/` mount after the
#'   Azure ML job completes (hybrid mode), or the result of evaluating
#'   `command` locally (cluster/local mode).
#' @keywords internal
tar_aml_job_internal <- function(
  target_name,
  command_str,
  deps,
  cluster,
  datastore_path,
  environment,
  resource_group,
  workspace,
  poll_interval = 60L
) {
  # ------------------------------------------------------------------
  # Guard: cluster mode or forced local mode
  # ------------------------------------------------------------------
  on_cluster <- nzchar(Sys.getenv("AZUREML_RUN_ID"))
  forced_local <- isTRUE(getOption("tar_azure_ml_local")) ||
    identical(Sys.getenv("TAR_AZURE_ML_LOCAL"), "true")

  if (on_cluster || forced_local) {
    return(eval(deps[[2L]], envir = parent.frame()))
  }

  # ------------------------------------------------------------------
  # Phase A: Submit or resume
  # ------------------------------------------------------------------
  if (!token_exists(target_name)) {
    # 1. Write a lock-free worker script for the cluster to execute
    script_file <- tempfile(
      pattern = paste0("_azureml_", target_name, "_"),
      tmpdir = ".",
      fileext = ".R"
    )
    script_name <- basename(script_file)
    script_content <- c(
      "# Source targets script to load packages, globals, and user functions",
      "suppressWarnings(suppressPackageStartupMessages(source('_targets.R')))",
      "",
      "# Find upstream dependencies using the pipeline's static network graph",
      "edges <- targets::tar_network(targets_only = TRUE)$edges",
      sprintf('deps <- edges$from[edges$to == "%s"]', target_name),
      'if (length(deps) > 0) {',
      '  targets::tar_load(any_of(deps))',
      '}',
      "",
      "# Evaluate the target expression",
      'result <- {',
      command_str,
      '}',
      "",
      "# Save to a transfer directory so the local machine can format it natively",
      'transfer_dir <- file.path(targets::tar_path_store(), "azure_transfers")',
      'dir.create(transfer_dir, showWarnings = FALSE, recursive = TRUE)',
      sprintf('saveRDS(result, file.path(transfer_dir, "%s.rds"))', target_name)
    )
    writeLines(script_content, script_file)

    # 2. Write the YAML using the project root as the code snapshot
    yaml_path <- write_job_yaml(
      target_name = target_name,
      cluster_command = paste0("Rscript ", script_name),
      cluster = cluster,
      datastore_path = datastore_path,
      environment = environment
    )

    # 3. Submit the job and immediately clean up the local worker script
    run_id <- submit_job(yaml_path, resource_group, workspace)
    unlink(script_file)

    write_token(target_name, run_id)
    message(
      "Submitted Azure ML job for target '",
      target_name,
      "'. Run ID: ",
      run_id
    )
  } else {
    run_id <- read_token(target_name)
    message(
      "Resuming tracking of Azure ML job for target '",
      target_name,
      "'. Run ID: ",
      run_id
    )
  }

  # ------------------------------------------------------------------
  # Phase B: Poll until terminal state
  # ------------------------------------------------------------------
  while (TRUE) {
    status <- get_job_status(run_id, resource_group, workspace)

    if (identical(status, "Completed")) {
      delete_token(target_name)

      # Retrieve the result from the cluster's transfer file
      transfer_dir <- file.path(targets::tar_path_store(), "azure_transfers")
      transfer_path <- file.path(
        transfer_dir,
        paste0(target_name, ".rds")
      )
      result <- readRDS(transfer_path)

      # Clean up the transfer file to keep the shared blob storage tidy
      unlink(transfer_path)

      # Remove the transfer directory only if it is empty
      if (
        dir.exists(transfer_dir) &&
          length(list.files(transfer_dir, all.files = TRUE, no.. = TRUE)) == 0L
      ) {
        unlink(transfer_dir, recursive = TRUE, force = TRUE)
      }

      # Return the result so local tar_make() can apply the requested format (qs, etc.)
      return(result)
    }

    if (status %in% c("Failed", "Canceled")) {
      delete_token(target_name)
      stop(
        "Azure ML job ",
        run_id,
        " for target '",
        target_name,
        "' ended with status: ",
        status,
        call. = FALSE
      )
    }

    message(
      "[",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "] ",
      "Job ",
      run_id,
      " status: ",
      status,
      ". Polling again in ",
      poll_interval,
      "s..."
    )
    Sys.sleep(poll_interval)
  }
}
