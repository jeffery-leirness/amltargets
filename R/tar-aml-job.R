#' Azure ML Target Factory for targets
#'
#' A drop-in replacement for [targets::tar_target()] that offloads the target's
#' computation to an Azure ML Compute Cluster as an asynchronous Command Job.
#' Job state is tracked via a JSON token file so the pipeline survives local
#' sleep and reboot cycles.
#'
#' The function uses non-standard evaluation so that \pkg{targets} can perform
#' automatic static code analysis on `command` and detect upstream dependencies.
#'
#' @section Storage architecture:
#' This package assumes a split-storage deployment:
#' * **Code** lives on `workspacefileshare` (Azure Files), mounted on both the
#'   local Compute Instance and the cluster. `code: "."` in the job YAML tells
#'   Azure ML to snapshot the current directory from the fileshare as the job
#'   source.
#' * **Pipeline cache** (`_targets/`) lives on `workspaceblobstore` (Azure
#'   Blob Storage), attached to the cluster via the job `outputs` block. The
#'   cluster container symlinks the blob mount to `_targets/` before R runs.
#'
#' @section Execution modes:
#' The orchestrator selects a mode at runtime:
#' * **Cluster mode** — triggered when the environment variable
#'   `AZUREML_RUN_ID` is set (i.e., the code is already running inside an
#'   Azure ML job). The command is evaluated locally on the cluster.
#' * **Forced local mode** — triggered when `getOption("tar_azure_ml_local")`
#'   is `TRUE` or `TAR_AZURE_ML_LOCAL=true` is set. Useful for testing
#'   pipelines without submitting cloud jobs.
#' * **Hybrid mode** (default) — the target is submitted as an Azure ML
#'   Command Job and the local R session polls until completion.
#'
#' @param name Symbol. The name of the target (unevaluated).
#' @param command Expression. The R expression to compute the target value
#'   (unevaluated). Passed to Azure ML as `Rscript -e '<command>'`.
#' @param cluster Character scalar. Name of the Azure ML compute cluster.
#' @param datastore_path Character scalar. Azure ML datastore URI pointing to
#'   the targets cache on blob storage, e.g.
#'   `"azureml://datastores/workspaceblobstore/paths/pipeline/_targets/"`.
#' @param environment Character scalar. Pre-formed Azure ML environment
#'   reference, e.g. `"azureml:r-tidyverse@latest"`.
#' @param resource_group Character scalar. Azure resource group name. If `NULL`
#'   (the default), falls back to the `AZURE_RESOURCE_GROUP` environment
#'   variable. An error is raised if neither is set.
#' @param workspace Character scalar. Azure ML workspace name. If `NULL`
#'   (the default), falls back to the `AZURE_ML_WORKSPACE` environment
#'   variable. An error is raised if neither is set.
#' @param poll_interval Integer scalar. Seconds to wait between status polls.
#'   Defaults to `60L`.
#' @param ... Additional arguments forwarded to [targets::tar_target_raw()],
#'   e.g. `format`, `pattern`, `priority`.
#'
#' @return A [targets::tar_target()] object.
#'
#' @examples
#' \dontrun{
#' # Option 1: supply resource_group and workspace explicitly
#' list(
#'   tar_aml_job(
#'     heavy_model,
#'     train_model(data),
#'     cluster = "gpu-cluster",
#'     datastore_path = "azureml://datastores/workspaceblobstore/paths/pipeline/_targets/",
#'     environment = "azureml:r-tidyverse@latest",
#'     resource_group = "my-rg",
#'     workspace = "my-ws"
#'   )
#' )
#'
#' # Option 2: set env vars once (e.g., in .Renviron or pipeline setup)
#' # AZURE_RESOURCE_GROUP=my-rg
#' # AZURE_ML_WORKSPACE=my-ws
#' list(
#'   tar_aml_job(
#'     heavy_model,
#'     train_model(data),
#'     cluster = "gpu-cluster",
#'     datastore_path = "azureml://datastores/workspaceblobstore/paths/pipeline/_targets/",
#'     environment = "azureml:r-tidyverse@latest",
#'   ),
#'   tar_aml_job(
#'     predictions,
#'     predict_model(heavy_model),
#'     cluster = "cpu-cluster",
#'     datastore_path = "azureml://datastores/workspaceblobstore/paths/pipeline/_targets/",
#'     environment = "azureml:r-tidyverse@latest",
#'   )
#' )
#' }
#'
#' @importFrom stats as.formula
#' @export
tar_aml_job <- function(
  name,
  command,
  cluster,
  datastore_path,
  environment,
  resource_group = NULL,
  workspace = NULL,
  poll_interval = 60L,
  ...
) {
  # Capture name and command via NSE
  name_str <- deparse(substitute(name))
  command_str <- paste(
    deparse(substitute(command), width.cutoff = 500L),
    collapse = "\n"
  )

  # Resolve resource_group and workspace from env vars if not supplied
  resource_group <- resolve_azure_arg(
    resource_group,
    "AZURE_RESOURCE_GROUP",
    "resource_group"
  )
  workspace <- resolve_azure_arg(
    workspace,
    "AZURE_ML_WORKSPACE",
    "workspace"
  )

  # deps formula — embeds the raw expression so targets sees upstream symbols
  # for static dependency analysis without executing anything locally
  deps_formula <- as.formula(paste("~", command_str))

  # Resolve the internal orchestrator from this package namespace at runtime
  # so targets can evaluate reliably without using self ::: calls.
  internal_call <- substitute(
    get(
      "tar_aml_job_internal",
      envir = asNamespace("amltargets"),
      inherits = FALSE
    )(
      target_name = target_name,
      command_str = command_str,
      deps = deps,
      cluster = cluster,
      datastore_path = datastore_path,
      environment = environment,
      resource_group = resource_group,
      workspace = workspace,
      poll_interval = poll_interval
    ),
    list(
      target_name = name_str,
      command_str = command_str,
      deps = deps_formula,
      cluster = cluster,
      datastore_path = datastore_path,
      environment = environment,
      resource_group = resource_group,
      workspace = workspace,
      poll_interval = poll_interval
    )
  )

  targets::tar_target_raw(
    name = name_str,
    command = internal_call,
    ...
  )
}


# ---------------------------------------------------------------------------
# Runtime orchestrator (evaluated by the target built above during tar_make())
# ---------------------------------------------------------------------------

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
    cli::cli_inform(
      "Submitted Azure ML job for target {.val {target_name}}. Run ID: {run_id}"
    )
  } else {
    run_id <- read_token(target_name)
    cli::cli_inform(
      "Resuming Azure ML job for target {.val {target_name}}. Run ID: {run_id}"
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
      cli::cli_abort(
        "Azure ML job {run_id} for target {.val {target_name}} ended with status: {status}."
      )
    }

    cli::cli_inform(
      "{format(Sys.time(), '%Y-%m-%d %H:%M:%S')} Job {run_id} status: {status}. Polling again in {poll_interval}s..."
    )
    Sys.sleep(poll_interval)
  }
}
