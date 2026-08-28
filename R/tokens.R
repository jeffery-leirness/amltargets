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
