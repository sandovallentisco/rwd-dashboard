args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 6) {
  stop(
    "Usage: refresh_rwd_cache.R <root> <url> <cache_version> ",
    "<normalization_start_year> <normalization_end_year> <library_paths> ",
    "[refresh_interval_hours]"
  )
}

root <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
source_url <- args[[2]]
cache_version <- as.integer(args[[3]])
normalization_start_year <- as.integer(args[[4]])
normalization_end_year <- as.integer(args[[5]])
library_paths <- strsplit(args[[6]], .Platform$path.sep, fixed = TRUE)[[1]]
refresh_interval_hours <- if (length(args) >= 7) as.numeric(args[[7]]) else 7 * 24
if (!is.finite(refresh_interval_hours) || refresh_interval_hours <= 0) {
  stop("refresh_interval_hours must be a positive number.")
}
library_paths <- library_paths[nzchar(library_paths) & dir.exists(library_paths)]
if (length(library_paths) > 0) {
  .libPaths(unique(c(library_paths, .libPaths())))
}

setwd(root)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))

source_path <- file.path(root, "retraction_watch.csv")
cache_path <- file.path(root, "retraction_watch_processed.rds")
status_path <- file.path(root, "rwd_refresh_status.rds")
lock_path <- file.path(root, "rwd_refresh.lock")
app_path <- file.path(root, "app.R")

timestamp <- function(value = Sys.time()) {
  format(value, "%Y-%m-%dT%H:%M:%S%z")
}

write_status <- function(state, message, source_modified_at = NA_character_, records = NA_integer_) {
  status <- list(
    state = state,
    message = message,
    updated_at = timestamp(),
    source_modified_at = source_modified_at,
    records = records
  )

  temporary_status <- tempfile(
    pattern = "rwd-refresh-status-",
    tmpdir = dirname(status_path),
    fileext = ".rds"
  )
  saveRDS(status, temporary_status)
  if (!file.copy(temporary_status, status_path, overwrite = TRUE)) {
    stop("Could not update the refresh status file.")
  }
  unlink(temporary_status)
  invisible(status)
}

safe_replace <- function(new_path, target_path) {
  backup_path <- paste0(target_path, ".backup")
  if (file.exists(backup_path)) unlink(backup_path)

  had_target <- file.exists(target_path)
  if (had_target && !file.rename(target_path, backup_path)) {
    stop("Could not prepare the existing file for replacement: ", basename(target_path))
  }

  replacement_succeeded <- file.rename(new_path, target_path)
  if (!replacement_succeeded) {
    if (had_target && file.exists(backup_path)) {
      file.rename(backup_path, target_path)
    }
    stop("Could not install the completed replacement: ", basename(target_path))
  }

  if (file.exists(backup_path)) unlink(backup_path)
  invisible(TRUE)
}

if (dir.exists(lock_path)) {
  lock_age_hours <- as.numeric(
    difftime(Sys.time(), file.info(lock_path)$mtime, units = "hours")
  )
  if (isTRUE(lock_age_hours < 4)) {
    quit(save = "no", status = 0)
  }
  unlink(lock_path, recursive = TRUE, force = TRUE)
}

if (!dir.create(lock_path, showWarnings = FALSE)) {
  quit(save = "no", status = 0)
}

tryCatch(
  {
    write_status(
      "refreshing",
      "A newer Retraction Watch dataset is being prepared in the background."
    )

    source_needs_download <- !file.exists(source_path)
    if (!source_needs_download) {
      source_age_hours <- as.numeric(
        difftime(Sys.time(), file.info(source_path)$mtime, units = "hours")
      )
      source_needs_download <- isTRUE(source_age_hours > refresh_interval_hours)
    }

    candidate_source <- source_path
    downloaded_source <- FALSE

    if (source_needs_download) {
      candidate_source <- tempfile(
        pattern = "retraction-watch-",
        tmpdir = root,
        fileext = ".csv"
      )
      download.file(
        source_url,
        destfile = candidate_source,
        mode = "wb",
        quiet = TRUE,
        method = "libcurl"
      )
      downloaded_source <- TRUE
    }

    if (!file.exists(candidate_source) || file.info(candidate_source)$size < 1e6) {
      stop("The downloaded Retraction Watch file is missing or unexpectedly small.")
    }

    header <- names(read.csv(candidate_source, stringsAsFactors = FALSE, nrows = 1))
    required_columns <- c(
      "Record.ID", "Title", "Country", "Publisher", "OriginalPaperDate",
      "RetractionDate", "RetractionNature"
    )
    if (!all(required_columns %in% header)) {
      stop("The Retraction Watch file does not contain the expected columns.")
    }

    if (!downloaded_source && file.exists(cache_path)) {
      existing_cache <- tryCatch(readRDS(cache_path), error = function(e) NULL)
      source_md5 <- unname(tools::md5sum(candidate_source))
      cache_is_current <-
        is.list(existing_cache) &&
        identical(existing_cache$cache_version, cache_version) &&
        identical(existing_cache$source_md5, source_md5) &&
        is.list(existing_cache$data) &&
        all(c(
          "retraction_data",
          "lag_paper_data",
          "lag_breakdown_data",
          "paper_reason_data",
          "reason_classification_data",
          "reason_classification_summary",
          "ieee_spike_summary",
          "ieee_spike_conferences"
        ) %in% names(existing_cache$data))

      if (cache_is_current) {
        source_modified_at <- if (
          !is.null(existing_cache$source_modified_at) &&
          nzchar(existing_cache$source_modified_at)
        ) {
          existing_cache$source_modified_at
        } else {
          timestamp(file.info(candidate_source)$mtime)
        }
        write_status(
          "ready",
          "The cached Retraction Watch data are already current.",
          source_modified_at = source_modified_at,
          records = nrow(existing_cache$data$retraction_data)
        )
        unlink(lock_path, recursive = TRUE, force = TRUE)
        quit(save = "no", status = 0)
      }
    }

    pipeline_environment <- new.env(parent = .GlobalEnv)
    pipeline_environment$normalization_start_year <- normalization_start_year
    pipeline_environment$normalization_end_year <- normalization_end_year

    expressions <- parse(file = app_path)
    for (expression in expressions) {
      is_assignment <-
        is.call(expression) &&
        identical(expression[[1]], as.name("<-"))

      assigned_name <- if (is_assignment) as.character(expression[[2]]) else ""
      if (assigned_name %in% c(
        "reason_category_definitions",
        "reason_category_types",
        "reason_procedural_labels",
        "build_reason_category_map",
        "process_retraction_data"
      )) {
        eval(expression, envir = pipeline_environment)
      }
    }

    if (!exists("process_retraction_data", envir = pipeline_environment, inherits = FALSE)) {
      stop("The Retraction Watch processing function could not be loaded.")
    }

    processed <- pipeline_environment$process_retraction_data(candidate_source)
    if (!is.list(processed) || !all(c(
      "retraction_data",
      "lag_paper_data",
      "lag_breakdown_data",
      "paper_reason_data",
      "country_data",
      "publisher_data",
      "reason_classification_data",
      "reason_classification_summary",
      "ieee_spike_summary",
      "ieee_spike_conferences"
    ) %in% names(processed))) {
      stop("The processed Retraction Watch cache is incomplete.")
    }

    source_modified_at <- timestamp(file.info(candidate_source)$mtime)
    new_cache <- list(
      cache_version = cache_version,
      source_md5 = unname(tools::md5sum(candidate_source)),
      source_modified_at = source_modified_at,
      refreshed_at = timestamp(),
      data = processed
    )

    temporary_cache <- tempfile(
      pattern = "retraction-watch-cache-",
      tmpdir = root,
      fileext = ".rds"
    )
    saveRDS(new_cache, temporary_cache, compress = "gzip")
    verified_cache <- readRDS(temporary_cache)
    if (
      !is.list(verified_cache) ||
      !identical(verified_cache$cache_version, cache_version) ||
      !is.list(verified_cache$data) ||
      !all(c(
        "retraction_data",
        "lag_paper_data",
        "lag_breakdown_data",
        "paper_reason_data",
        "reason_classification_data",
        "reason_classification_summary",
        "ieee_spike_summary",
        "ieee_spike_conferences"
      ) %in% names(verified_cache$data))
    ) {
      stop("The completed cache did not pass verification.")
    }

    if (downloaded_source) {
      safe_replace(candidate_source, source_path)
    }
    safe_replace(temporary_cache, cache_path)

    write_status(
      "ready",
      "The latest Retraction Watch data have been processed and are ready.",
      source_modified_at = source_modified_at,
      records = nrow(processed$retraction_data)
    )
  },
  error = function(error) {
    try(
      write_status(
        "failed",
        paste(
          "The background update failed. The previous cached dashboard remains available.",
          conditionMessage(error)
        )
      ),
      silent = TRUE
    )
    unlink(lock_path, recursive = TRUE, force = TRUE)
    message(conditionMessage(error))
    quit(save = "no", status = 1)
  }
)

unlink(lock_path, recursive = TRUE, force = TRUE)
