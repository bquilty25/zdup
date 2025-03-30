#!/usr/bin/env Rscript

#' Initialize logging
#' @param log_file Optional path to log file
initialize_logging <- function(log_file = NULL) {
  # Create log directory if it doesn't exist
  if (!dir.exists("logs")) {
    dir.create("logs")
  }
  
  # If no log file specified, create one with timestamp
  if (is.null(log_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_file <- file.path("logs", paste0("run_", timestamp, ".log"))
  }
  
  # Initialize global log file
  options(log_file = log_file)
  
  log_message("Logging initialized")
  log_message(paste("R Version:", R.version.string))
  log_message(paste("System:", Sys.info()["sysname"]))
  log_message(paste("Working directory:", getwd()))
}

#' Log a message with timestamp
#' @param message The message to log
#' @param level The log level (INFO, WARNING, ERROR)
log_message <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(paste(timestamp, "-", msg))
}

#' Log a warning message
#' @param message The warning message
log_warning <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(paste(timestamp, "- WARNING:", msg))
}

#' Log an error message
#' @param message The error message
log_error <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(paste(timestamp, "- ERROR:", msg))
}

#' Log the start of a process
#' @param process_name Name of the process
log_start <- function(process_name) {
  log_message(sprintf("Starting %s", process_name))
}

#' Log the end of a process
#' @param process_name Name of the process
log_end <- function(process_name) {
  log_message(sprintf("Completed %s", process_name))
}

#' Log an error and optionally stop execution
#' @param message The error message
#' @param stop_execution Whether to stop execution (default: TRUE)
log_fatal <- function(message, stop_execution = TRUE) {
  log_message(message, level = "FATAL")
  if (stop_execution) {
    stop(message, call. = FALSE)
  }
}

#' Time a process and log its duration
#' @param expr Expression to time
#' @param process_name Name of the process
time_process <- function(expr, process_name) {
  log_start(process_name)
  start_time <- Sys.time()
  
  result <- eval(expr)
  
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  log_message(sprintf("%s completed in %.2f seconds", process_name, duration))
  
  return(result)
}

#' Log system information
log_system_info <- function() {
  log_message("System Information:")
  log_message(paste("  R Version:", R.version.string))
  log_message(paste("  Operating System:", Sys.info()["sysname"]))
  log_message(paste("  Platform:", R.version$platform))
  log_message(paste("  Working Directory:", getwd()))
  
  # Log package versions
  packages <- c("tidyverse", "Rcpp", "data.table", "ggplot2", "scales", "viridis", "gridExtra")
  pkg_versions <- sapply(packages, function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      as.character(packageVersion(pkg))
    } else {
      "not installed"
    }
  })
  
  log_message("Package Versions:")
  for (i in seq_along(packages)) {
    log_message(sprintf("  %s: %s", packages[i], pkg_versions[i]))
  }
}

#' Create a progress logger for iterative processes
#' @param total Total number of iterations
#' @param process_name Name of the process
#' @return Function to update progress
create_progress_logger <- function(total, process_name) {
  start_time <- Sys.time()
  counter <- 0
  
  function(increment = 1) {
    counter <<- counter + increment
    progress <- counter / total * 100
    elapsed <- difftime(Sys.time(), start_time, units = "secs")
    estimated_total <- elapsed * total / counter
    remaining <- estimated_total - elapsed
    
    log_message(sprintf(
      "%s: %.1f%% complete (%d/%d) - Est. remaining: %.0f seconds",
      process_name, progress, counter, total, remaining
    ))
  }
}

# Function to log debug information with timestamp
log_debug <- function(msg) {
  if (getOption("debug", FALSE)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message(paste(timestamp, "- DEBUG:", msg))
  }
}

# Function to log information with timestamp
log_info <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(paste(timestamp, "- INFO:", msg))
} 