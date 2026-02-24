# COSERO Project Setup Functions
# Create new COSERO project folders with required binaries
# Author: COSERO R Interface
# Date: 2025-01-25

#' Setup Working Example COSERO Project
#'
#' Creates a ready-to-run COSERO project by extracting the complete Wildalpen
#' example catchment (includes binaries, input files, and example outputs).
#'
#' @param project_path Path where the example project should be created.
#' @param overwrite Overwrite existing files? (default: FALSE)
#'
#' @return Invisibly returns the project path.
#'
#' @details
#' Extracts a complete working example (Wildalpen catchment) that includes:
#' \itemize{
#'   \item COSERO.exe and required DLLs
#'   \item Complete input files (defaults.txt, para.txt, meteorological data)
#'   \item Example output files
#' }
#'
#' Use this to quickly test COSERO or as a template for your own projects.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create example project
#' setup_cosero_project_example("C:/COSERO_example")
#'
#' # Run the example
#' run_cosero("C:/COSERO_example")
#' }
setup_cosero_project_example <- function(project_path, overwrite = FALSE) {

  if (is.null(project_path) || nchar(project_path) == 0) {
    stop("project_path must be specified", call. = FALSE)
  }

  # Find the example zip
  zip_path <- system.file("extdata", "COSERO_Wildalpen.zip", package = "CoseRo")

  if (!file.exists(zip_path)) {
    stop(
      "Example project zip not found at: ", zip_path, "\n",
      "The package installation may be incomplete.",
      call. = FALSE
    )
  }

  # Create project directory if needed
  if (!dir.exists(project_path)) {
    dir.create(project_path, recursive = TRUE)
    cat("Created project directory:", project_path, "\n")
  }

  # Extract to temp location first (zip contains COSERO/ subfolder)
  cat("Extracting Wildalpen example project...\n")
  temp_extract <- file.path(tempdir(), "cosero_extract_temp")
  if (dir.exists(temp_extract)) unlink(temp_extract, recursive = TRUE)

  utils::unzip(zip_path, exdir = temp_extract)

  # Move contents from COSERO/ subfolder to project_path
  source_dir <- file.path(temp_extract, "COSERO")
  if (!dir.exists(source_dir)) {
    # Fallback: maybe zip structure is different
    source_dir <- temp_extract
  }

  # Copy all files and folders
  all_items <- list.files(source_dir, full.names = TRUE, all.files = FALSE)
  for (item in all_items) {
    dest_item <- file.path(project_path, basename(item))
    if (dir.exists(item)) {
      # Copy directory recursively
      if (dir.exists(dest_item) && !overwrite) {
        cat("Skipping", basename(item), "(already exists)\n")
      } else {
        if (dir.exists(dest_item) && overwrite) {
          unlink(dest_item, recursive = TRUE)
        }
        file.copy(item, project_path, recursive = TRUE, overwrite = overwrite)
      }
    } else {
      # Copy file
      if (file.exists(dest_item) && !overwrite) {
        cat("Skipping", basename(item), "(already exists)\n")
      } else {
        file.copy(item, dest_item, overwrite = overwrite)
      }
    }
  }

  # Cleanup
  unlink(temp_extract, recursive = TRUE)

  cat("\n=== Example Project Ready ===\n")
  cat("Location:", project_path, "\n")
  cat("Catchment: Wildalpen (Austria)\n")
  cat("\nContents:\n")
  cat("  COSERO.exe + DLLs\n")
  cat("  Input files: defaults.txt, MetDefaults.txt, para_ini.txt,\n")
  cat("               P/T data (ASCII & binary), Qobs.txt, radmat.par\n")
  cat("  Example outputs already included\n")
  cat("\nTo visualize:\n")
  cat("  launch_cosero_app('", project_path, "')\n", sep = "")
  cat("\nTo run new simulation:\n")
  cat("  run_cosero('", project_path, "')\n", sep = "")

  invisible(project_path)
}


#' Setup New COSERO Project Directory
#'
#' Creates an empty COSERO project structure with binaries and configuration.
#' For a working example, use \code{\link{setup_cosero_project_example}} instead.
#'
#' @param project_path Path where the new COSERO project should be created.
#' @param cosero_bin_source Source of COSERO binaries:
#'   "package" (default), path to folder with binaries, or path to zip file.
#' @param create_defaults Create a defaults.txt file? (default: TRUE)
#' @param defaults_settings Optional list of settings for defaults.txt.
#' @param overwrite Overwrite existing files? (default: FALSE)
#' @param quiet Suppress progress messages? (default: FALSE)
#'
#' @return Invisibly returns a list with paths and files copied.
#'
#' @details
#' Creates basic project structure with COSERO.exe, DLLs, and empty input/output
#' folders. You'll need to add input files manually. See \code{\link{show_required_files}}
#' for what's needed.
#'
#' @export
#' @examples
#' \dontrun{
#' # Most users should use the example instead:
#' setup_cosero_project_example("C:/COSERO_example")
#'
#' # Advanced: Create empty project with custom settings
#' setup_cosero_project(
#'   "C:/my_project",
#'   defaults_settings = list(STARTDATE = "2020 1 1 0 0", OUTPUTTYPE = 2)
#' )
#' }
setup_cosero_project <- function(project_path,
                                 cosero_bin_source = "package",
                                 create_defaults = TRUE,
                                 defaults_settings = NULL,
                                 overwrite = FALSE,
                                 quiet = FALSE) {

  # Validate inputs
  if (is.null(project_path) || nchar(project_path) == 0) {
    stop("project_path must be specified", call. = FALSE)
  }

  # Create main project directory
  if (!dir.exists(project_path)) {
    dir.create(project_path, recursive = TRUE)
    if (!quiet) cat("Created project directory:", project_path, "\n")
  } else {
    if (!quiet) cat("Using existing directory:", project_path, "\n")
  }

  # Create subdirectories
  input_dir <- file.path(project_path, "input")
  output_dir <- file.path(project_path, "output")

  if (!dir.exists(input_dir)) {
    dir.create(input_dir, recursive = TRUE)
    if (!quiet) cat("Created input directory\n")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (!quiet) cat("Created output directory\n")
  }

  # Copy COSERO binaries
  files_copied <- copy_cosero_binaries(
    source = cosero_bin_source,
    destination = project_path,
    overwrite = overwrite,
    quiet = quiet
  )

  # Verify COSERO.exe exists
  exe_path <- file.path(project_path, "COSERO.exe")
  if (!file.exists(exe_path)) {
    stop("COSERO.exe not found after binary installation. Check cosero_bin_source.",
         call. = FALSE)
  }

  # Create defaults.txt if requested
  defaults_file <- NULL
  if (create_defaults) {
    defaults_file <- file.path(input_dir, "defaults.txt")

    if (file.exists(defaults_file) && !overwrite) {
      if (!quiet) cat("defaults.txt already exists (use overwrite=TRUE to replace)\n")
    } else {
      # Use modify_defaults which handles creation
      if (is.null(defaults_settings)) {
        create_default_defaults(defaults_file, quiet = quiet)
      } else {
        # Validate settings first
        validation <- validate_cosero_defaults(defaults_settings)
        if (length(validation$messages) > 0) {
          sapply(validation$messages, message)
        }
        if (!validation$valid) {
          stop("Invalid defaults_settings provided", call. = FALSE)
        }
        # Create with custom settings
        create_default_defaults(defaults_file, quiet = TRUE)
        modify_defaults(defaults_file, validation$settings, quiet = quiet)
      }
    }
  }

  # Return project structure info
  result <- list(
    project_path = project_path,
    input_dir = input_dir,
    output_dir = output_dir,
    defaults_file = defaults_file,
    exe_path = exe_path,
    files_copied = files_copied
  )

  if (!quiet) {
    cat("\n=== COSERO Project Created ===\n")
    cat("Location:", project_path, "\n")
    cat("Executable:", exe_path, "\n")
    if (!is.null(defaults_file)) {
      cat("Config:", defaults_file, "\n")
    }
    cat("\nNext: Add required input files to:", input_dir, "\n")
    cat("  - MetDefaults.txt (meteorological file definitions)\n")
    cat("  - para.txt (model parameters)\n")
    cat("  - Precipitation and temperature files\n")
    cat("  - Runoff observations (DATAFILE)\n")
    cat("\nSee: show_required_files() for details\n")
    cat("Or use setup_cosero_project_example() for a working template\n")
  }

  invisible(result)
}


#' Copy COSERO Binaries to Project
#'
#' Internal function to handle copying COSERO.exe and DLL files from various sources.
#'
#' @param source Source of binaries:
#'   - "package": Extract from package zip
#'   - Path to folder containing binaries
#'   - Path to zip file
#' @param destination Destination folder
#' @param overwrite Overwrite existing files?
#' @param quiet Suppress messages?
#'
#' @return Character vector of copied file names
#' @keywords internal
copy_cosero_binaries <- function(source, destination, overwrite = FALSE, quiet = FALSE) {
  files_copied <- character(0)

  if (source == "package") {
    # Extract from package zip
    zip_path <- system.file("extdata", "cosero_binaries.zip", package = "CoseRo")

    if (!file.exists(zip_path)) {
      stop(
        "Package binary zip not found at: ", zip_path, "\n",
        "Please ensure inst/extdata/cosero_binaries.zip exists in the package,\n",
        "or specify cosero_bin_source as a path to COSERO binaries.",
        call. = FALSE
      )
    }

    if (!quiet) cat("Extracting COSERO binaries from package...\n")

    # List files in zip
    zip_contents <- utils::unzip(zip_path, list = TRUE)

    # Extract only root-level files (not in subdirectories)
    root_files <- zip_contents$Name[!grepl("/", zip_contents$Name)]

    if (length(root_files) == 0) {
      stop("No files found in cosero_binaries.zip", call. = FALSE)
    }

    # Extract to destination
    utils::unzip(zip_path, files = root_files, exdir = destination, overwrite = overwrite)
    files_copied <- root_files

    if (!quiet) cat("Extracted", length(files_copied), "files\n")

  } else if (file.exists(source) && grepl("\\.zip$", source, ignore.case = TRUE)) {
    # Extract from custom zip file
    if (!quiet) cat("Extracting binaries from:", source, "\n")

    zip_contents <- utils::unzip(source, list = TRUE)
    root_files <- zip_contents$Name[!grepl("/", zip_contents$Name)]

    if (length(root_files) == 0) {
      stop("No root-level files found in zip:", source, call. = FALSE)
    }

    utils::unzip(source, files = root_files, exdir = destination, overwrite = overwrite)
    files_copied <- root_files

    if (!quiet) cat("Extracted", length(files_copied), "files\n")

  } else if (dir.exists(source)) {
    # Copy from directory
    if (!quiet) cat("Copying binaries from:", source, "\n")

    # Look for exe and dll files
    bin_files <- list.files(source, pattern = "\\.(exe|dll)$", ignore.case = TRUE, full.names = FALSE)

    if (length(bin_files) == 0) {
      stop("No .exe or .dll files found in:", source, call. = FALSE)
    }

    # Copy each file
    for (file in bin_files) {
      src_path <- file.path(source, file)
      dest_path <- file.path(destination, file)

      if (file.exists(dest_path) && !overwrite) {
        if (!quiet) cat("Skipping", file, "(already exists)\n")
      } else {
        file.copy(src_path, dest_path, overwrite = overwrite)
        files_copied <- c(files_copied, file)
        if (!quiet) cat("Copied:", file, "\n")
      }
    }

  } else {
    stop(
      "Invalid cosero_bin_source: '", source, "'\n",
      "Must be:\n",
      "  - 'package' (to use bundled binaries)\n",
      "  - Path to folder containing COSERO.exe and DLLs\n",
      "  - Path to zip file with binaries",
      call. = FALSE
    )
  }

  return(files_copied)
}


#' Copy Template Files to Project
#'
#' Internal function to copy example/template input files to a new project.
#'
#' @param input_dir Destination input directory
#' @param overwrite Overwrite existing files?
#' @param quiet Suppress messages?
#'
#' @keywords internal
copy_template_files <- function(input_dir, overwrite = FALSE, quiet = FALSE) {
  # Check for template files in package
  template_path <- system.file("extdata", "template_input", package = "CoseRo")

  if (!dir.exists(template_path)) {
    if (!quiet) {
      message("No template files available in package (inst/extdata/template_input/)")
    }
    return(invisible(NULL))
  }

  # Copy template files
  template_files <- list.files(template_path, full.names = FALSE)

  if (length(template_files) == 0) {
    if (!quiet) message("Template folder is empty")
    return(invisible(NULL))
  }

  if (!quiet) cat("Copying", length(template_files), "template files...\n")

  for (file in template_files) {
    src <- file.path(template_path, file)
    dest <- file.path(input_dir, file)

    if (file.exists(dest) && !overwrite) {
      if (!quiet) cat("Skipping", file, "(already exists)\n")
    } else {
      file.copy(src, dest, overwrite = overwrite)
      if (!quiet) cat("Copied:", file, "\n")
    }
  }

  invisible(NULL)
}


#' Show Required COSERO Input Files
#'
#' Displays a structured overview of required input files for COSERO projects,
#' grouped by fixed-name control files and user-defined data files.
#' Includes formatting requirements and configuration notes.
#'
#' @return Invisibly returns a data frame with file information.
#' @export
#' @examples
#' \dontrun{
#' show_required_files()
#' }
show_required_files <- function() {

  cat("=== COSERO Input File Requirements ===\n")

  # --- Section 1: Fixed-name control files ---
  cat("\nFIXED-NAME CONTROL FILES\n")
  cat("  These files must use the exact names shown below.\n\n")

  control_files <- list(
    list(
      name = "defaults.txt",
      desc = "Master configuration file linking simulation dates, land use classes,\n",
      note = "      and other filenames."
    ),
    list(
      name = "MetDefaults.txt",
      desc = "Defines meteorological data sources, the ETP calculation method,\n",
      note = "      and the data format (ASCII vs. BIN)."
    ),
    list(
      name = "radmat.par",
      desc = "Average hourly diurnal cycle of solar radiation (W/m^2).\n",
      note = "      Format: 12 rows (months) x 24 columns (hours)."
    ),
    list(
      name = "raster_write.txt",
      desc = "Controls writing of state/flux rasters for external tools."
    )
  )

  for (f in control_files) {
    cat(sprintf("  %-20s %s", f$name, f$desc))
    if (!is.null(f$note))  cat(f$note)
    if (!is.null(f$note2)) cat(f$note2)
    cat("\n")
  }

  # --- Section 2: User-defined data files ---
  cat("\nUSER-DEFINED ASCII DATA FILES\n")
  cat("  Names for these files are specified within defaults.txt or MetDefaults.txt.\n\n")

  cat(sprintf("  %-20s %-18s %s\n", "File Type", "Mapping Keyword", "Required ASCII Formatting"))
  cat(sprintf("  %-20s %-18s %s\n",
              "--------------------", "------------------",
              "---------------------------------------------------"))

  data_files <- list(
    list(
      type    = "Parameter File",
      keyword = "PARAFILE",
      format  = "169 columns; Row 1: Project Name; Row 2: Headers; Row 3+: one row per zone (NZ)."
    ),
    list(
      type    = "Precipitation",
      keyword = "PRECFILE",
      format  = "No header. Columns: YYYY MM DD hh mm followed by one column per zone (NZ)."
    ),
    list(
      type    = "Temperature",
      keyword = "TEMPFILE",
      format  = "No header. Format identical to the Precipitation file."
    ),
    list(
      type    = "Runoff Obs",
      keyword = "DATAFILE",
      format  = "Header line + YYYY MM DD hh mm + one column per subbasin. Must use blanks (not tabs)."
    )
  )

  for (f in data_files) {
    cat(sprintf("  %-20s %-18s %s\n", f$type, f$keyword, f$format))
  }

  # --- Section 3: Performance and formatting notes ---
  cat("\nPERFORMANCE & FORMATTING NOTES\n\n")

  notes <- c(
    "Binary Mode:      Set ASCIIorBIN to 1 in MetDefaults.txt to use binary files.\n                  Recommended for larger modeling domains (significant speed increase).",
    "Missing Data:     In the runoff file (DATAFILE), use -0.01 or any value < 0\n                  for missing observations.",
    "ETP Options:      If ETPCONTROL is 0, the model uses the Thornthwaite method\n                  (requires TMMon parameters in PARAFILE).\n                  If ETPCONTROL is 1, an external ETPFILE (same format as Precip)\n                  must be provided.",
    "External Inflow:  If ADDFLUXCONT is 1, an ADDFLUXFILE is required, starting\n                  with an NB-TONZ mapping header followed by the time series."
  )

  for (note in notes) {
    cat(sprintf("  %s\n\n", note))
  }

  # Return data invisibly for programmatic use
  files_df <- data.frame(
    Category = c(
      rep("CONTROL", 4),
      rep("DATA", 4)
    ),
    Filename = c(
      "defaults.txt", "MetDefaults.txt", "radmat.par", "raster_write.txt",
      "PARAFILE", "PRECFILE", "TEMPFILE", "DATAFILE"
    ),
    Description = c(
      "Master configuration file",
      "Meteorological data source and format definitions",
      "Solar radiation diurnal cycle (12 months x 24 hours)",
      "Raster output control",
      "Model parameters (169 columns x NZ+2 rows)",
      "Precipitation time series (no header, one column per zone)",
      "Temperature time series (identical format to PRECFILE)",
      "Runoff observations (header + one column per subbasin, blanks not tabs)"
    ),
    stringsAsFactors = FALSE
  )

  invisible(files_df)
}


#' List Package Binary Contents
#'
#' Show what files are included in the package's cosero_binaries.zip.
#' Useful for troubleshooting and verification.
#'
#' @return Data frame with file names and sizes, or NULL if zip not found
#' @export
#' @examples
#' \dontrun{
#' # Check what binaries are included
#' list_package_binaries()
#' }
list_package_binaries <- function() {
  zip_path <- system.file("extdata", "cosero_binaries.zip", package = "CoseRo")

  if (!file.exists(zip_path)) {
    message("Package binary zip not found at: ", zip_path)
    message("The cosero_binaries.zip should be located at: inst/extdata/cosero_binaries.zip")
    return(invisible(NULL))
  }

  cat("Binary zip location:", zip_path, "\n")
  cat("Zip size:", format(file.info(zip_path)$size / 1024^2, digits = 2), "MB\n\n")

  contents <- utils::unzip(zip_path, list = TRUE)
  cat("Contents:\n")
  print(contents)

  invisible(contents)
}
