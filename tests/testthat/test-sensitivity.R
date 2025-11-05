# Tests for sensitivity analysis functions
# File: tests/testthat/test-sensitivity.R

test_that("load_parameter_bounds loads default bounds file", {
  # This will use the default parameter_bounds.csv from inst/extdata/
  # In tests, the package may not be installed, so we'll skip if file not found
  skip_if_not(file.exists(system.file("extdata", "parameter_bounds.csv", package = "COSERO")),
              "Default parameter bounds file not found")

  bounds <- load_parameter_bounds()

  expect_s3_class(bounds, "data.frame")
  expect_true("parameter" %in% colnames(bounds))
  expect_true("min" %in% colnames(bounds))
  expect_true("max" %in% colnames(bounds))
  expect_true("default" %in% colnames(bounds))
  expect_true("modification_type" %in% colnames(bounds))
})

test_that("load_parameter_bounds filters by parameter names", {
  # Create temporary bounds file
  temp_file <- tempfile(fileext = ".csv")
  test_bounds <- data.frame(
    parameter = c("BETA", "CTMAX", "FK"),
    min = c(0.1, 2, 0.08),
    max = c(10, 12, 1),
    default = c(4.5, 5, 1),
    modification_type = c("absval", "absval", "absval")
  )
  write.csv(test_bounds, temp_file, row.names = FALSE)

  # Load only specific parameters
  bounds <- load_parameter_bounds(temp_file, parameters = c("BETA", "FK"))

  expect_equal(nrow(bounds), 2)
  expect_true("BETA" %in% bounds$parameter)
  expect_true("FK" %in% bounds$parameter)
  expect_false("CTMAX" %in% bounds$parameter)

  # Clean up
  unlink(temp_file)
})

test_that("create_sobol_bounds creates correct matrix format", {
  # Create test bounds
  test_bounds <- data.frame(
    parameter = c("BETA", "CTMAX"),
    min = c(0.1, 2),
    max = c(10, 12),
    default = c(4.5, 5),
    modification_type = c("absval", "absval")
  )

  bounds_matrix <- create_sobol_bounds(test_bounds)

  expect_s3_class(bounds_matrix, "tbl_df")
  expect_equal(nrow(bounds_matrix), 2)  # min and max rows
  expect_equal(ncol(bounds_matrix), 2)  # BETA and CTMAX columns
  expect_true("BETA" %in% colnames(bounds_matrix))
  expect_true("CTMAX" %in% colnames(bounds_matrix))
  expect_equal(bounds_matrix$BETA[1], 0.1)  # min
  expect_equal(bounds_matrix$BETA[2], 10)   # max
})

test_that("read_parameter_file extracts parameter values correctly", {
  # Create temporary parameter file
  temp_file <- tempfile(fileext = ".txt")
  param_content <- c(
    "BETA",
    "4.5",
    "CTMAX",
    "5.0",
    "FK",
    "1.0"
  )
  writeLines(param_content, temp_file)

  param_names <- c("BETA", "CTMAX", "FK")
  values <- read_parameter_file(temp_file, param_names)

  expect_type(values, "list")
  expect_equal(length(values), 3)
  expect_equal(values$BETA, 4.5)
  expect_equal(values$CTMAX, 5.0)
  expect_equal(values$FK, 1.0)

  # Clean up
  unlink(temp_file)
})

test_that("modify_parameter_file updates absval parameters correctly", {
  # Create temporary parameter file
  temp_file <- tempfile(fileext = ".txt")
  param_content <- c(
    "BETA",
    "4.5",
    "CTMAX",
    "5.0"
  )
  writeLines(param_content, temp_file)

  # Create test bounds with absval modification
  par_bounds <- data.frame(
    parameter = c("BETA", "CTMAX"),
    modification_type = c("absval", "absval")
  )

  # Create parameter changes
  original_values <- list(BETA = 4.5, CTMAX = 5.0)
  new_params <- data.frame(BETA = 7.0, CTMAX = 8.0)

  # Modify file
  modify_parameter_file(temp_file, new_params, par_bounds, original_values)

  # Read back
  lines <- readLines(temp_file)
  expect_equal(lines[2], "7")  # BETA value line
  expect_equal(lines[4], "8")  # CTMAX value line

  # Clean up
  unlink(temp_file)
})

test_that("modify_parameter_file updates relchg parameters correctly", {
  # Create temporary parameter file
  temp_file <- tempfile(fileext = ".txt")
  param_content <- c(
    "TAB1",
    "2.0"
  )
  writeLines(param_content, temp_file)

  # Create test bounds with relchg modification
  par_bounds <- data.frame(
    parameter = "TAB1",
    modification_type = "relchg"
  )

  # Multiply by 1.5
  original_values <- list(TAB1 = 2.0)
  new_params <- data.frame(TAB1 = 1.5)  # multiplier

  # Modify file
  modify_parameter_file(temp_file, new_params, par_bounds, original_values)

  # Read back
  lines <- readLines(temp_file)
  expect_equal(as.numeric(lines[2]), 3.0)  # 2.0 * 1.5 = 3.0

  # Clean up
  unlink(temp_file)
})

test_that("modify_parameter_file updates abschg parameters correctly", {
  # Create temporary parameter file
  temp_file <- tempfile(fileext = ".txt")
  param_content <- c(
    "TCOR",
    "0.0"
  )
  writeLines(param_content, temp_file)

  # Create test bounds with abschg modification
  par_bounds <- data.frame(
    parameter = "TCOR",
    modification_type = "abschg"
  )

  # Add 2.0
  original_values <- list(TCOR = 0.0)
  new_params <- data.frame(TCOR = 2.0)  # value to add

  # Modify file
  modify_parameter_file(temp_file, new_params, par_bounds, original_values)

  # Read back
  lines <- readLines(temp_file)
  expect_equal(as.numeric(lines[2]), 2.0)  # 0.0 + 2.0 = 2.0

  # Clean up
  unlink(temp_file)
})
