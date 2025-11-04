# Tests for COSERO run functions
# File: tests/testthat/test-cosero_run.R

test_that("validate_cosero_defaults validates parameter types correctly", {
  # Test valid settings
  valid_settings <- list(
    SPINUP = 365,
    OUTPUTTYPE = 3,
    SC_FLAG = 0
  )

  validation <- validate_cosero_defaults(valid_settings)
  expect_true(validation$valid)
  expect_length(validation$messages, 0)
})

test_that("validate_cosero_defaults catches invalid integer parameters", {
  # Test invalid integer (non-numeric)
  invalid_settings <- list(
    SPINUP = "not_a_number"
  )

  validation <- validate_cosero_defaults(invalid_settings)
  expect_false(validation$valid)
  expect_true(length(validation$messages) > 0)
})

test_that("validate_cosero_defaults catches invalid OUTPUTTYPE values", {
  # Test OUTPUTTYPE out of range
  invalid_settings <- list(
    OUTPUTTYPE = 5  # Must be 0, 1, 2, or 3
  )

  validation <- validate_cosero_defaults(invalid_settings)
  expect_false(validation$valid)
  expect_match(validation$messages, "OUTPUTTYPE must be 0, 1, 2, or 3")
})

test_that("validate_cosero_defaults validates flag parameters", {
  # Test valid flag
  valid_flag <- list(SC_FLAG = 1)
  validation <- validate_cosero_defaults(valid_flag)
  expect_true(validation$valid)

  # Test invalid flag
  invalid_flag <- list(SC_FLAG = 2)
  validation <- validate_cosero_defaults(invalid_flag)
  expect_false(validation$valid)
  expect_match(validation$messages, "must be 0 or 1")
})

test_that("validate_cosero_defaults validates date format", {
  # Test valid date as string
  valid_date_string <- list(
    STARTDATE = "2015 1 1 0 0"
  )
  validation <- validate_cosero_defaults(valid_date_string)
  expect_true(validation$valid)
  expect_length(validation$settings$STARTDATE, 5)

  # Test valid date as vector
  valid_date_vector <- list(
    STARTDATE = c(2015, 1, 1, 0, 0)
  )
  validation <- validate_cosero_defaults(valid_date_vector)
  expect_true(validation$valid)

  # Test invalid date (wrong number of elements)
  invalid_date <- list(
    STARTDATE = c(2015, 1, 1)  # Only 3 elements instead of 5
  )
  validation <- validate_cosero_defaults(invalid_date)
  expect_false(validation$valid)
  expect_match(validation$messages, "must have 5 elements")
})

test_that("validate_cosero_defaults catches end date before start date", {
  # Test end date before start date
  invalid_dates <- list(
    STARTDATE = "2020 1 1 0 0",
    ENDDATE = "2019 1 1 0 0"
  )

  validation <- validate_cosero_defaults(invalid_dates)
  expect_false(validation$valid)
  expect_match(validation$messages, "ENDDATE must be after STARTDATE")
})

test_that("get_default_cosero_values returns expected defaults", {
  defaults <- get_default_cosero_values()

  expect_type(defaults, "list")
  expect_true("SPINUP" %in% names(defaults))
  expect_true("OUTPUTTYPE" %in% names(defaults))
  expect_equal(defaults$SPINUP, 365)
  expect_equal(defaults$OUTPUTTYPE, 1)
})

test_that("run_cosero rejects non-existent project path", {
  expect_error(
    run_cosero(project_path = "/path/does/not/exist"),
    "Project path does not exist"
  )
})

test_that("run_cosero rejects missing COSERO.exe", {
  # Create temporary directory without COSERO.exe
  temp_dir <- tempdir()
  test_project <- file.path(temp_dir, "test_project_no_exe")
  dir.create(test_project, showWarnings = FALSE)

  expect_error(
    run_cosero(project_path = test_project),
    "COSERO executable not found"
  )

  # Clean up
  unlink(test_project, recursive = TRUE)
})

test_that("run_cosero validates statevar_source parameter", {
  # Create temporary project directory with COSERO.exe
  temp_dir <- tempdir()
  test_project <- file.path(temp_dir, "test_project_statevar")
  dir.create(test_project, showWarnings = FALSE)
  file.create(file.path(test_project, "COSERO.exe"))

  expect_error(
    run_cosero(project_path = test_project, statevar_source = 3),
    "statevar_source must be 1.*or 2"
  )

  # Clean up
  unlink(test_project, recursive = TRUE)
})

test_that("run_cosero validates tmmon_option parameter", {
  # Create temporary project directory with COSERO.exe
  temp_dir <- tempdir()
  test_project <- file.path(temp_dir, "test_project_tmmon")
  dir.create(test_project, showWarnings = FALSE)
  file.create(file.path(test_project, "COSERO.exe"))

  expect_error(
    run_cosero(project_path = test_project, tmmon_option = 5),
    "tmmon_option must be 1.*or 2"
  )

  # Clean up
  unlink(test_project, recursive = TRUE)
})
