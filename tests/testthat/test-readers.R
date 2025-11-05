# Tests for COSERO reader functions
# File: tests/testthat/test-readers.R

test_that("detect_outputtype identifies type 1 correctly", {
  # Create temp directory with only basic files (no monitor or var_MET)
  temp_dir <- tempdir()
  output_dir <- file.path(temp_dir, "output_type1")
  dir.create(output_dir, showWarnings = FALSE)

  # Create basic output files
  file.create(file.path(output_dir, "COSERO.runoff"))
  file.create(file.path(output_dir, "statistics.txt"))

  outputtype <- detect_outputtype(output_dir)
  expect_equal(outputtype, 1)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("detect_outputtype identifies type 2 correctly", {
  # Create temp directory with var_MET but no monitor
  temp_dir <- tempdir()
  output_dir <- file.path(temp_dir, "output_type2")
  dir.create(output_dir, showWarnings = FALSE)

  file.create(file.path(output_dir, "var_MET.txt"))

  outputtype <- detect_outputtype(output_dir)
  expect_equal(outputtype, 2)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("detect_outputtype identifies type 3 correctly", {
  # Create temp directory with monitor file
  temp_dir <- tempdir()
  output_dir <- file.path(temp_dir, "output_type3")
  dir.create(output_dir, showWarnings = FALSE)

  file.create(file.path(output_dir, "monitor.txt"))

  outputtype <- detect_outputtype(output_dir)
  expect_equal(outputtype, 3)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("add_datetime_columns creates Date and DateTime columns", {
  # Create test data
  test_data <- data.frame(
    yyyy = c(2020, 2020, 2020),
    mm = c(1, 1, 1),
    dd = c(1, 2, 3),
    hh = c(0, 0, 0),
    value = c(10, 20, 30)
  )

  result <- add_datetime_columns(test_data)

  expect_true("DateTime" %in% colnames(result))
  expect_true("Date" %in% colnames(result))
  expect_s3_class(result$Date, "Date")
  expect_s3_class(result$DateTime, "POSIXct")
})

test_that("handle_missing_values replaces -999 with NA", {
  # Create test data with missing values
  test_data <- data.frame(
    Date = as.Date("2020-01-01"),
    QOBS_0001 = c(10, -999.00, 30),
    QSIM_0001 = c(15, 25, -999.00)
  )

  result <- handle_missing_values(test_data, missing_value = -999.00)

  expect_true(is.na(result$QOBS_0001[2]))
  expect_true(is.na(result$QSIM_0001[3]))
  expect_equal(result$QOBS_0001[1], 10)
})

test_that("list_subbasins extracts subbasin IDs correctly", {
  # Create test data with multiple subbasins
  test_data <- data.frame(
    Date = as.Date("2020-01-01"),
    QOBS_0001 = 10,
    QSIM_0001 = 15,
    QOBS_0003 = 20,
    QSIM_0003 = 25,
    QOBS_0002 = 30,
    QSIM_0002 = 35
  )

  subbasins <- list_subbasins(test_data)

  expect_length(subbasins, 3)
  expect_true("0001" %in% subbasins)
  expect_true("0002" %in% subbasins)
  expect_true("0003" %in% subbasins)
})

test_that("get_subbasin_data extracts correct columns", {
  # Create test data
  test_data <- data.frame(
    yyyy = 2020,
    mm = 1,
    dd = 1,
    hh = 0,
    Date = as.Date("2020-01-01"),
    DateTime = as.POSIXct("2020-01-01"),
    QOBS_0001 = 10,
    QSIM_0001 = 15,
    QOBS_0002 = 20,
    QSIM_0002 = 25
  )

  # Extract subbasin 0001
  result <- get_subbasin_data(test_data, "0001")

  expect_true("Q_obs" %in% colnames(result))
  expect_true("Q_sim" %in% colnames(result))
  expect_equal(result$Q_obs, 10)
  expect_equal(result$Q_sim, 15)
  expect_equal(attr(result, "subbasin_id"), "0001")
})

test_that("get_subbasin_data handles numeric subbasin ID", {
  # Create test data
  test_data <- data.frame(
    Date = as.Date("2020-01-01"),
    QOBS_0005 = 100,
    QSIM_0005 = 150
  )

  # Pass numeric ID
  result <- get_subbasin_data(test_data, 5)

  expect_true("Q_obs" %in% colnames(result))
  expect_equal(result$Q_obs, 100)
  expect_equal(attr(result, "subbasin_id"), "0005")
})

test_that("get_subbasin_data throws error for non-existent subbasin", {
  test_data <- data.frame(
    Date = as.Date("2020-01-01"),
    QOBS_0001 = 10,
    QSIM_0001 = 15
  )

  expect_error(
    get_subbasin_data(test_data, "9999"),
    "Subbasin 9999 not found"
  )
})
