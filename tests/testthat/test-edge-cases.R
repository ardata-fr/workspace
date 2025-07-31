test_that("handles special characters in names and paths", {
  z <- new_workspace()
  special_name <- "dataset with spaces & symbols!@#"
  z <- store_dataset(z, iris, special_name)

  retrieved <- read_dataset_in_workspace(z, special_name)
  expect_identical(nrow(retrieved), nrow(iris))
  expect_identical(ncol(retrieved), ncol(iris))
})

test_that("handles unicode characters in names", {
  z <- new_workspace()
  unicode_name <- "données_été_café"
  z <- store_dataset(z, iris, unicode_name)

  objects <- list_object_in_workspace(z)
  expect_true(unicode_name %in% objects$name)

  retrieved <- read_dataset_in_workspace(z, unicode_name)
  expect_identical(nrow(retrieved), nrow(iris))
})

test_that("handles empty workspaces", {
  z <- new_workspace()
  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 0)
  expect_true(is.data.frame(objects))
  expect_true(all(c("file", "name", "subdir", "type", "timestamp") %in% names(objects)))
})

test_that("empty workspace pack/unpack cycle works", {
  z <- new_workspace()

  packed <- tempfile(fileext = ".zip")
  pack_workspace(z, packed)

  z2 <- unpack_workspace(packed)
  objects <- list_object_in_workspace(z2)

  expect_equal(nrow(objects), 0)
  expect_s3_class(z2, "workspace")
})

test_that("handles very long object names", {
  z <- new_workspace()
  long_name <- paste(rep("very_long_name", 10), collapse = "_")
  z <- store_dataset(z, iris, long_name)

  objects <- list_object_in_workspace(z)
  expect_true(long_name %in% objects$name)

  retrieved <- read_dataset_in_workspace(z, long_name)
  expect_identical(nrow(retrieved), nrow(iris))
})

test_that("handles datasets with zero rows", {
  z <- new_workspace()
  empty_df <- data.frame(x = numeric(0), y = character(0))
  z <- store_dataset(z, empty_df, "empty_dataset")

  retrieved <- read_dataset_in_workspace(z, "empty_dataset")
  expect_equal(nrow(retrieved), 0)
  expect_equal(ncol(retrieved), 2)
})

test_that("handles datasets with many columns", {
  z <- new_workspace()
  wide_df <- data.frame(matrix(runif(100), nrow = 10, ncol = 10))
  names(wide_df) <- paste0("col_", 1:10)
  z <- store_dataset(z, wide_df, "wide_dataset")

  retrieved <- read_dataset_in_workspace(z, "wide_dataset")
  expect_equal(ncol(retrieved), 10)
  expect_equal(nrow(retrieved), 10)
})

test_that("handles JSON with special characters", {
  z <- new_workspace()
  special_json <- '{"special": "chars: àáâãäå, 中文, 🎉"}'
  z <- store_json(z, special_json, "special.json", subdir = "test")

  retrieved <- read_json_str_in_workspace(z, "special", subdir = "test")
  expect_equal(retrieved, special_json)
})

test_that("handles empty JSON strings", {
  z <- new_workspace()
  empty_json <- "{}"
  z <- store_json(z, empty_json, "empty.json", subdir = "test")

  retrieved <- read_json_str_in_workspace(z, "empty", subdir = "test")
  expect_equal(retrieved, empty_json)
})

test_that("handles deeply nested subdirectories", {
  z <- new_workspace()
  deep_subdir <- "level1/level2/level3/level4"
  z <- store_json(z, '{"deep": "nested"}', "deep.json", subdir = deep_subdir)

  objects <- list_object_in_workspace(z)
  expect_true(deep_subdir %in% objects$subdir)

  retrieved <- read_json_str_in_workspace(z, "deep", subdir = deep_subdir)
  expect_equal(retrieved, '{"deep": "nested"}')
})

test_that("handles mixed data types in same workspace after operations", {
  z <- new_workspace()

  z <- store_dataset(z, iris, "iris")
  z <- store_json(z, '{"test": "data"}', "config.json", subdir = "config")
  z <- store_rds(z, list(x = 1:5), "data.rds", subdir = "rdata")

  z <- delete_dataset(z, "iris")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 2)
  expect_true(all(c("config", "data") %in% objects$name))
})
