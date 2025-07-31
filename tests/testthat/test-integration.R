test_that("complete workflow with multiple data types", {
  z <- new_workspace()

  # Add datasets, JSON, and RDS
  z <- store_dataset(z, iris, "iris")
  z <- store_dataset(z, mtcars, "mtcars")
  z <- store_json(z, '{"config": "test_data", "version": 1}', "config.json", subdir = "config")
  z <- store_rds(z, list(x = 1:10, y = letters[1:5]), "data.rds", subdir = "rdata")

  # Verify initial state
  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 4)

  # Pack and unpack
  packed <- tempfile(fileext = ".zip")
  pack_workspace(z, packed)
  z2 <- unpack_workspace(packed)

  # Verify all data types preserved
  expect_identical(read_dataset_in_workspace(z2, "iris"), tibble::as_tibble(iris))
  expect_identical(read_dataset_in_workspace(z2, "mtcars"), tibble::as_tibble(mtcars))
  expect_identical(read_json_str_in_workspace(z2, "config", subdir = "config"), '{"config": "test_data", "version": 1}')
  expect_identical(read_rds_in_workspace(z2, "data", subdir = "rdata"), list(x = 1:10, y = letters[1:5]))
})

test_that("complex workspace merge scenario", {
  # Create workspace 1 with datasets and JSON
  z1 <- new_workspace()
  z1 <- store_dataset(z1, iris, "iris")
  z1 <- store_json(z1, '{"source": "workspace1"}', "metadata.json", subdir = "meta")

  # Create workspace 2 with different datasets and RDS
  z2 <- new_workspace()
  z2 <- store_dataset(z2, mtcars, "mtcars")
  z2 <- store_rds(z2, list(models = c("lm", "glm")), "models.rds", subdir = "analysis")

  # Create workspace 3 with overlapping and new content
  z3 <- new_workspace()
  z3 <- store_dataset(z3, iris, "iris")  # Same name as z1
  z3 <- store_json(z3, '{"source": "workspace3"}', "metadata.json", subdir = "meta")  # Same name as z1
  z3 <- store_dataset(z3, cars, "cars")

  # Merge operations
  merged_z1_z2 <- workspace_bind(z1, z2)
  final_workspace <- workspace_bind(merged_z1_z2, z3, replace = FALSE)

  objects <- list_object_in_workspace(final_workspace)
  expect_equal(nrow(objects), 5)  # iris (from z1), metadata (from z1), mtcars, models, cars

  # Verify z1 content preserved (replace = FALSE)
  iris_data <- read_dataset_in_workspace(final_workspace, "iris")
  expect_identical(iris_data, tibble::as_tibble(iris))

  metadata_json <- read_json_str_in_workspace(final_workspace, "metadata", subdir = "meta")
  expect_identical(metadata_json, '{"source": "workspace1"}')
})

test_that("full CRUD operations workflow", {
  z <- new_workspace()

  # CREATE
  z <- store_dataset(z, iris, "iris")
  z <- store_dataset(z, mtcars, "mtcars")
  z <- store_json(z, '{"initial": "data"}', "config.json", subdir = "config")
  z <- store_rds(z, list(version = 1), "metadata.rds", subdir = "meta")

  expect_equal(nrow(list_object_in_workspace(z)), 4)

  # READ
  iris_data <- read_dataset_in_workspace(z, "iris")
  config_data <- read_json_str_in_workspace(z, "config", subdir = "config")
  meta_data <- read_rds_in_workspace(z, "metadata", subdir = "meta")

  expect_identical(iris_data, tibble::as_tibble(iris))
  expect_identical(config_data, '{"initial": "data"}')
  expect_identical(meta_data, list(version = 1))

  # UPDATE (replace existing)
  z <- store_json(z, '{"updated": "data"}', "config.json", subdir = "config")
  updated_config <- read_json_str_in_workspace(z, "config", subdir = "config")
  expect_identical(updated_config, '{"updated": "data"}')

  # DELETE
  z <- delete_dataset(z, "mtcars")
  z <- rm_object_in_workspace(z, "metadata", "rds", subdir = "meta")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 2)
  expect_true(all(c("iris", "config") %in% objects$name))
})

test_that("workspace persistence across multiple operations", {
  z <- new_workspace()

  # Build workspace incrementally
  z <- store_dataset(z, iris, "iris")

  packed1 <- tempfile(fileext = ".zip")
  pack_workspace(z, packed1)
  z2 <- unpack_workspace(packed1)

  z2 <- store_dataset(z2, mtcars, "mtcars")
  z2 <- store_json(z2, '{"step": 2}', "progress.json", subdir = "tracking")

  packed2 <- tempfile(fileext = ".zip")
  pack_workspace(z2, packed2)
  z3 <- unpack_workspace(packed2)

  z3 <- store_rds(z3, list(final = TRUE), "final.rds", subdir = "results")
  z3 <- delete_dataset(z3, "iris")

  # Final verification
  final_objects <- list_object_in_workspace(z3)
  expect_equal(nrow(final_objects), 3)
  expect_true(all(c("mtcars", "progress", "final") %in% final_objects$name))

  # Verify data integrity
  expect_identical(read_dataset_in_workspace(z3, "mtcars"), tibble::as_tibble(mtcars))
  expect_identical(read_json_str_in_workspace(z3, "progress", subdir = "tracking"), '{"step": 2}')
  expect_identical(read_rds_in_workspace(z3, "final", subdir = "results"), list(final = TRUE))
})

test_that("timestamp consistency across operations", {
  z <- new_workspace()

  custom_time1 <- "2024-01-01 10:00:00"
  custom_time2 <- "2024-01-02 15:30:00"

  z <- store_dataset(z, iris, "iris", timestamp = custom_time1)
  z <- store_json(z, '{"test": "data"}', "config.json", subdir = "config", timestamp = custom_time2)

  # Pack and unpack
  packed <- tempfile(fileext = ".zip")
  pack_workspace(z, packed)
  z2 <- unpack_workspace(packed)

  # Verify timestamps preserved
  iris_time <- read_timestamp(z2, "iris", "dataset", subdir = "datasets")
  config_time <- read_timestamp(z2, "config", "json", subdir = "config")

  expect_equal(iris_time, custom_time1)
  expect_equal(config_time, custom_time2)

  # Copy workspace and verify timestamps
  z3 <- workspace_copy(z2)
  iris_time_copy <- read_timestamp(z3, "iris", "dataset", subdir = "datasets")
  config_time_copy <- read_timestamp(z3, "config", "json", subdir = "config")

  expect_equal(iris_time_copy, custom_time1)
  expect_equal(config_time_copy, custom_time2)
})
