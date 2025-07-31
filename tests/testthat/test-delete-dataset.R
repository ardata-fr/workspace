test_that("delete_dataset removes datasets correctly", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "iris_test")
  z <- store_dataset(z, mtcars, "mtcars_test")

  initial_objects <- list_object_in_workspace(z)
  expect_equal(nrow(initial_objects), 2)

  z <- delete_dataset(z, "iris_test")
  objects <- list_object_in_workspace(z)

  expect_false("iris_test" %in% objects$name)
  expect_true("mtcars_test" %in% objects$name)
  expect_equal(nrow(objects), 1)
})

test_that("delete_dataset removes physical parquet files", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "iris_to_delete")

  objects <- list_object_in_workspace(z)
  file_path <- file.path(z$dir, objects$file[objects$name == "iris_to_delete"])
  expect_true(file.exists(file_path))

  z <- delete_dataset(z, "iris_to_delete")
  expect_false(file.exists(file_path))
})

test_that("delete_dataset handles datasets with special characters in names", {
  z <- new_workspace()
  special_name <- "dataset with spaces & symbols"
  z <- store_dataset(z, iris, special_name)

  z <- delete_dataset(z, special_name)
  objects <- list_object_in_workspace(z)

  expect_false(special_name %in% objects$name)
})

test_that("delete_dataset handles datasets with .parquet extension in name", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "test.parquet")

  z <- delete_dataset(z, "test.parquet")
  objects <- list_object_in_workspace(z)

  expect_false("test.parquet" %in% objects$name)
})

test_that("delete_dataset handles datasets without .parquet extension", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "test_dataset")

  z <- delete_dataset(z, "test_dataset")
  objects <- list_object_in_workspace(z)

  expect_false("test_dataset" %in% objects$name)
})

test_that("delete_dataset updates workspace object descriptions", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "iris_dataset")
  z <- store_dataset(z, mtcars, "mtcars_dataset")

  initial_desc <- list_object_in_workspace(z)
  expect_equal(nrow(initial_desc), 2)

  z <- delete_dataset(z, "iris_dataset")
  updated_desc <- list_object_in_workspace(z)

  expect_equal(nrow(updated_desc), 1)
  expect_equal(updated_desc$name, "mtcars_dataset")
})

test_that("delete_dataset works after pack/unpack cycle", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "iris_dataset")
  z <- store_dataset(z, mtcars, "mtcars_dataset")

  packed_file <- tempfile(fileext = ".zip")
  pack_workspace(z, packed_file)
  z2 <- unpack_workspace(packed_file)

  z2 <- delete_dataset(z2, "iris_dataset")
  objects <- list_object_in_workspace(z2)

  expect_false("iris_dataset" %in% objects$name)
  expect_true("mtcars_dataset" %in% objects$name)
})

test_that("delete_dataset handles nonexistent dataset gracefully", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "iris")

  # Should not error when trying to delete nonexistent dataset
  z_after <- delete_dataset(z, "nonexistent_dataset")
  objects <- list_object_in_workspace(z_after)

  # Original dataset should still be there
  expect_true("iris" %in% objects$name)
  expect_equal(nrow(objects), 1)
})

test_that("delete_dataset preserves other data types", {
  z <- new_workspace()

  # Store different types of objects
  z <- store_dataset(z, iris, "iris_dataset")
  z <- store_json(z, '{"config": "data"}', "config.json", subdir = "config")
  z <- store_rds(z, list(model = "test"), "model.rds", subdir = "models")

  initial_objects <- list_object_in_workspace(z)
  expect_equal(nrow(initial_objects), 3)

  # Delete only the dataset
  z <- delete_dataset(z, "iris_dataset")
  objects <- list_object_in_workspace(z)

  expect_equal(nrow(objects), 2)
  expect_false("iris_dataset" %in% objects$name)
  expect_true(all(c("config", "model") %in% objects$name))

  # Verify non-dataset objects are intact
  expect_equal(read_json_str_in_workspace(z, "config", subdir = "config"), '{"config": "data"}')
  expect_equal(read_rds_in_workspace(z, "model", subdir = "models"), list(model = "test"))
})

test_that("delete_dataset handles multiple deletions", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "dataset1")
  z <- store_dataset(z, mtcars, "dataset2")
  z <- store_dataset(z, cars, "dataset3")

  expect_equal(nrow(list_object_in_workspace(z)), 3)

  # Delete datasets one by one
  z <- delete_dataset(z, "dataset1")
  expect_equal(nrow(list_object_in_workspace(z)), 2)
  expect_false("dataset1" %in% list_object_in_workspace(z)$name)

  z <- delete_dataset(z, "dataset3")
  expect_equal(nrow(list_object_in_workspace(z)), 1)
  expect_false("dataset3" %in% list_object_in_workspace(z)$name)

  z <- delete_dataset(z, "dataset2")
  expect_equal(nrow(list_object_in_workspace(z)), 0)
})

test_that("delete_dataset preserves timestamps of remaining datasets", {
  z <- new_workspace()

  timestamp1 <- "2024-01-01 10:00:00"
  timestamp2 <- "2024-01-02 15:30:00"

  z <- store_dataset(z, iris, "iris", timestamp = timestamp1)
  z <- store_dataset(z, mtcars, "mtcars", timestamp = timestamp2)

  z <- delete_dataset(z, "iris")

  # Check that remaining dataset's timestamp is preserved
  remaining_timestamp <- read_timestamp(z, "mtcars", "dataset", subdir = "datasets")
  expect_equal(remaining_timestamp, timestamp2)
})

test_that("delete_dataset handles unicode characters in names", {
  z <- new_workspace()
  unicode_name <- "données_été_café"
  z <- store_dataset(z, iris, unicode_name)

  z <- delete_dataset(z, unicode_name)
  objects <- list_object_in_workspace(z)

  expect_false(unicode_name %in% objects$name)
  expect_equal(nrow(objects), 0)
})

# test_that("delete_dataset works with very long dataset names", {
#   z <- new_workspace()
#   long_name <- paste(rep("very_long_dataset_name", 10), collapse = "_")
#   z <- store_dataset(z, iris, long_name)
#
#   z <- delete_dataset(z, long_name)
#   objects <- list_object_in_workspace(z)
#
#   expect_false(long_name %in% objects$name)
#   expect_equal(nrow(objects), 0)
# })

test_that("delete_dataset only affects datasets, not geospatial data", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Store regular dataset
  z <- store_dataset(z, iris, "regular_data")

  # Store geospatial dataset with same name pattern
  points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), crs = st_crs(4326))
  sf_data <- st_sf(id = 1:2, value = c("A", "B"), geometry = points)
  z <- store_dataset(z, sf_data, "geo_data")

  initial_objects <- list_object_in_workspace(z)
  expect_equal(nrow(initial_objects), 2)

  # Delete the regular dataset
  z <- delete_dataset(z, "regular_data")
  objects <- list_object_in_workspace(z)

  # Should only affect the regular dataset, not geospatial
  expect_equal(nrow(objects), 1)
  expect_false("regular_data" %in% objects$name)
  expect_true("geo_data" %in% objects$name)
  expect_equal(objects$type, "geospatial")
})

test_that("delete_dataset handles empty workspace", {
  z <- new_workspace()

  # Try to delete from empty workspace
  z_after <- delete_dataset(z, "nonexistent")
  objects <- list_object_in_workspace(z_after)

  expect_equal(nrow(objects), 0)
})

test_that("delete_dataset maintains workspace structure integrity", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "test_dataset")

  # Verify initial structure
  expect_true(dir.exists(file.path(z$dir, "datasets")))
  expect_true(file.exists(file.path(z$dir, "dataset_description.parquet")))

  z <- delete_dataset(z, "test_dataset")

  # Verify structure is maintained even after deletion
  expect_true(dir.exists(file.path(z$dir, "datasets")))
  expect_true(file.exists(file.path(z$dir, "dataset_description.parquet")))
  expect_s3_class(z, "workspace")
})

test_that("delete_dataset works with datasets containing dots in names", {
  z <- new_workspace()
  dotted_name <- "my.dataset.with.dots"
  z <- store_dataset(z, iris, dotted_name)

  z <- delete_dataset(z, dotted_name)
  objects <- list_object_in_workspace(z)

  expect_false(dotted_name %in% objects$name)
})

test_that("delete_dataset case sensitivity", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "MyDataset")

  # Should not delete with different case
  z_after <- delete_dataset(z, "mydataset")
  objects <- list_object_in_workspace(z_after)
  expect_true("MyDataset" %in% objects$name)

  # Should delete with exact case
  z_final <- delete_dataset(z_after, "MyDataset")
  final_objects <- list_object_in_workspace(z_final)
  expect_false("MyDataset" %in% final_objects$name)
})

