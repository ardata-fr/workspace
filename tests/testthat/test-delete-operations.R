test_that("removes datasets correctly", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "iris_test")
  z <- store_dataset(z, mtcars, "mtcars_test")

  initial_objects <- list_object_in_workspace(z)
  expect_equal(nrow(initial_objects), 2)

  z <- rm_object_in_workspace(z, "iris_test", type = "dataset")
  objects <- list_object_in_workspace(z)

  expect_false("iris_test" %in% objects$name)
  expect_true("mtcars_test" %in% objects$name)
  expect_equal(nrow(objects), 1)
})

test_that("removes physical files", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "iris_to_delete")

  objects <- list_object_in_workspace(z)
  file_path <- file.path(z$dir, objects$file[objects$name == "iris_to_delete"])
  expect_true(file.exists(file_path))

  z <- rm_object_in_workspace(z, "iris_to_delete", type = "dataset")
  expect_false(file.exists(file_path))
})

test_that("handles datasets with special characters in names", {
  z <- new_workspace()
  special_name <- "dataset with spaces & symbols"
  z <- store_dataset(z, iris, special_name)

  z <- rm_object_in_workspace(z, name = special_name, type = "dataset")
  objects <- list_object_in_workspace(z)

  expect_false(special_name %in% objects$name)
})

test_that("handles datasets with .parquet extension in name", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "test.parquet")

  z <- rm_object_in_workspace(z, "test.parquet", type = "dataset")
  objects <- list_object_in_workspace(z)

  expect_false("test.parquet" %in% objects$name)
})

test_that("updates workspace object descriptions", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "iris_dataset")
  z <- store_dataset(z, mtcars, "mtcars_dataset")

  initial_desc <- list_object_in_workspace(z)
  expect_equal(nrow(initial_desc), 2)

  z <- rm_object_in_workspace(z, "iris_dataset", type = "dataset")
  updated_desc <- list_object_in_workspace(z)

  expect_equal(nrow(updated_desc), 1)
  expect_equal(updated_desc$name, "mtcars_dataset")
})

test_that("works after pack/unpack cycle", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "iris_dataset")
  z <- store_dataset(z, mtcars, "mtcars_dataset")

  packed_file <- tempfile(fileext = ".zip")
  pack_workspace(z, packed_file)
  z2 <- unpack_workspace(packed_file)

  z2 <- rm_object_in_workspace(z2, "iris_dataset", "dataset")
  objects <- list_object_in_workspace(z2)

  expect_false("iris_dataset" %in% objects$name)
  expect_true("mtcars_dataset" %in% objects$name)
})

test_that("removes JSON objects correctly", {
  z <- new_workspace()
  z <- store_json(z, '{"test": "data1"}', "config1.json", subdir = "config")
  z <- store_json(z, '{"test": "data2"}', "config2.json", subdir = "config")

  initial_objects <- list_object_in_workspace(z)
  expect_equal(nrow(initial_objects), 2)

  z <- rm_object_in_workspace(z, "config1", type = "json", subdir = "config")
  objects <- list_object_in_workspace(z)

  expect_false("config1" %in% objects$name)
  expect_true("config2" %in% objects$name)
  expect_equal(nrow(objects), 1)
})

test_that("removes JSON physical files", {
  z <- new_workspace()
  z <- store_json(z, '{"test": "data"}', "test.json", subdir = "test")

  objects <- list_object_in_workspace(z)
  file_path <- file.path(z$dir, objects$file[objects$name == "test"])
  expect_true(file.exists(file_path))

  z <- rm_object_in_workspace(z, "test", type = "json", subdir = "test")
  expect_false(file.exists(file_path))
})

test_that("handles JSON with special characters", {
  z <- new_workspace()
  special_name <- "json with spaces & symbols"
  z <- store_json(z, '{"special": "data"}', "special.json", name = special_name, subdir = "test")

  z <- rm_object_in_workspace(z, special_name, type = "json", subdir = "test")
  objects <- list_object_in_workspace(z)

  expect_false(special_name %in% objects$name)
})

test_that("removes RDS objects correctly", {
  z <- new_workspace()
  z <- store_rds(z, list(data = 1), "obj1.rds", subdir = "objects")
  z <- store_rds(z, list(data = 2), "obj2.rds", subdir = "objects")

  initial_objects <- list_object_in_workspace(z)
  expect_equal(nrow(initial_objects), 2)

  z <- rm_object_in_workspace(z, "obj1", type = "rds", subdir = "objects")
  objects <- list_object_in_workspace(z)

  expect_false("obj1" %in% objects$name)
  expect_true("obj2" %in% objects$name)
  expect_equal(nrow(objects), 1)
})

test_that("removes RDS physical files", {
  z <- new_workspace()
  z <- store_rds(z, list(test = "data"), "test.rds", subdir = "rdata")

  objects <- list_object_in_workspace(z)
  file_path <- file.path(z$dir, objects$file[objects$name == "test"])
  expect_true(file.exists(file_path))

  z <- rm_object_in_workspace(z, "test", type = "rds", subdir = "rdata")
  expect_false(file.exists(file_path))
})

test_that("handles RDS with custom names", {
  z <- new_workspace()
  z <- store_rds(z, list(value = 42), "data.rds", name = "custom_rds_name", subdir = "rdata")
  z <- rm_object_in_workspace(z, "custom_rds_name", type = "rds", subdir = "rdata")
  objects <- list_object_in_workspace(z)

  expect_false("custom_rds_name" %in% objects$name)
})

test_that("handles mixed object types", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "iris")
  z <- store_json(z, '{"config": "data"}', "config.json", subdir = "config")
  z <- store_rds(z, list(model = "test"), "model.rds", subdir = "models")

  initial_objects <- list_object_in_workspace(z)
  expect_equal(nrow(initial_objects), 3)

  # Remove JSON object
  z <- rm_object_in_workspace(z, "config", type = "json", subdir = "config")
  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 2)
  expect_false("config" %in% objects$name)

  # Remove RDS object
  z <- rm_object_in_workspace(z, "model", type = "rds", subdir = "models")
  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 1)
  expect_false("model" %in% objects$name)

  # Dataset should remain
  expect_true("iris" %in% objects$name)

  # Verify remaining data is intact
  iris_data <- read_dataset_in_workspace(z, "iris")
  expect_equal(nrow(iris_data), nrow(iris))
})
