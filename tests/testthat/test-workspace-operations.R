test_that("workspace_copy creates independent copy", {
  z1 <- new_workspace()
  z1 <- store_dataset(z1, iris, "iris")
  z1 <- store_json(z1, '{"test": "data"}', "config.json", subdir = "config")

  z2 <- workspace_copy(z1)

  expect_false(identical(z1$dir, z2$dir))
  expect_identical(list_object_in_workspace(z1), list_object_in_workspace(z2))
  expect_s3_class(z2, "workspace")
})

test_that("workspace_copy preserves all data types", {
  z1 <- new_workspace()
  z1 <- store_dataset(z1, mtcars, "mtcars")
  z1 <- store_json(z1, '{"key": "value"}', "json.json", subdir = "json")
  z1 <- store_rds(z1, list(x = 1:5), "rds.rds", subdir = "rds")

  z2 <- workspace_copy(z1)

  expect_identical(read_dataset_in_workspace(z2, "mtcars"), tibble::as_tibble(mtcars))
  expect_identical(read_json_str_in_workspace(z2, "json"), '{"key": "value"}')
  expect_identical(read_rds_in_workspace(z2, "rds", subdir = "rds"), list(x = 1:5))
})

test_that("workspace_bind merges workspaces correctly", {
  z1 <- new_workspace()
  z1 <- store_dataset(z1, iris, "iris")

  z2 <- new_workspace()
  z2 <- store_dataset(z2, mtcars, "mtcars")

  z3 <- workspace_bind(z1, z2)
  objects <- list_object_in_workspace(z3)

  expect_true(all(c("iris", "mtcars") %in% objects$name))
  expect_equal(nrow(objects), 2)
})

test_that("workspace_bind handles different data types", {
  z1 <- new_workspace()
  z1 <- store_dataset(z1, iris, "iris")
  z1 <- store_json(z1, '{"source": "z1"}', "config.json", subdir = "config")

  z2 <- new_workspace()
  z2 <- store_dataset(z2, mtcars, "mtcars")
  z2 <- store_rds(z2, list(data = "test"), "test.rds", subdir = "rds")

  z3 <- workspace_bind(z1, z2)
  objects <- list_object_in_workspace(z3)

  expect_equal(nrow(objects), 4)
  expect_true(all(c("iris", "mtcars", "config", "test") %in% objects$name))
})

test_that("workspace_bind with replace=TRUE overwrites existing objects", {
  z1 <- new_workspace()
  z1 <- store_dataset(z1, iris, "shared_name")

  z2 <- new_workspace()
  z2 <- store_dataset(z2, mtcars, "shared_name")

  z3 <- workspace_bind(z1, z2, replace = TRUE)
  objects <- list_object_in_workspace(z3)

  expect_equal(nrow(objects), 1)
  expect_equal(objects$name, "shared_name")

  result <- read_dataset_in_workspace(z3, "shared_name")
  expect_identical(result, tibble::as_tibble(mtcars))
})

test_that("workspace_bind with replace=FALSE keeps original objects", {
  z1 <- new_workspace()
  z1 <- store_dataset(z1, iris, "shared_name")

  z2 <- new_workspace()
  z2 <- store_dataset(z2, mtcars, "shared_name")

  z3 <- workspace_bind(z1, z2, replace = FALSE)
  objects <- list_object_in_workspace(z3)

  expect_equal(nrow(objects), 1)
  expect_equal(objects$name, "shared_name")

  result <- read_dataset_in_workspace(z3, "shared_name")
  expect_identical(result, tibble::as_tibble(iris))
})

test_that("workspace_bind preserves timestamps", {
  z1 <- new_workspace()
  custom_timestamp <- "2024-01-01 10:00:00"
  z1 <- store_dataset(z1, iris, "iris", timestamp = custom_timestamp)

  z2 <- new_workspace()
  z2 <- store_dataset(z2, mtcars, "mtcars")

  z3 <- workspace_bind(z1, z2)

  iris_timestamp <- read_timestamp(z3, "iris", "dataset", subdir = "datasets")
  expect_equal(iris_timestamp, custom_timestamp)
})

test_that("workspace operations validate input types", {
  z <- new_workspace()
  not_workspace <- list(dir = "test")

  expect_error(workspace_bind(not_workspace, z))
  expect_error(workspace_bind(z, not_workspace))
})

