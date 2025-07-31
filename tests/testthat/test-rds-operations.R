test_that("RDS storage and retrieval works correctly", {
  z <- new_workspace()
  test_obj <- list(data = mtcars, meta = "test metadata", numbers = 1:10)

  z <- store_rds(z, test_obj, "test_obj.rds", subdir = "objects")
  retrieved <- read_rds_in_workspace(z, "test_obj", subdir = "objects")

  expect_identical(test_obj, retrieved)
})

test_that("RDS storage handles different R object types", {
  z <- new_workspace()

  # Test different object types
  test_list <- list(a = 1, b = "text", c = TRUE)
  test_vector <- c(1, 2, 3, 4, 5)
  test_matrix <- matrix(1:12, nrow = 3)
  test_df <- data.frame(x = 1:3, y = letters[1:3])

  z <- store_rds(z, test_list, "list.rds", subdir = "test")
  z <- store_rds(z, test_vector, "vector.rds", subdir = "test")
  z <- store_rds(z, test_matrix, "matrix.rds", subdir = "test")
  z <- store_rds(z, test_df, "dataframe.rds", subdir = "test")

  expect_identical(read_rds_in_workspace(z, "list", subdir = "test"), test_list)
  expect_identical(read_rds_in_workspace(z, "vector", subdir = "test"), test_vector)
  expect_identical(read_rds_in_workspace(z, "matrix", subdir = "test"), test_matrix)
  expect_identical(read_rds_in_workspace(z, "dataframe", subdir = "test"), test_df)
})

test_that("RDS storage automatically adds .rds extension", {
  z <- new_workspace()
  test_data <- list(value = 42)

  z <- store_rds(z, test_data, "no_extension", subdir = "test")

  objects <- list_object_in_workspace(z)
  expect_true(grepl("\\.rds$", objects$file[objects$name == "no_extension"]))
})

test_that("RDS timestamp is recorded correctly", {
  z <- new_workspace()
  test_obj <- list(data = "test")
  custom_timestamp <- "2024-01-15 10:30:00"

  z <- store_rds(z, test_obj, "timestamped.rds", subdir = "test", timestamp = custom_timestamp)

  retrieved_timestamp <- read_timestamp(z, "timestamped", "rds", subdir = "test")
  expect_equal(retrieved_timestamp, custom_timestamp)
})

test_that("RDS name parameter works correctly", {
  z <- new_workspace()
  test_obj <- list(data = "test")

  z <- store_rds(z, test_obj, "file.rds", name = "custom_name", subdir = "test")

  objects <- list_object_in_workspace(z)
  expect_true("custom_name" %in% objects$name)
  expect_false("file" %in% objects$name)
})
