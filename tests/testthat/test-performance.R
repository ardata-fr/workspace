test_that("handles moderately large datasets efficiently", {
  z <- new_workspace()

  # Create a moderately sized dataset (1000 rows, 20 columns)
  large_df <- data.frame(
    matrix(runif(20000), nrow = 1000, ncol = 20)
  )
  names(large_df) <- paste0("var_", 1:20)

  # Test storage
  start_time <- Sys.time()
  z <- store_dataset(z, large_df, "large_dataset")
  storage_time <- difftime(Sys.time(), start_time, units = "secs")

  # Should complete in reasonable time (less than 5 seconds)
  expect_lt(as.numeric(storage_time), 5)

  # Test retrieval
  start_time <- Sys.time()
  retrieved <- read_dataset_in_workspace(z, "large_dataset")
  retrieval_time <- difftime(Sys.time(), start_time, units = "secs")

  expect_lt(as.numeric(retrieval_time), 5)
  expect_equal(nrow(retrieved), 1000)
  expect_equal(ncol(retrieved), 20)
})

test_that("handles multiple datasets efficiently", {
  z <- new_workspace()

  # Store multiple datasets
  datasets <- list(
    iris = iris,
    mtcars = mtcars,
    cars = cars,
    airquality = airquality,
    chickwts = ChickWeight
  )

  start_time <- Sys.time()
  for (name in names(datasets)) {
    z <- store_dataset(z, datasets[[name]], name)
  }
  storage_time <- difftime(Sys.time(), start_time, units = "secs")

  expect_lt(as.numeric(storage_time), 10)

  # Test pack/unpack performance
  packed_file <- tempfile(fileext = ".zip")

  start_time <- Sys.time()
  pack_workspace(z, packed_file)
  pack_time <- difftime(Sys.time(), start_time, units = "secs")

  start_time <- Sys.time()
  z2 <- unpack_workspace(packed_file)
  unpack_time <- difftime(Sys.time(), start_time, units = "secs")

  expect_lt(as.numeric(pack_time), 5)
  expect_lt(as.numeric(unpack_time), 5)

  objects <- list_object_in_workspace(z2)
  expect_equal(nrow(objects), 5)
})

test_that("workspace copy performance is reasonable", {
  z <- new_workspace()

  # Create workspace with multiple objects
  z <- store_dataset(z, iris, "iris")
  z <- store_dataset(z, mtcars, "mtcars")
  z <- store_json(z, '{"large": "json with some content that is moderately sized"}', "config.json", subdir = "config")
  z <- store_rds(z, list(data = 1:1000), "data.rds", subdir = "rdata")

  start_time <- Sys.time()
  z2 <- workspace_copy(z)
  copy_time <- difftime(Sys.time(), start_time, units = "secs")

  expect_lt(as.numeric(copy_time), 10)

  # Verify copy is complete
  objects1 <- list_object_in_workspace(z)
  objects2 <- list_object_in_workspace(z2)
  expect_identical(objects1, objects2)
})

test_that("workspace bind performance with multiple workspaces", {
  # Create multiple workspaces
  z1 <- new_workspace()
  z1 <- store_dataset(z1, iris, "iris")
  z1 <- store_json(z1, '{"workspace": 1}', "meta.json", subdir = "meta")

  z2 <- new_workspace()
  z2 <- store_dataset(z2, mtcars, "mtcars")
  z2 <- store_rds(z2, list(workspace = 2), "meta.rds", subdir = "meta")

  z3 <- new_workspace()
  z3 <- store_dataset(z3, cars, "cars")
  z3 <- store_json(z3, '{"workspace": 3}', "config.json", subdir = "config")

  start_time <- Sys.time()
  temp_workspace <- workspace_bind(z1, z2)
  final_workspace <- workspace_bind(temp_workspace, z3)
  bind_time <- difftime(Sys.time(), start_time, units = "secs")

  expect_lt(as.numeric(bind_time), 15)

  objects <- list_object_in_workspace(final_workspace)
  expect_equal(nrow(objects), 5)
})

test_that("file system operations are efficient", {
  z <- new_workspace()

  # Test multiple file operations
  start_time <- Sys.time()
  for (i in 1:10) {
    z <- store_json(z, paste0('{"iteration": ', i, '}'), paste0("file_", i, ".json"), subdir = "batch")
  }
  multi_file_time <- difftime(Sys.time(), start_time, units = "secs")

  expect_lt(as.numeric(multi_file_time), 5)

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 10)

  # Test deletion performance
  start_time <- Sys.time()
  for (i in 1:5) {
    z <- rm_object_in_workspace(z, paste0("file_", i), "json", subdir = "batch")
  }
  deletion_time <- difftime(Sys.time(), start_time, units = "secs")

  expect_lt(as.numeric(deletion_time), 5)

  remaining_objects <- list_object_in_workspace(z)
  expect_equal(nrow(remaining_objects), 5)
})

# test_that("memory usage is reasonable for typical workloads", {
#   z <- new_workspace()
#
#   # Monitor memory before operations
#   gc()  # Force garbage collection
#
#   # Store various data types
#   z <- store_dataset(z, iris, "iris")
#   z <- store_dataset(z, mtcars, "mtcars")
#
#   # Create some JSON data
#   json_data <- jsonlite::toJSON(list(
#     numbers = 1:100,
#     text = rep("sample text", 50),
#     nested = list(a = 1:20, b = letters)
#   ), auto_unbox = TRUE)
#   z <- store_json(z, json_data, "complex.json", subdir = "data")
#
#   # Store RDS object
#   complex_obj <- list(
#     matrices = lapply(1:5, function(i) matrix(runif(100), 10, 10)),
#     vectors = lapply(1:10, function(i) runif(50)),
#     metadata = list(created = Sys.time(), version = "1.0")
#   )
#   z <- store_rds(z, complex_obj, "complex.rds", subdir = "objects")
#
#   # Pack and unpack
#   packed_file <- tempfile(fileext = ".zip")
#   pack_workspace(z, packed_file)
#   z2 <- unpack_workspace(packed_file)
#
#   # Verify all operations completed without error
#   objects <- list_object_in_workspace(z2)
#   expect_equal(nrow(objects), 3)
#
#   # Force garbage collection to clean up
#   gc()
# })
