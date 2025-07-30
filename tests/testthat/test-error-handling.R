test_that("store_json handles invalid inputs gracefully", {
  z <- new_workspace()

  expect_error(store_json(z, 123, "test.json", subdir = "test"))
  expect_error(store_json(z, c("a", "b"), "test.json", subdir = "test"))
  expect_error(store_json(z, '{"valid": "json"}', 123, subdir = "test"))
  expect_error(store_json(z, '{"valid": "json"}', "test.json", subdir = 123))
})

test_that("store_rds handles invalid inputs gracefully", {
  z <- new_workspace()
  expect_error(store_rds(z, list(data = "test"), 123, subdir = "test"))
  expect_error(store_rds(z, list(data = "test"), "test.rds", subdir = 123))
  expect_error(store_rds(z, list(data = "test"), "test.rds", subdir = "test", timestamp = 123))
})

test_that("read functions handle nonexistent objects gracefully", {
  z <- new_workspace()

  expect_error(read_dataset_in_workspace(z, "nonexistent"))
  expect_error(read_json_str_in_workspace(z, "nonexistent", subdir = "test"))
  expect_error(read_rds_in_workspace(z, "nonexistent", subdir = "test"))
  expect_error(read_timestamp(z, "nonexistent", "dataset", subdir = "datasets"))
})

test_that("new_workspace handles existing directory", {
  existing_dir <- tempdir()
  expect_error(new_workspace(existing_dir))
})

test_that("delete_dataset handles nonexistent dataset", {
  z <- new_workspace()
  z <- store_dataset(z, iris, "iris")
  z_after <- delete_dataset(z, "nonexistent_dataset")
  objects <- list_object_in_workspace(z_after)
  expect_true("iris" %in% objects$name)
})

test_that("duplicate names replace old name", {
  z <- new_workspace()
  z <- store_json(z, '{"test1": "data"}', "test.json", name = "duplicate", subdir = "dir1")
  z <- store_json(z, '{"test2": "data"}', "test.json", name = "duplicate", subdir = "dir2")
  yy <- list_object_in_workspace(z)
  expect_equal(nrow(yy), 1L)
  expect_no_error(read_json_str_in_workspace(z, "duplicate"))
})

test_that("rm_object_in_workspace handles various error conditions", {
  z <- new_workspace()
  expect_error(rm_object_in_workspace(z, "nonexistent", "dataset"))
  expect_error(rm_object_in_workspace(z, "test", "nonexistent_type"))
  z <- store_dataset(z, iris, "iris")
  expect_error(rm_object_in_workspace(z, "iris", "dataset", subdir = "nonexistent"))
})

test_that("unpack_workspace handles invalid files", {
  invalid_file <- tempfile(fileext = ".zip")
  writeLines("not a valid zip", invalid_file)
  expect_error(unpack_workspace(invalid_file))
  nonexistent_file <- tempfile(fileext = ".zip")
  expect_warning(
    expect_error(
      unpack_workspace(nonexistent_file)
      ))
})

test_that("pack functions handle invalid paths", {
  z <- new_workspace()
  invalid_dir <- "/nonexistent/path"
  expect_error(pack_workspace(z, file.path(invalid_dir, "test.zip")))
})

test_that("functions validate string inputs correctly", {
  z <- new_workspace()
  expect_error(store_json(z, '{"valid": "json"}', NULL, subdir = "test"))
  expect_error(store_json(z, '{"valid": "json"}', "test.json", subdir = NULL))
  expect_error(store_rds(z, list(data = "test"), NULL, subdir = "test"))
})
