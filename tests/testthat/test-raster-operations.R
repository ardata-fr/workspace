test_that("store_raster works with basic SpatRaster objects", {
  skip_if_not_installed("terra")

  library(terra)
  library(workspace)
  z <- new_workspace()

  # Create a simple raster object
  r <- rast(ncols=10, nrows=10, xmin=0, xmax=10, ymin=0, ymax=10)
  values(r) <- 1:100

  z <- store_raster(z, r, "test_raster")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 1)
  expect_equal(objects$name, "test_raster")
  expect_equal(objects$type, "raster")
  expect_true(grepl("\\.tiff$", objects$file))
})

test_that("store_raster handles different file extensions", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  # Create test raster object
  r <- rast(ncols=5, nrows=5, vals=1:25)

  # Test with .tiff extension
  z <- store_raster(z, r, "test.tiff")
  z <- store_raster(z, r, "test.tif")
  z <- store_raster(z, r, "test_no_ext")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 3)

  # Check that extensions are preserved/added correctly
  expect_true(any(grepl("test\\.tiff$", objects$file)))
  expect_true(any(grepl("test\\.tif$", objects$file)))
  expect_true(any(grepl("test_no_ext\\.tiff$", objects$file)))
})

test_that("store_raster creates raster dataset type", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  # Create multi-layer raster
  r1 <- rast(ncols=10, nrows=10, vals=1:100)
  r2 <- rast(ncols=10, nrows=10, vals=101:200)
  r_multi <- c(r1, r2)
  names(r_multi) <- c("layer1", "layer2")

  z <- store_raster(z, r_multi, "multilayer_raster")

  objects <- list_object_in_workspace(z)
  expect_equal(objects$type, "raster")
  expect_equal(objects$name, "multilayer_raster")
})

test_that("store_raster works with different raster value types", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  # Integer raster
  r_int <- rast(ncols=5, nrows=5, vals=1:25)
  z <- store_raster(z, r_int, "integer_raster")

  # Float raster
  r_float <- rast(ncols=5, nrows=5, vals=runif(25))
  z <- store_raster(z, r_float, "float_raster")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 2)
  expect_true(all(objects$type == "raster"))
})

test_that("store_raster replaces existing raster datasets", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  # Create two different rasters
  r1 <- rast(ncols=5, nrows=5, vals=1:25)
  r2 <- rast(ncols=10, nrows=10, vals=1:100)

  z <- store_raster(z, r1, "replaceable")
  z <- store_raster(z, r2, "replaceable")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 1)
  expect_equal(objects$name, "replaceable")

  # Verify the second raster was stored
  retrieved <- read_raster_in_workspace(z, "replaceable")
  expect_equal(ncell(retrieved), 100)
})

test_that("store_raster works with custom timestamps", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  r <- rast(ncols=5, nrows=5, vals=1:25)
  custom_timestamp <- "2023-12-25 10:30:45"

  z <- store_raster(z, r, "timestamped_raster", timestamp = custom_timestamp)

  objects <- list_object_in_workspace(z)
  expect_equal(objects$timestamp, custom_timestamp)
})

test_that("read_raster_in_workspace retrieves stored rasters correctly", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  # Create and store raster
  original_raster <- rast(ncols=10, nrows=10, xmin=0, xmax=10, ymin=0, ymax=10)
  values(original_raster) <- 1:100

  z <- store_raster(z, original_raster, "test_raster")
  retrieved_raster <- read_raster_in_workspace(z, "test_raster")

  # Check dimensions and values
  expect_equal(ncol(retrieved_raster), ncol(original_raster))
  expect_equal(nrow(retrieved_raster), nrow(original_raster))
  expect_equal(as.vector(values(retrieved_raster)), as.vector(values(original_raster)))
})

test_that("raster round-trip preserves all properties", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  # Create raster with specific properties
  original_raster <- rast(ncols=8, nrows=6, xmin=-180, xmax=180, ymin=-90, ymax=90,
                         crs="+proj=longlat +datum=WGS84")
  values(original_raster) <- runif(48, min=-100, max=100)
  names(original_raster) <- "temperature"

  # Store and retrieve
  z <- store_raster(z, original_raster, "test_round_trip")
  retrieved_raster <- read_raster_in_workspace(z, "test_round_trip")

  # Check all properties are preserved
  expect_equal(ncol(retrieved_raster), ncol(original_raster))
  expect_equal(nrow(retrieved_raster), nrow(original_raster))
  expect_equal(as.vector(ext(retrieved_raster)), as.vector(ext(original_raster)))
  expect_equal(crs(retrieved_raster), crs(original_raster))
  expect_equal(names(retrieved_raster), names(original_raster))

  # Check values are preserved (allowing for small floating point differences)
  expect_equal(as.vector(values(retrieved_raster)), as.vector(values(original_raster)), tolerance = 1e-5)
})

test_that("raster metadata preservation via YAML", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  # Create raster with complex CRS that might not be preserved in TIFF
  original_raster <- rast(ncols=5, nrows=5, xmin=500000, xmax=600000, ymin=4000000, ymax=4100000,
                         crs="+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
  values(original_raster) <- 1:25
  names(original_raster) <- "utm_data"

  # Store and retrieve
  z <- store_raster(z, original_raster, "utm_test")
  retrieved_raster <- read_raster_in_workspace(z, "utm_test")

  # Check that CRS is preserved exactly via metadata
  expect_equal(crs(retrieved_raster), crs(original_raster))
  expect_equal(names(retrieved_raster), names(original_raster))

  # Verify YAML metadata file was created
  yaml_path <- file.path(z$dir, "assets", "raster_metadata", "utm_test.yaml")
  expect_true(file.exists(yaml_path))

  # Check metadata content
  metadata <- yaml::read_yaml(yaml_path)
  expect_equal(metadata$crs, crs(original_raster))
  expect_equal(metadata$names, names(original_raster))
  expect_equal(metadata$nlyr, nlyr(original_raster))
})

test_that("multilayer raster round-trip preserves layer structure", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  # Create multi-layer raster
  layer1 <- rast(ncols=5, nrows=5, vals=1:25)
  layer2 <- rast(ncols=5, nrows=5, vals=26:50)
  layer3 <- rast(ncols=5, nrows=5, vals=51:75)
  multilayer_raster <- c(layer1, layer2, layer3)
  names(multilayer_raster) <- c("band1", "band2", "band3")

  # Store and retrieve
  z <- store_raster(z, multilayer_raster, "multilayer_test")
  retrieved_raster <- read_raster_in_workspace(z, "multilayer_test")

  # Check layer structure
  expect_equal(nlyr(retrieved_raster), nlyr(multilayer_raster))
  expect_equal(names(retrieved_raster), names(multilayer_raster))

  # Check each layer's values
  for (i in 1:nlyr(multilayer_raster)) {
    expect_equal(as.vector(values(retrieved_raster[[i]])),
                as.vector(values(multilayer_raster[[i]])))
  }
})

test_that("store_raster and read_raster_in_workspace error handling", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  # Test reading non-existent raster
  expect_error(read_raster_in_workspace(z, "nonexistent"),
               "Value of.*name.*cannot be found in workspace")

  # Test with empty workspace
  empty_z <- new_workspace()
  expect_error(read_raster_in_workspace(empty_z, "test"),
               "Value of.*name.*cannot be found in workspace")
})

test_that("store_raster works with store_dataset.SpatRaster compatibility", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  # Create raster
  r <- rast(ncols=5, nrows=5, vals=1:25)

  # Both methods should work equivalently
  z1 <- store_raster(z, r, "direct_raster")
  z2 <- store_dataset(z1, r, "s3_raster")

  objects <- list_object_in_workspace(z2)
  expect_equal(nrow(objects), 2)
  expect_true(all(objects$type == "raster"))

  # Both should be readable
  direct_result <- read_raster_in_workspace(z2, "direct_raster")
  s3_result <- read_raster_in_workspace(z2, "s3_raster")

  expect_equal(as.vector(values(direct_result)), as.vector(values(s3_result)))
})
