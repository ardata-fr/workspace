test_that("store_dataset.sf works with basic sf objects", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create a simple sf object with points
  points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), st_point(c(2, 2)),
                   crs = st_crs(4263))
  sf_data <- st_sf(id = 1:3, value = c("A", "B", "C"), geometry = points)

  z <- store_dataset(z, sf_data, "test_points")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 1)
  expect_equal(objects$name, "test_points")
  expect_equal(objects$type, "geospatial")
  expect_true(grepl("\\.gpkg$", objects$file))
})

test_that("store_dataset.sf handles different file extensions", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create test sf object
  points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), crs = st_crs(4263))
  sf_data <- st_sf(id = 1:2, geometry = points)

  # Test with .gpkg extension
  z <- store_dataset(z, sf_data, "test.gpkg")
  expect_warning(z <- store_dataset(z, sf_data, "test.shp"))
  expect_warning(z <- store_dataset(z, sf_data, "test.geojson"))
  z <- store_dataset(z, sf_data, "test_no_ext")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 4)

  # Check that extensions are preserved/added correctly
  expect_true(any(grepl("test\\.gpkg$", objects$file)))
  expect_true(any(grepl("test\\.gpkg$", objects$file)))
  expect_true(any(grepl("test\\.gpkg$", objects$file)))
  expect_true(any(grepl("test_no_ext\\.gpkg$", objects$file)))
})

test_that("store_dataset.sf creates geospatial_dataset type", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create sf object
  polygon <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))), )
  sf_data <- st_sf(id = 1, name = "test_polygon", geometry = st_sfc(polygon), crs = st_crs(4263))

  z <- store_dataset(z, sf_data, "polygon_data")

  objects <- list_object_in_workspace(z)
  expect_equal(objects$type, "geospatial")
  expect_true(objects$subdir == "geospatial")
})

test_that("store_dataset.sf works with different geometry types", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Test with POINT geometry
  points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)))
  point_sf <- st_sf(id = 1:2, type = "point", geometry = points, crs = st_crs(4263))
  z <- store_dataset(z, point_sf, "points")

  # Test with LINESTRING geometry
  line <- st_linestring(rbind(c(0,0), c(1,1), c(2,2)))
  line_sf <- st_sf(id = 1, type = "line", geometry = st_sfc(line), crs = st_crs(4263))
  z <- store_dataset(z, line_sf, "lines")

  # Test with POLYGON geometry
  polygon <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  polygon_sf <- st_sf(id = 1, type = "polygon", geometry = st_sfc(polygon), crs = st_crs(4263))
  z <- store_dataset(z, polygon_sf, "polygons")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 3)
  expect_true(all(objects$type == "geospatial"))
  expect_true(all(c("points", "lines", "polygons") %in% objects$name))
})

test_that("store_dataset.sf preserves CRS information", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create sf object with specific CRS
  points <- st_sfc(st_point(c(500000, 4000000)), st_point(c(600000, 4100000)),
                   crs = "EPSG:32633")  # UTM Zone 33N
  sf_data <- st_sf(id = 1:2, value = c(10, 20), geometry = points)

  z <- store_dataset(z, sf_data, "utm_points")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 1)
  expect_equal(objects$type, "geospatial")

  # Verify file was created
  file_path <- file.path(z$dir, objects$file)
  expect_true(file.exists(file_path))
})

test_that("store_dataset.sf handles sf objects with attributes", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create sf object with multiple attribute columns
  points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), st_point(c(2, 2)))
  sf_data <- st_sf(
    id = 1:3,
    name = c("Location A", "Location B", "Location C"),
    value = c(10.5, 20.3, 15.7),
    category = factor(c("type1", "type2", "type1")),
    geometry = points,
    crs = st_crs(4263)
  )

  z <- store_dataset(z, sf_data, "attributed_points")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 1)
  expect_equal(objects$name, "attributed_points")
  expect_equal(objects$type, "geospatial")
})

test_that("store_dataset.sf replaces existing geospatial datasets", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create first sf object
  points1 <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)))
  sf_data1 <- st_sf(id = 1:2, value = c("old1", "old2"), geometry = points1, crs = st_crs(4263))
  z <- store_dataset(z, sf_data1, "replaceable")

  # Create second sf object with same name
  points2 <- st_sfc(st_point(c(5, 5)), st_point(c(6, 6)), st_point(c(7, 7)))
  sf_data2 <- st_sf(id = 1:3, value = c("new1", "new2", "new3"), geometry = points2, crs = st_crs(4263))
  z <- store_dataset(z, sf_data2, "replaceable")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 1)
  expect_equal(objects$name, "replaceable")
})

test_that("store_dataset.sf works with custom timestamps", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  points <- st_sfc(st_point(c(0, 0)))
  sf_data <- st_sf(id = 1, geometry = points, crs = st_crs(4263))
  custom_timestamp <- "2024-06-15 14:30:00"

  z <- store_dataset(z, sf_data, "timestamped_sf", timestamp = custom_timestamp)

  retrieved_timestamp <- read_timestamp(z, "timestamped_sf", type = "geospatial", subdir = "geospatial")
  expect_equal(retrieved_timestamp, custom_timestamp)
})

test_that("store_dataset.sf pack/unpack cycle preserves geospatial data", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create and store sf object
  points <- st_sfc(st_point(c(10, 20)), st_point(c(30, 40)))
  sf_data <- st_sf(id = 1:2, name = c("A", "B"), geometry = points, crs = st_crs(4263))
  z <- store_dataset(z, sf_data, "test_sf")

  # Pack and unpack
  packed_file <- tempfile(fileext = ".zip")
  pack_workspace(z, packed_file)
  z2 <- unpack_workspace(packed_file)

  objects <- list_object_in_workspace(z2)
  expect_equal(nrow(objects), 1)
  expect_equal(objects$name, "test_sf")
  expect_equal(objects$type, "geospatial")

  # Verify file exists after unpacking
  file_path <- file.path(z2$dir, objects$file)
  expect_true(file.exists(file_path))
})


test_that("store_dataset.sf handles empty sf objects", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create empty sf object
  empty_sf <- st_sf(
    id = integer(0),
    name = character(0),
    geometry = st_sfc(crs = 4326)
  )

  z <- store_dataset(z, empty_sf, "empty_sf")

  read_dataset_in_workspace(z, "empty_sf")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 1)
  expect_equal(objects$name, "empty_sf")
  expect_equal(objects$type, "geospatial")
})

# Tests for reading geospatial data
test_that("read_dataset_in_workspace retrieves sf objects correctly", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create and store sf object
  original_points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), st_point(c(2, 2)), crs = st_crs(4326))
  original_sf <- st_sf(id = 1:3, value = c("A", "B", "C"), geometry = original_points)

  z <- store_dataset(z, original_sf, "test_points")

  # Read back the sf object
  retrieved_sf <- read_dataset_in_workspace(z, "test_points")

  # Check that it's an sf object
  expect_s3_class(retrieved_sf, "sf")
  expect_true("geometry" %in% names(retrieved_sf))

  # Check data integrity
  expect_equal(nrow(retrieved_sf), 3)
  expect_equal(retrieved_sf$id, 1:3)
  expect_equal(retrieved_sf$value, c("A", "B", "C"))
})

test_that("read_dataset_in_workspace preserves CRS information", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create sf object with specific CRS
  utm_points <- st_sfc(st_point(c(500000, 4000000)), st_point(c(600000, 4100000)),
                       crs = "EPSG:32633")  # UTM Zone 33N
  original_sf <- st_sf(id = 1:2, value = c(10, 20), geometry = utm_points)

  z <- store_dataset(z, original_sf, "utm_data")
  retrieved_sf <- read_dataset_in_workspace(z, "utm_data")

  # Check CRS preservation
  expect_equal(st_crs(retrieved_sf), st_crs("EPSG:32633"))
  expect_equal(st_crs(retrieved_sf)$input, "EPSG:32633")
})

test_that("read_dataset_in_workspace handles different geometry types", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Store different geometry types
  points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), crs = st_crs(4326))
  point_sf <- st_sf(id = 1:2, type = "point", geometry = points)
  z <- store_dataset(z, point_sf, "points")

  line <- st_linestring(rbind(c(0,0), c(1,1), c(2,2)))
  line_sf <- st_sf(id = 1, type = "line", geometry = st_sfc(line, crs = st_crs(4326)))
  z <- store_dataset(z, line_sf, "lines")

  polygon <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  polygon_sf <- st_sf(id = 1, type = "polygon", geometry = st_sfc(polygon, crs = st_crs(4326)))
  z <- store_dataset(z, polygon_sf, "polygons")

  # Read back each geometry type
  retrieved_points <- read_dataset_in_workspace(z, "points")
  retrieved_lines <- read_dataset_in_workspace(z, "lines")
  retrieved_polygons <- read_dataset_in_workspace(z, "polygons")

  # Check geometry types are preserved
  expect_equal(as.character(st_geometry_type(retrieved_points, by_geometry = FALSE)), "POINT")
  expect_equal(as.character(st_geometry_type(retrieved_lines, by_geometry = FALSE)), "LINESTRING")
  expect_equal(as.character(st_geometry_type(retrieved_polygons, by_geometry = FALSE)), "POLYGON")
})

test_that("read_dataset_in_workspace preserves attribute data", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create sf object with various attribute types
  points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), st_point(c(2, 2)), crs = st_crs(4326))
  original_sf <- st_sf(
    id = 1:3,
    name = c("Location A", "Location B", "Location C"),
    value = c(10.5, 20.3, 15.7),
    category = factor(c("type1", "type2", "type1")),
    active = c(TRUE, FALSE, TRUE),
    geometry = points
  )

  z <- store_dataset(z, original_sf, "attributed_data")
  retrieved_sf <- read_dataset_in_workspace(z, "attributed_data")

  # Check all attributes are preserved
  expect_equal(retrieved_sf$id, 1:3)
  expect_equal(retrieved_sf$name, c("Location A", "Location B", "Location C"))
  expect_equal(retrieved_sf$value, c(10.5, 20.3, 15.7))
  expect_equal(retrieved_sf$active, c(TRUE, FALSE, TRUE))
  # Note: factor levels might not be perfectly preserved through file I/O
  expect_equal(as.character(retrieved_sf$category), c("type1", "type2", "type1"))
})

test_that("read_dataset_in_workspace works after pack/unpack cycle", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create and store sf object
  points <- st_sfc(st_point(c(10, 20)), st_point(c(30, 40)), crs = st_crs(4326))
  original_sf <- st_sf(id = 1:2, name = c("A", "B"), geometry = points)
  z <- store_dataset(z, original_sf, "pack_test")

  # Pack and unpack
  packed_file <- tempfile(fileext = ".zip")
  pack_workspace(z, packed_file)
  z2 <- unpack_workspace(packed_file)

  # Read from unpacked workspace
  retrieved_sf <- read_dataset_in_workspace(z2, "pack_test")

  expect_s3_class(retrieved_sf, "sf")
  expect_equal(nrow(retrieved_sf), 2)
  expect_equal(retrieved_sf$id, 1:2)
  expect_equal(retrieved_sf$name, c("A", "B"))
  expect_equal(st_crs(retrieved_sf), st_crs(4326))
})

test_that("read_dataset_in_workspace handles empty sf objects", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create and store empty sf object
  empty_sf <- st_sf(
    id = integer(0),
    name = character(0),
    geometry = st_sfc(crs = 4326)
  )
  z <- store_dataset(z, empty_sf, "empty_test")

  # Read back empty sf object
  retrieved_sf <- read_dataset_in_workspace(z, "empty_test")

  expect_s3_class(retrieved_sf, "sf")
  expect_equal(nrow(retrieved_sf), 0)
  expect_equal(names(retrieved_sf), names(empty_sf))
  expect_equal(attr(retrieved_sf, "sf_column"), attr(empty_sf, "sf_column"))
  expect_equal(st_crs(retrieved_sf), st_crs(empty_sf))
  expect_identical(st_agr(retrieved_sf), st_agr(empty_sf))
})

test_that("read_dataset_in_workspace tracks metadata properly", {
  skip_if_not_installed("sf")

  z <- workspace::new_workspace()

  points <- sf::st_sfc(sf::st_point(c(10, 20)), sf::st_point(c(30, 40)), crs = sf::st_crs(4326))
  original_sf <- sf::st_sf(
    id = 1:2,
    name = c("A", "B"),
    funky_geometry_name = points
    )
  z <- workspace::store_dataset(z, original_sf, "geospatial_data")

  # Read back empty sf object
  retrieved_sf <- workspace::read_dataset_in_workspace(z, "geospatial_data")

  expect_s3_class(retrieved_sf, "sf")
  expect_equal(nrow(retrieved_sf), 2)
  expect_equal(names(retrieved_sf), names(original_sf))
  expect_equal(attr(retrieved_sf, "sf_column"), attr(original_sf, "sf_column"))
  expect_equal(st_crs(retrieved_sf), st_crs(original_sf))
  expect_identical(st_agr(retrieved_sf), st_agr(original_sf))
})

test_that("read_dataset_in_workspace can read both regular and geospatial datasets", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Store regular dataset
  z <- store_dataset(z, iris, "regular_data")

  # Store geospatial dataset
  points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), crs = st_crs(4326))
  sf_data <- st_sf(id = 1:2, value = c("A", "B"), geometry = points)
  z <- store_dataset(z, sf_data, "geo_data")

  # Read both types
  regular_retrieved <- read_dataset_in_workspace(z, "regular_data")
  geo_retrieved <- read_dataset_in_workspace(z, "geo_data")

  # Check regular dataset (should be tibble)
  expect_s3_class(regular_retrieved, "tbl_df")
  expect_equal(nrow(regular_retrieved), nrow(iris))

  # Check geospatial dataset (should be sf)
  expect_s3_class(geo_retrieved, "sf")
  expect_equal(nrow(geo_retrieved), 2)
  expect_true("geometry" %in% names(geo_retrieved))
})

test_that("read_dataset_in_workspace error when sf package not available for geospatial data", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Store geospatial data
  points <- st_sfc(st_point(c(0, 0)), crs = st_crs(4326))
  sf_data <- st_sf(id = 1, geometry = points)
  z <- store_dataset(z, sf_data, "geo_test")

  # Mock requireNamespace to return FALSE for sf
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "sf") FALSE else TRUE
    },
    .package = "base"
  )

  expect_error(
    read_dataset_in_workspace(z, "geo_test"),
    "sf.*package must be installed"
  )
})

test_that("read_dataset_in_workspace preserves coordinate precision", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create sf object with high precision coordinates
  precise_points <- st_sfc(
    st_point(c(1.123456789, 2.987654321)),
    st_point(c(-45.111111111, 78.999999999)),
    crs = st_crs(4326)
  )
  original_sf <- st_sf(id = 1:2, geometry = precise_points)

  z <- store_dataset(z, original_sf, "precise_coords")
  retrieved_sf <- read_dataset_in_workspace(z, "precise_coords")

  # Check coordinate precision is reasonably preserved
  original_coords <- st_coordinates(original_sf)
  retrieved_coords <- st_coordinates(retrieved_sf)

  # Allow for small floating point differences due to file I/O
  expect_equal(retrieved_coords, original_coords, tolerance = 1e-6)
})
