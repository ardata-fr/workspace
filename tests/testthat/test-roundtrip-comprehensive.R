library(workspace)

test_that("data.frame round-trip preserves all column names and types", {
  z <- new_workspace()

  # Create data frame with challenging column names and various data types
  df_original <- data.frame(
    "ID" = 1:5,
    "Name With Spaces" = c("Alice", "Bob", "Charlie", "David", "Eve"),
    "special-chars_123" = c("a@b.com", "c#d", "e$f", "g%h", "i&j"),
    "unicode_café" = c("café", "naïve", "résumé", "München", "тест"),
    "Logical Flag" = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    "Factor Column" = factor(c("A", "B", "A", "C", "B"), levels = c("A", "B", "C")),
    "Numeric Value" = c(1.1, 2.2, 3.3, 4.4, 5.5),
    "Integer Count" = as.integer(c(10, 20, 30, 40, 50)),
    "Date Column" = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05")),
    "DateTime Column" = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-02 11:00:00",
                                    "2024-01-03 12:00:00", "2024-01-04 13:00:00", "2024-01-05 14:00:00")),
    "Missing Values" = c(1, NA, 3, NA, 5),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Store and retrieve
  z <- store_dataset(z, df_original, "comprehensive_df")
  df_retrieved <- read_dataset_in_workspace(z, "comprehensive_df")

  # Test exact column names preservation (this is the key issue)
  expect_identical(names(df_retrieved), names(df_original))
  expect_true("Name With Spaces" %in% names(df_retrieved))
  expect_true("unicode_café" %in% names(df_retrieved))
  expect_true("Logical Flag" %in% names(df_retrieved))

  # Test dimensions
  expect_equal(nrow(df_retrieved), nrow(df_original))
  expect_equal(ncol(df_retrieved), ncol(df_original))

  # Test data types and values
  expect_equal(df_retrieved$ID, df_original$ID)
  expect_equal(df_retrieved$`Name With Spaces`, df_original$`Name With Spaces`)
  expect_equal(df_retrieved$`special-chars_123`, df_original$`special-chars_123`)
  expect_equal(df_retrieved$`unicode_café`, df_original$`unicode_café`)
  expect_equal(df_retrieved$`Logical Flag`, df_original$`Logical Flag`)
  expect_equal(df_retrieved$`Numeric Value`, df_original$`Numeric Value`)
  expect_equal(df_retrieved$`Integer Count`, df_original$`Integer Count`)
  expect_equal(df_retrieved$`Date Column`, df_original$`Date Column`)
  expect_equal(as.character(df_retrieved$`DateTime Column`), as.character(df_original$`DateTime Column`))

  # Test NA preservation
  expect_equal(is.na(df_retrieved$`Missing Values`), is.na(df_original$`Missing Values`))
  expect_equal(df_retrieved$`Missing Values`[!is.na(df_retrieved$`Missing Values`)],
               df_original$`Missing Values`[!is.na(df_original$`Missing Values`)])

  # Test factor preservation (may be converted to character in parquet)
  expect_equal(as.character(df_retrieved$`Factor Column`), as.character(df_original$`Factor Column`))
})

test_that("data.frame round-trip handles empty and edge cases", {
  z <- new_workspace()

  # Empty data frame
  df_empty <- data.frame()
  z <- store_dataset(z, df_empty, "empty_df")
  df_empty_retrieved <- read_dataset_in_workspace(z, "empty_df")
  expect_equal(nrow(df_empty_retrieved), 0)
  expect_equal(ncol(df_empty_retrieved), 0)

  # Data frame with only one row
  df_single <- data.frame("Single Column" = "single value", check.names = FALSE)
  z <- store_dataset(z, df_single, "single_df")
  df_single_retrieved <- read_dataset_in_workspace(z, "single_df")
  expect_equal(names(df_single_retrieved), names(df_single))
  expect_equal(df_single_retrieved$`Single Column`, df_single$`Single Column`)

  # Data frame with only NAs
  df_nas <- data.frame("All NA" = c(NA_character_, NA_character_, NA_character_), check.names = FALSE)
  z <- store_dataset(z, df_nas, "na_df")
  df_nas_retrieved <- read_dataset_in_workspace(z, "na_df")
  expect_equal(names(df_nas_retrieved), names(df_nas))
  expect_true(all(is.na(df_nas_retrieved$`All NA`)))
})

test_that("data.frame round-trip preserves very long column names", {
  z <- new_workspace()

  long_name <- paste0("This_is_a_very_long_column_name_with_spaces_and_underscores_",
                     "that_might_cause_issues_in_some_file_formats_but_should_be_preserved_",
                     "exactly_as_it_is_without_any_modifications_whatsoever")

  df_long_names <- data.frame(
    "Short" = 1:3,
    check.names = FALSE
  )
  df_long_names[[long_name]] <- c("a", "b", "c")

  z <- store_dataset(z, df_long_names, "long_names_df")
  df_retrieved <- read_dataset_in_workspace(z, "long_names_df")

  expect_true(long_name %in% names(df_retrieved))
  expect_identical(names(df_retrieved), names(df_long_names))
  expect_equal(df_retrieved[[long_name]], df_long_names[[long_name]])
})

test_that("sf round-trip preserves column names with spaces and special characters", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create sf object with challenging column names
  points <- st_sfc(
    st_point(c(0, 0)),
    st_point(c(1, 1)),
    st_point(c(2, 2)),
    crs = st_crs(4326)
  )

  # Create data frame first, then convert to sf to control column names precisely
  df_data <- data.frame(
    "ID Number" = 1:3,
    "Location Name" = c("Site A", "Site B", "Site C"),
    "GPS Coordinate" = c("N40.7", "N40.8", "N40.9"),
    "special-chars_123" = c("test@example.com", "data#1", "value$2"),
    "unicode_café" = c("café", "naïve", "résumé"),
    "Measurement Value" = c(10.5, 20.3, 15.7),
    "Data Quality Flag" = c(TRUE, FALSE, TRUE),
    "Category Type" = factor(c("type1", "type2", "type1")),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  sf_original <- st_sf(df_data, geometry = points)

  # Store and retrieve
  z <- store_dataset(z, sf_original, "comprehensive_sf")
  sf_retrieved <- read_dataset_in_workspace(z, "comprehensive_sf")

  # Critical test: column names must be preserved exactly (this was the reported issue)
  expect_identical(names(sf_retrieved), names(sf_original))
  expect_true("Location Name" %in% names(sf_retrieved))
  expect_true("GPS Coordinate" %in% names(sf_retrieved))
  expect_true("unicode_café" %in% names(sf_retrieved))
  expect_true("Measurement Value" %in% names(sf_retrieved))
  expect_true("Data Quality Flag" %in% names(sf_retrieved))

  # Verify no spaces became dots (the specific issue mentioned)
  expect_false(any(grepl("Location\\.Name", names(sf_retrieved))))
  expect_false(any(grepl("GPS\\.Coordinate", names(sf_retrieved))))
  expect_false(any(grepl("Measurement\\.Value", names(sf_retrieved))))
  expect_false(any(grepl("Data\\.Quality\\.Flag", names(sf_retrieved))))

  # Test data integrity
  expect_equal(nrow(sf_retrieved), nrow(sf_original))
  expect_equal(sf_retrieved$`ID Number`, sf_original$`ID Number`)
  expect_equal(sf_retrieved$`Location Name`, sf_original$`Location Name`)
  expect_equal(sf_retrieved$`GPS Coordinate`, sf_original$`GPS Coordinate`)
  expect_equal(sf_retrieved$`unicode_café`, sf_original$`unicode_café`)
  expect_equal(sf_retrieved$`Measurement Value`, sf_original$`Measurement Value`)
  expect_equal(sf_retrieved$`Data Quality Flag`, sf_original$`Data Quality Flag`)

  # Test geometry preservation
  expect_s3_class(sf_retrieved, "sf")
  expect_equal(st_crs(sf_retrieved), st_crs(sf_original))
  expect_equal(as.character(st_geometry_type(sf_retrieved, by_geometry = FALSE)), "POINT")

  # Test coordinates precision
  coords_original <- st_coordinates(sf_original)
  coords_retrieved <- st_coordinates(sf_retrieved)
  expect_equal(coords_retrieved, coords_original, tolerance = 1e-10)
})

test_that("sf round-trip handles geometry column name preservation", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create sf object with custom geometry column name
  points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), crs = st_crs(4326))
  sf_custom_geom <- st_sf(
    id = 1:2,
    name = c("A", "B"),
    custom_geom_name = points
  )

  z <- store_dataset(z, sf_custom_geom, "custom_geom")
  sf_retrieved <- read_dataset_in_workspace(z, "custom_geom")

  # Verify geometry column name is preserved
  expect_equal(attr(sf_retrieved, "sf_column"), attr(sf_custom_geom, "sf_column"))
  expect_true(attr(sf_custom_geom, "sf_column") %in% names(sf_retrieved))
})

test_that("sf round-trip preserves ALL sf object attributes comprehensively", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create sf object with all possible attributes
  points <- st_sfc(
    st_point(c(0, 0)),
    st_point(c(1, 1)),
    st_point(c(2, 2)),
    crs = st_crs(4326)
  )

  # Create sf object with explicit attribute-geometry relationship (agr)
  sf_original <- st_sf(
    id = 1:3,
    "Station Name" = c("Site Alpha", "Site Beta", "Site Gamma"),
    "Temperature Reading" = c(20.5, 22.1, 19.8),
    "Quality Flag" = factor(c("good", "fair", "excellent")),
    "Measurement Date" = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    custom_geometry_col = points
  )

  # Set attribute-geometry relationships explicitly
  st_agr(sf_original) <- c(
    id = "identity",
    "Station Name" = "constant",
    "Temperature Reading" = "aggregate",
    "Quality Flag" = "constant",
    "Measurement Date" = "identity"
  )

  # Capture all original attributes
  original_attributes <- attributes(sf_original)

  z <- store_dataset(z, sf_original, "comprehensive_sf_attrs")
  sf_retrieved <- read_dataset_in_workspace(z, "comprehensive_sf_attrs")

  # Capture retrieved attributes
  retrieved_attributes <- attributes(sf_retrieved)

  # Test 1: names attribute (column names)
  expect_identical(retrieved_attributes$names, original_attributes$names)
  expect_true("Station Name" %in% retrieved_attributes$names)
  expect_true("Temperature Reading" %in% retrieved_attributes$names)
  expect_true("Quality Flag" %in% retrieved_attributes$names)
  expect_true("custom_geometry_col" %in% retrieved_attributes$names)

  # Test 2: row.names attribute
  expect_identical(retrieved_attributes$row.names, original_attributes$row.names)

  # Test 3: class attribute (may include tibble classes from arrow/parquet)
  expect_s3_class(sf_retrieved, "sf")
  expect_s3_class(sf_retrieved, "data.frame")
  # Note: parquet reading may add tibble classes, so we test core classes

  # Test 4: sf_column attribute (critical for geometry column identification)
  expect_identical(retrieved_attributes$sf_column, original_attributes$sf_column)
  expect_equal(attr(sf_retrieved, "sf_column"), "custom_geometry_col")

  # Test 5: agr attribute (attribute-geometry relationships)
  # AGR attributes are now preserved through YAML metadata
  agr_retrieved <- st_agr(sf_retrieved)
  agr_original <- st_agr(sf_original)
  expect_identical(agr_retrieved, agr_original)
  expect_s3_class(agr_retrieved, "factor")
  expect_equal(levels(agr_retrieved), c("constant", "aggregate", "identity"))

  # Verify specific agr values are preserved
  expect_equal(as.character(agr_retrieved[["id"]]), "identity")
  expect_equal(as.character(agr_retrieved[["Station Name"]]), "constant")
  expect_equal(as.character(agr_retrieved[["Temperature Reading"]]), "aggregate")
  expect_equal(as.character(agr_retrieved[["Quality Flag"]]), "constant")
  expect_equal(as.character(agr_retrieved[["Measurement Date"]]), "identity")

  # Test 6: Complete attributes comparison (all at once)
  # Compare all attribute names
  expect_setequal(names(retrieved_attributes), names(original_attributes))

  # Test 7: Data integrity with all attributes preserved
  expect_equal(nrow(sf_retrieved), nrow(sf_original))
  expect_equal(sf_retrieved$id, sf_original$id)
  expect_equal(sf_retrieved$`Station Name`, sf_original$`Station Name`)
  expect_equal(sf_retrieved$`Temperature Reading`, sf_original$`Temperature Reading`)
  expect_equal(as.character(sf_retrieved$`Quality Flag`), as.character(sf_original$`Quality Flag`))
  expect_equal(sf_retrieved$`Measurement Date`, sf_original$`Measurement Date`)

  # Test 8: Geometry integrity
  expect_equal(st_crs(sf_retrieved), st_crs(sf_original))
  coords_original <- st_coordinates(sf_original)
  coords_retrieved <- st_coordinates(sf_retrieved)
  expect_equal(coords_retrieved, coords_original, tolerance = 1e-10)
})

test_that("sf round-trip preserves agr attribute with all possible values", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create sf object with all three agr types
  points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), crs = st_crs(4326))
  sf_agr_test <- st_sf(
    constant_attr = c("same", "same"),        # constant across space
    aggregate_attr = c(10, 20),               # can be aggregated (summed, averaged)
    identity_attr = c("id1", "id2"),          # unique identifier
    geometry = points
  )

  # Set all three types of attribute-geometry relationships
  st_agr(sf_agr_test) <- c(
    constant_attr = "constant",
    aggregate_attr = "aggregate",
    identity_attr = "identity"
  )

  z <- store_dataset(z, sf_agr_test, "agr_test")
  sf_retrieved <- read_dataset_in_workspace(z, "agr_test")

  # Test agr preservation
  agr_original <- st_agr(sf_agr_test)
  agr_retrieved <- st_agr(sf_retrieved)

  # AGR attributes are now preserved through YAML metadata
  expect_identical(agr_retrieved, agr_original)
  expect_equal(as.character(agr_retrieved[["constant_attr"]]), "constant")
  expect_equal(as.character(agr_retrieved[["aggregate_attr"]]), "aggregate")
  expect_equal(as.character(agr_retrieved[["identity_attr"]]), "identity")

  # Verify the agr attribute structure
  expect_s3_class(agr_retrieved, "factor")
  expect_equal(levels(agr_retrieved), c("constant", "aggregate", "identity"))
})

test_that("sf round-trip handles NA agr values correctly", {
  skip_if_not_installed("sf")

  library(sf)
  z <- new_workspace()

  # Create sf object without explicitly setting agr (defaults to NA)
  points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), crs = st_crs(4326))
  sf_na_agr <- st_sf(
    attr1 = c("a", "b"),
    attr2 = c(1, 2),
    geometry = points
  )

  # By default, agr should be NA
  original_agr <- st_agr(sf_na_agr)
  expect_true(all(is.na(original_agr)))

  z <- store_dataset(z, sf_na_agr, "na_agr_test")
  sf_retrieved <- read_dataset_in_workspace(z, "na_agr_test")

  # Test that NA agr values are preserved
  retrieved_agr <- st_agr(sf_retrieved)
  expect_identical(retrieved_agr, original_agr)
  expect_true(all(is.na(retrieved_agr)))
  expect_equal(names(retrieved_agr), c("attr1", "attr2"))
})

test_that("raster round-trip preserves all metadata and values exactly", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  # Create raster with challenging properties
  r_original <- rast(
    ncols = 10, nrows = 8,
    xmin = -180, xmax = 180,
    ymin = -90, ymax = 90,
    crs = "+proj=longlat +datum=WGS84 +no_defs"
  )

  # Set values with precision requirements and edge cases
  values(r_original) <- c(
    -999.123456789, # Negative with high precision
    0.000000001,    # Very small positive
    1.23e-10,       # Scientific notation
    9999999.999,    # Large with decimals
    NA,             # Missing values
    Inf,            # Infinity
    -Inf,           # Negative infinity
    rep(c(1.1, 2.2, 3.3), length.out = 80 - 7)
  )

  names(r_original) <- "Temperature_Data_with_Spaces_and_Special-Chars_123"

  # Store and retrieve
  z <- store_raster(z, r_original, "precision_raster")
  r_retrieved <- read_raster_in_workspace(z, "precision_raster")

  # Test exact metadata preservation
  expect_equal(ncol(r_retrieved), ncol(r_original))
  expect_equal(nrow(r_retrieved), nrow(r_original))
  expect_equal(as.vector(ext(r_retrieved)), as.vector(ext(r_original)))
  expect_equal(crs(r_retrieved), crs(r_original))
  expect_equal(names(r_retrieved), names(r_original))

  # Test value preservation (allowing for floating point tolerance due to TIFF format limitations)
  values_original <- as.vector(values(r_original))
  values_retrieved <- as.vector(values(r_retrieved))

  # Test finite values with realistic tolerance for TIFF format
  finite_mask <- is.finite(values_original)
  expect_equal(values_retrieved[finite_mask], values_original[finite_mask], tolerance = 1e-3)

  # Test NA preservation
  expect_equal(is.na(values_retrieved), is.na(values_original))

  # Test infinite values preservation
  expect_equal(is.infinite(values_retrieved), is.infinite(values_original))
  expect_equal(values_retrieved[is.infinite(values_retrieved)],
               values_original[is.infinite(values_original)])
})

test_that("multilayer raster round-trip preserves layer names with spaces", {
  skip_if_not_installed("terra")

  library(terra)
  z <- new_workspace()

  # Create multilayer raster with challenging layer names
  layer1 <- rast(ncols = 5, nrows = 5, vals = 1:25)
  layer2 <- rast(ncols = 5, nrows = 5, vals = 26:50)
  layer3 <- rast(ncols = 5, nrows = 5, vals = 51:75)

  multilayer <- c(layer1, layer2, layer3)
  names(multilayer) <- c(
    "Band 1 Red Channel",
    "Band 2 Green Channel",
    "Band 3 Blue-NIR_Channel"
  )

  z <- store_raster(z, multilayer, "multilayer_names")
  r_retrieved <- read_raster_in_workspace(z, "multilayer_names")

  # Test layer names preservation
  expect_equal(names(r_retrieved), names(multilayer))
  expect_equal(nlyr(r_retrieved), nlyr(multilayer))

  # Test individual layer values
  for (i in 1:nlyr(multilayer)) {
    expect_equal(as.vector(values(r_retrieved[[i]])),
                as.vector(values(multilayer[[i]])))
  }
})

test_that("JSON round-trip preserves exact string content including unicode", {
  z <- new_workspace()

  # JSON string content (noting that JSON storage preserves content semantically, not formatting)
  json_content <- list(
    basic_string = "Hello World",
    unicode_text = "café, naïve, résumé, 中文, العربية, 🎉",
    special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?",
    multiline = "Line 1\nLine 2\nLine 3",
    nested = list(
      array = list(1, 2, 3, "text with spaces"),
      boolean = TRUE,
      null_value = NULL,
      number = 123.456
    ),
    edge_cases = list(
      empty_string = "",
      whitespace = "   ",
      tabs_and_newlines = "\t\n\r"
    )
  )

  json_original <- jsonlite::toJSON(json_content, pretty = FALSE, auto_unbox = TRUE)

  z <- store_json(z, json_original, "complex.json", subdir = "test")
  json_retrieved <- read_json_str_in_workspace(z, "complex", subdir = "test")

  # Parse both JSON strings and compare the data structures
  parsed_original <- jsonlite::fromJSON(json_original)
  parsed_retrieved <- jsonlite::fromJSON(json_retrieved)

  # Test content preservation (semantic, not formatting)
  expect_equal(parsed_retrieved$basic_string, parsed_original$basic_string)
  expect_equal(parsed_retrieved$unicode_text, parsed_original$unicode_text)
  expect_equal(parsed_retrieved$special_chars, parsed_original$special_chars)
  expect_equal(parsed_retrieved$multiline, parsed_original$multiline)

  # Verify specific content is preserved
  expect_true(grepl("café, naïve, résumé, 中文, العربية, 🎉", json_retrieved, fixed = TRUE))
  expect_true(grepl("!@#\\$%\\^&\\*\\(\\)_\\+-=\\[\\]\\{\\}\\|;':", json_retrieved))
  expect_true(grepl("Line 1\\\\nLine 2\\\\nLine 3", json_retrieved))
})

test_that("RDS round-trip preserves complex nested structures exactly", {
  z <- new_workspace()

  # Create complex nested R object
  complex_object <- list(
    "basic_types" = list(
      "character" = "text with spaces and unicode: café",
      "numeric" = pi,
      "integer" = 42L,
      "logical" = TRUE,
      "complex" = 1 + 2i
    ),
    "data_structures" = list(
      "vector" = c(1.1, 2.2, 3.3),
      "matrix" = matrix(1:12, nrow = 3, dimnames = list(c("row 1", "row 2", "row 3"),
                                                       c("col A", "col B", "col C", "col D"))),
      "data_frame" = data.frame(
        "Name With Spaces" = c("Alice", "Bob", "Charlie"),
        "Value" = c(1.1, 2.2, 3.3),
        "Flag" = c(TRUE, FALSE, TRUE),
        check.names = FALSE,
        stringsAsFactors = FALSE
      ),
      "factor" = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
      "list" = list("nested" = list("deeply" = "value"))
    ),
    "special_values" = list(
      "na_values" = c(1, NA, 3, NA, 5),
      "inf_values" = c(-Inf, 0, Inf),
      "empty_vectors" = list(
        "character" = character(0),
        "numeric" = numeric(0),
        "logical" = logical(0)
      )
    ),
    "attributes_test" = structure(
      c(1, 2, 3),
      names = c("first item", "second item", "third item"),
      custom_attr = "This should be preserved",
      class = "custom_class"
    )
  )

  z <- store_rds(z, complex_object, "complex.rds", subdir = "test")
  retrieved_object <- read_rds_in_workspace(z, "complex", subdir = "test")

  # Test exact preservation using identical()
  expect_identical(retrieved_object, complex_object)

  # Test specific nested elements
  expect_equal(retrieved_object$data_structures$data_frame$`Name With Spaces`,
               complex_object$data_structures$data_frame$`Name With Spaces`)
  expect_identical(names(retrieved_object$data_structures$matrix),
                  names(complex_object$data_structures$matrix))
  expect_identical(dimnames(retrieved_object$data_structures$matrix),
                  dimnames(complex_object$data_structures$matrix))

  # Test attributes preservation
  expect_identical(attributes(retrieved_object$attributes_test),
                  attributes(complex_object$attributes_test))
})

test_that("YAML round-trip preserves complex nested structures with unicode", {
  z <- new_workspace()

  # Complex YAML structure
  yaml_original <- list(
    "text_with_spaces" = "This is text with spaces",
    "unicode_content" = list(
      "french" = "café, naïve, résumé",
      "chinese" = "中文测试",
      "arabic" = "العربية",
      "emoji" = "🎉🚀✨"
    ),
    "special_characters" = list(
      "symbols" = "!@#$%^&*()_+-=[]{}|;':\",./<>?",
      "multiline" = "Line 1\nLine 2\nLine 3",
      "quotes" = "Text with 'single' and \"double\" quotes"
    ),
    "data_types" = list(
      "string" = "text",
      "number" = 123.456,
      "integer" = 42L,
      "boolean" = TRUE,
      "null_value" = NULL,
      "vector" = c(1, 2, 3, 4, 5),
      "character_vector" = c("a", "b", "c")
    ),
    "nested_structure" = list(
      "level1" = list(
        "level2" = list(
          "level3" = list(
            "deep_value" = "This is deeply nested",
            "deep_list" = c("item1", "item2", "item3")
          )
        )
      )
    ),
    "edge_cases" = list(
      "empty_string" = "",
      "whitespace_only" = "   ",
      "tab_and_newline" = "\t\n",
      "very_long_text" = paste(rep("This is a very long text that should be preserved exactly. ", 10), collapse = "")
    )
  )

  z <- store_yaml(z, yaml_original, "complex.yaml", subdir = "test")
  yaml_retrieved <- read_yaml_in_workspace(z, "complex", subdir = "test")

  # Test exact structure preservation
  expect_identical(yaml_retrieved, yaml_original)

  # Test specific unicode preservation
  expect_identical(yaml_retrieved$unicode_content$french, yaml_original$unicode_content$french)
  expect_identical(yaml_retrieved$unicode_content$chinese, yaml_original$unicode_content$chinese)
  expect_identical(yaml_retrieved$unicode_content$arabic, yaml_original$unicode_content$arabic)
  expect_identical(yaml_retrieved$unicode_content$emoji, yaml_original$unicode_content$emoji)

  # Test nested structure preservation
  expect_identical(yaml_retrieved$nested_structure$level1$level2$level3$deep_value,
                  yaml_original$nested_structure$level1$level2$level3$deep_value)

  # Test data type preservation
  expect_identical(yaml_retrieved$data_types$vector, yaml_original$data_types$vector)
  expect_identical(yaml_retrieved$data_types$character_vector, yaml_original$data_types$character_vector)

  # Test edge cases
  expect_identical(yaml_retrieved$edge_cases$empty_string, yaml_original$edge_cases$empty_string)
  expect_identical(yaml_retrieved$edge_cases$whitespace_only, yaml_original$edge_cases$whitespace_only)
  expect_identical(yaml_retrieved$edge_cases$very_long_text, yaml_original$edge_cases$very_long_text)
})

test_that("all file types round-trip in pack/unpack cycle", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  library(sf)
  library(terra)

  z <- new_workspace()

  # Store one of each file type with challenging names/content
  df_test <- data.frame(
    "Column With Spaces" = c("A", "B", "C"),
    "unicode_café" = c("café", "naïve", "résumé"),
    check.names = FALSE
  )
  z <- store_dataset(z, df_test, "df_with_spaces")

  points <- st_sfc(st_point(c(0, 0)), st_point(c(1, 1)), crs = st_crs(4326))
  sf_test <- st_sf(
    "Location Name" = c("Site A", "Site B"),
    "GPS Data" = c("N40.7", "N40.8"),
    geometry = points
  )
  z <- store_dataset(z, sf_test, "sf_with_spaces")

  r_test <- rast(ncols = 3, nrows = 3, vals = 1:9)
  names(r_test) <- "Layer With Spaces"
  z <- store_raster(z, r_test, "raster_with_spaces")

  json_test <- '{"key with spaces": "value with unicode café"}'
  z <- store_json(z, json_test, "json_test.json", subdir = "test")

  rds_test <- list("Item With Spaces" = "Value with unicode café")
  z <- store_rds(z, rds_test, "rds_test.rds", subdir = "test")

  yaml_test <- list("Key With Spaces" = "Value with unicode café")
  z <- store_yaml(z, yaml_test, "yaml_test.yaml", subdir = "test")

  # Pack and unpack
  packed_file <- tempfile(fileext = ".zip")
  pack_workspace(z, packed_file)
  z2 <- unpack_workspace(packed_file)

  # Verify all content preserved after pack/unpack cycle
  df_retrieved <- read_dataset_in_workspace(z2, "df_with_spaces")
  expect_identical(names(df_retrieved), names(df_test))
  expect_true("Column With Spaces" %in% names(df_retrieved))

  sf_retrieved <- read_dataset_in_workspace(z2, "sf_with_spaces")
  expect_identical(names(sf_retrieved), names(sf_test))
  expect_true("Location Name" %in% names(sf_retrieved))

  r_retrieved <- read_raster_in_workspace(z2, "raster_with_spaces")
  expect_equal(names(r_retrieved), names(r_test))

  json_retrieved <- read_json_str_in_workspace(z2, "json_test", subdir = "test")
  expect_identical(json_retrieved, json_test)

  rds_retrieved <- read_rds_in_workspace(z2, "rds_test", subdir = "test")
  expect_identical(rds_retrieved, rds_test)

  yaml_retrieved <- read_yaml_in_workspace(z2, "yaml_test", subdir = "test")
  expect_identical(yaml_retrieved, yaml_test)
})
