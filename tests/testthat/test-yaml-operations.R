test_that("store_yaml works with basic list objects", {


  z <- new_workspace()

  # Create a simple list
  test_list <- list(
    name = "John Doe",
    age = 30,
    active = TRUE,
    scores = c(85, 90, 78)
  )

  z <- store_yaml(z, test_list, "simple.yaml", subdir = "configs")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 1)
  expect_equal(objects$name, "simple")
  expect_equal(objects$type, "yaml")
  expect_true(grepl("\\.yaml$", objects$file))
})

test_that("store_yaml handles nested list structures", {


  z <- new_workspace()

  # Create nested list structure
  complex_list <- list(
    database = list(
      host = "localhost",
      port = 5432,
      credentials = list(
        username = "admin",
        password = "secret123"
      )
    ),
    settings = list(
      debug = TRUE,
      max_connections = 100,
      features = list(
        caching = TRUE,
        logging = FALSE
      )
    )
  )

  z <- store_yaml(z, complex_list, "config.yaml", subdir = "configs")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 1)
  expect_equal(objects$name, "config")
  expect_equal(objects$type, "yaml")
})

test_that("store_yaml automatically adds .yaml extension", {


  z <- new_workspace()
  test_list <- list(key = "value")

  z <- store_yaml(z, test_list, "no_extension", subdir = "test")

  objects <- list_object_in_workspace(z)
  expect_true(grepl("\\.yaml$", objects$file))
})

test_that("store_yaml preserves .yaml extension", {


  z <- new_workspace()
  test_list <- list(key = "value")

  z <- store_yaml(z, test_list, "with_extension.yaml", subdir = "test")

  objects <- list_object_in_workspace(z)
  expect_true(grepl("with_extension\\.yaml$", objects$file))
  expect_false(grepl("with_extension\\.yaml\\.yaml$", objects$file))
})

test_that("store_yaml works with custom names", {


  z <- new_workspace()
  test_list <- list(data = "test")

  z <- store_yaml(z, test_list, "file.yaml", name = "custom_name", subdir = "test")

  objects <- list_object_in_workspace(z)
  expect_equal(objects$name, "custom_name")
  expect_false("file" %in% objects$name)
})

test_that("store_yaml works with custom timestamps", {


  z <- new_workspace()
  test_list <- list(data = "test")
  custom_timestamp <- "2024-06-15 14:30:00"

  z <- store_yaml(z, test_list, "timestamped.yaml", subdir = "test", timestamp = custom_timestamp)

  retrieved_timestamp <- read_timestamp(z, "timestamped", "yaml", subdir = "test")
  expect_equal(retrieved_timestamp, custom_timestamp)
})

test_that("store_yaml replaces existing YAML files", {


  z <- new_workspace()

  # Store first YAML
  first_list <- list(version = 1, data = "old")
  z <- store_yaml(z, first_list, "replaceable.yaml", subdir = "test")

  # Store second YAML with same name
  second_list <- list(version = 2, data = "new", extra = "field")
  z <- store_yaml(z, second_list, "replaceable.yaml", subdir = "test")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 1)
  expect_equal(objects$name, "replaceable")
})

test_that("store_yaml handles different data types in lists", {

  z <- new_workspace()

  # List with various R data types
  mixed_list <- list(
    string = "text",
    number = 42.5,
    integer = 10L,
    logical = TRUE,
    vector = c(1, 2, 3),
    character_vector = c("a", "b", "c"),
    nested = list(
      inner_string = "nested text",
      inner_number = 99
    )
  )

  z <- store_yaml(z, mixed_list, "mixed_types.yaml", subdir = "test")

  objects <- list_object_in_workspace(z)
  expect_equal(nrow(objects), 1)
  expect_equal(objects$type, "yaml")
})

test_that("read_yaml_in_workspace retrieves YAML objects correctly", {


  z <- new_workspace()

  # Create and store YAML
  original_list <- list(
    name = "John Doe",
    age = 30,
    active = TRUE,
    scores = c(85, 90, 78)
  )

  z <- store_yaml(z, original_list, "test.yaml", subdir = "configs")

  # Read back the YAML
  retrieved_list <- read_yaml_in_workspace(z, "test", subdir = "configs")

  # Check that it's a list
  expect_type(retrieved_list, "list")

  # Check data integrity
  expect_equal(retrieved_list$name, "John Doe")
  expect_equal(retrieved_list$age, 30)
  expect_equal(retrieved_list$active, TRUE)
  expect_equal(retrieved_list$scores, c(85, 90, 78))
})

test_that("read_yaml_in_workspace preserves nested structures", {


  z <- new_workspace()

  # Create nested structure
  nested_list <- list(
    database = list(
      host = "localhost",
      port = 5432,
      settings = list(
        timeout = 30,
        retry = TRUE
      )
    ),
    features = list(
      caching = TRUE,
      logging = FALSE
    )
  )

  z <- store_yaml(z, nested_list, "nested.yaml", subdir = "configs")
  retrieved_list <- read_yaml_in_workspace(z, "nested", subdir = "configs")

  # Check nested structure preservation
  expect_equal(retrieved_list$database$host, "localhost")
  expect_equal(retrieved_list$database$port, 5432)
  expect_equal(retrieved_list$database$settings$timeout, 30)
  expect_equal(retrieved_list$database$settings$retry, TRUE)
  expect_equal(retrieved_list$features$caching, TRUE)
  expect_equal(retrieved_list$features$logging, FALSE)
})

test_that("read_yaml_in_workspace works without specifying subdir", {


  z <- new_workspace()

  test_list <- list(key = "value", number = 42)
  z <- store_yaml(z, test_list, "global.yaml", subdir = "root")

  # Should work when subdir is not specified if name is unique
  retrieved_list <- read_yaml_in_workspace(z, "global")

  expect_equal(retrieved_list$key, "value")
  expect_equal(retrieved_list$number, 42)
})

test_that("read_yaml_in_workspace works after pack/unpack cycle", {


  z <- new_workspace()

  # Create and store YAML
  config_list <- list(
    app_name = "MyApp",
    version = "1.0.0",
    settings = list(
      debug = FALSE,
      port = 8080
    )
  )
  z <- store_yaml(z, config_list, "app_config.yaml", subdir = "configs")

  # Pack and unpack
  packed_file <- tempfile(fileext = ".zip")
  pack_workspace(z, packed_file)
  z2 <- unpack_workspace(packed_file)

  # Read from unpacked workspace
  retrieved_list <- read_yaml_in_workspace(z2, "app_config", subdir = "configs")

  expect_equal(retrieved_list$app_name, "MyApp")
  expect_equal(retrieved_list$version, "1.0.0")
  expect_equal(retrieved_list$settings$debug, FALSE)
  expect_equal(retrieved_list$settings$port, 8080)
})

test_that("store_yaml validates input types", {
  z <- new_workspace()

  # Test non-list inputs
  expect_error(store_yaml(z, "not a list", "test.yaml", subdir = "test"))
  expect_error(store_yaml(z, 123, "test.yaml", subdir = "test"))
  expect_error(store_yaml(z, c(1, 2, 3), "test.yaml", subdir = "test"))
  expect_error(store_yaml(z, data.frame(x = 1:3), "test.yaml", subdir = "test"))
})

test_that("store_yaml validates string parameters", {


  z <- new_workspace()
  test_list <- list(key = "value")

  expect_error(store_yaml(z, test_list, 123, subdir = "test"))
  expect_error(store_yaml(z, test_list, "test.yaml", subdir = 123))
  expect_error(store_yaml(z, test_list, "test.yaml", subdir = "test", timestamp = 123))
})

test_that("read_yaml_in_workspace handles nonexistent files", {


  z <- new_workspace()

  expect_error(read_yaml_in_workspace(z, "nonexistent"))
  expect_error(read_yaml_in_workspace(z, "nonexistent", subdir = "test"))
})

test_that("store_yaml and read_yaml_in_workspace handle empty lists", {
  z <- new_workspace()

  # Store empty list
  empty_list <- list()
  z <- store_yaml(z, empty_list, "empty.yaml", subdir = "test")

  # Read back empty list
  retrieved_list <- read_yaml_in_workspace(z, "empty", subdir = "test")

  expect_type(retrieved_list, "list")
  expect_length(retrieved_list, 0)
})

test_that("YAML handles special characters and unicode", {

  z <- new_workspace()

  # List with special characters and unicode
  special_list <- list(
    text_with_spaces = "Hello World",
    special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?",
    unicode = "café, naïve, résumé, 中文, العربية, 🎉",
    multiline = "Line 1\nLine 2\nLine 3"
  )

  z <- store_yaml(z, special_list, "special.yaml", subdir = "test")
  retrieved_list <- read_yaml_in_workspace(z, "special", subdir = "test")

  expect_equal(retrieved_list$text_with_spaces, "Hello World")
  expect_equal(retrieved_list$special_chars, "!@#$%^&*()_+-=[]{}|;':\",./<>?")
  expect_equal(retrieved_list$unicode, "café, naïve, résumé, 中文, العربية, 🎉")
  expect_equal(retrieved_list$multiline, "Line 1\nLine 2\nLine 3")
})
