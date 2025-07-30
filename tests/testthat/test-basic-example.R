test_that("basic workflow is correct", {

  z <- new_workspace()
  expect_s3_class(z, "workspace")
  expect_true(!is.null(z$dir))
  expect_true(!is.null(z$version))

  # store datasets
  z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
  z <- store_dataset(
    x = z,
    dataset = tibble::rownames_to_column(mtcars, var = "car"),
    name = "mtcars"
    )


  # store json
  json_str <- paste0("{\"first_name\": \"John\",\"last_name\": \"Smith\",\"is_alive\": true,",
                     "\"age\": 27, \"address\": { \"street_address\": \"21 2nd Street\",",
                     "\"city\": \"New York\",\"state\": \"NY\",\"postal_code\": \"10021-3100\"",
                     "}}")

  z <- store_json(
    x = z,
    name = "json-example",
    json_str = json_str,
    filename = "json-example.json",
    timestamp = "2023-11-12 11:37:41",
    subdir = "blah"
  )

  desc <- list_object_in_workspace(z)
  expect_identical(desc$file, c(
    "datasets/iris_dataset.parquet",
    "datasets/mtcars.parquet",
    "assets/blah/json-example.json"
  ))
  expect_identical(desc$name, c(
    "iris_dataset", "mtcars", "json-example"
  ))
  expect_identical(desc$subdir, c(
    "datasets", "datasets", "blah"
  ))
  expect_identical(desc$type, c(
    "dataset", "dataset", "json"
  ))

  # pack workspace as a zip to share it
  workspace_zip_file <- tempfile(fileext = ".zip")
  pack_workspace(x = z, file = workspace_zip_file)


  new_z <- unpack_workspace(workspace_zip_file)
  new_desc <- list_object_in_workspace(new_z)

  expect_identical(desc, new_desc)

  dataset <- read_dataset_in_workspace(z, name = "mtcars")
  expect_identical(tibble::as_tibble(tibble::rownames_to_column(mtcars, var = "car")), dataset)

  # store json
  new_json_str <- read_json_str_in_workspace(z, name = "json-example", subdir = "blah")
  expect_identical(json_str, new_json_str)
})
