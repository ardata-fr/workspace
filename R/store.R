#' @importFrom stringi stri_trans_general
#' @importFrom arrow write_parquet
#' @importFrom dplyr rows_upsert
#' @export
#' @title Store a dataset into a workspace
#' @description
#' Store a dataset as a parquet file into an existing workspace.
#' @param x the workspace object
#' @param dataset the data.frame to store in the workspace
#' @param name name associated with the data.frame
#' @param timestamp A timestamp string to associate with the entry in the workspace.
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
#' z
#' @family functions to write in a workspace
store_dataset <- function(x, dataset, name, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {

  base_file <- stri_trans_general(name, id = "latin-ascii")

  contains_parquet_ext <- grepl(pattern = "\\.parquet", x = base_file, ignore.case = TRUE)
  if (!contains_parquet_ext) {
    base_file <- paste0(base_file, ".parquet")
  }

  filepath <- file.path(x$dir, .datasets_directory, base_file)

  write_parquet(dataset, filepath)
  objects_desc <- dataset_description(
    file = file.path(.datasets_directory, base_file),
    subdir = .datasets_directory,
    name = name,
    type = "dataset",
    timestamp = timestamp
  )

  objects_descriptions <- read_objects_description(x)
  objects_descriptions <- rows_upsert(objects_descriptions, objects_desc, by = "name")
  save_objects_description(x, objs_desc = objects_descriptions)

  x
}

#' @export
#' @importFrom dplyr filter
#' @title Delete a dataset from a workspace
#' @description
#' Delete a dataset stored in a workspace.
#' This function removes the dataset file and updates the workspace's object descriptions.
#' @param x The workspace object.
#' @param data_name The name of the dataset to delete from the workspace.
#' @return return the workspace object
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
#' z <- delete_dataset(x = z, data_name = "iris_dataset")
#' z
#' @family functions to write in a workspace
delete_dataset <- function(x, data_name) {

  base_file <- stri_trans_general(data_name, id = "latin-ascii")

  contains_parquet_ext <- grepl(pattern = "\\.parquet", x = base_file, ignore.case = TRUE)
  if (!contains_parquet_ext) {
    base_file <- paste0(base_file, ".parquet")
  }
  filepath <- file.path(x$dir, .datasets_directory, base_file)
  if (file.exists(filepath)) {
    unlink(filepath, force = TRUE)
  }

  objects_descriptions <- read_objects_description(x)
  objects_descriptions <- filter(.data = objects_descriptions, !.data$name %in% data_name)
  save_objects_description(x, objs_desc = objects_descriptions)

  x
}

#' @export
#' @importFrom tools file_path_sans_ext
#' @title Store a JSON string in a workspace
#' @description
#' Saves a JSON string as a file in an existing workspace.
#'
#' This function allows users to save JSON strings into a specified workspace.
#' The file is saved under the provided filename and can be organized within
#' a specific subdirectory for better management.
#' @param x The workspace object.
#' @param json_str The JSON string to save in the workspace.
#' @param filename The name of the file used to store the JSON string in the workspace.
#' @param name name associated with the object
#' @param timestamp A timestamp string to associate with the entry in the workspace.
#' @param subdir A subdirectory within the asset directory where the JSON file will be stored.
#' @return return the workspace object
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#'
#' json_str <- paste0("{\"first_name\": \"John\",\"last_name\": \"Smith\",\"is_alive\": true,",
#' "\"age\": 27, \"address\": { \"street_address\": \"21 2nd Street\",",
#' "\"city\": \"New York\",\"state\": \"NY\",\"postal_code\": \"10021-3100\"",
#' "}}")
#' z <- store_json(
#'   x = z,
#'   json_str = json_str,
#'   filename = "example.json",
#'   timestamp = "2023-11-12 11:37:41",
#'   subdir = "blah"
#' )
#' z
#' @family functions to write in a workspace
store_json <- function(x, json_str, filename, name = NULL, subdir, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {

  if (!is_string(json_str)) {
    cli_abort("Argument {.code json_str} must be a single character string.")
  }
  if (!is_string(filename)) {
    cli_abort("Argument {.code filename} must be a single character string.")
  }
  if (!is_string(timestamp)) {
    cli_abort("Argument {.code timestamp} must be a single character string.")
  }
  if (!is_string(subdir)) {
    cli_abort("Argument {.code subdir} must be a single character string.")
  }

  contains_json_ext <- grepl(pattern = "\\.json", x = filename, ignore.case = TRUE)
  if (!contains_json_ext) {
    filename <- paste0(filename, ".json")
  }

  if (is.null(name)) {
    name <- file_path_sans_ext(filename)
  }
  if (!is_string(name)) {
    cli_abort("Argument {.code name} must be a single character string.")
  }

  json_filepath <- file.path(x$dir, .assets_directory, subdir, filename)

  dir.create(path = dirname(json_filepath), showWarnings = FALSE, recursive = TRUE)

  writeLines(json_str, json_filepath, useBytes = TRUE)

  objects_desc <- dataset_description(
    file = file.path(.assets_directory, subdir, filename),
    subdir = subdir,
    name = name,
    type = "json",
    timestamp = timestamp
  )

  objects_descriptions <- read_objects_description(x)
  objects_descriptions <- rows_upsert(objects_descriptions, objects_desc, by = "name")
  save_objects_description(x, objs_desc = objects_descriptions)

  x
}


#' @export
#' @title Store an RDS file in a workspace
#' @description
#' Saves an R object as an RDS file in an existing workspace.
#' This function allows users to save R objects in a workspace by serializing them as RDS files.
#' The RDS file is saved under the specified filename and organized within
#' a subdirectory for better structure. The timestamp parameter helps to keep track of when the file was stored.
#' @param x The workspace object.
#' @param obj The R object to save as an RDS file.
#' @param filename The name of the file used to store the RDS file in the workspace.
#' @param name name associated with the object
#' @param timestamp A timestamp string to associate with the entry in the workspace.
#' @param subdir A subdirectory within the asset directory where the RDS file will be stored.
#' @return return the workspace object
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_rds(
#'   x = z,
#'   obj = mtcars,
#'   filename = "obj.rds",
#'   timestamp = "2023-11-12 11:37:41",
#'   subdir = "r-object"
#' )
#' z
#' @family functions to write in a workspace
store_rds <- function(x, obj, filename, name = NULL, subdir, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {

  if (!is_string(filename)) {
    cli_abort("Argument {.code filename} must be a single character string.")
  }
  if (!is_string(timestamp)) {
    cli_abort("Argument {.code timestamp} must be a single character string.")
  }
  if (!is_string(subdir)) {
    cli_abort("Argument {.code subdir} must be a single character string.")
  }

  contains_rds_ext <- grepl(pattern = "\\.rds", x = filename, ignore.case = TRUE)
  if (!contains_rds_ext) {
    filename <- paste0(filename, ".rds")
  }

  if (is.null(name)) {
    name <- file_path_sans_ext(filename)
  }
  if (!is_string(name)) {
    cli_abort("Argument {.code name} must be a single character string.")
  }

  rds_filepath <- file.path(x$dir, .assets_directory, subdir, filename)
  dir.create(path = dirname(rds_filepath), showWarnings = FALSE, recursive = TRUE)

  saveRDS(obj, rds_filepath)

  objects_desc <- dataset_description(
    file = file.path(.assets_directory, subdir, filename),
    name = file_path_sans_ext(filename),
    subdir = subdir,
    type = "rds",
    timestamp = timestamp
  )

  objects_descriptions <- read_objects_description(x)
  objects_descriptions <- rows_upsert(objects_descriptions, objects_desc, by = "name")
  save_objects_description(x, objs_desc = objects_descriptions)

  x
}

