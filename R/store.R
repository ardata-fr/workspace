#' @importFrom stringi stri_trans_general
#' @importFrom arrow write_parquet
#' @importFrom dplyr rows_upsert
#' @export
#' @title Store a dataset into a workspace
#' @description
#' Store a dataset as a parquet file into an existing workspace.
#' @param x the workspace object
#' @param dataset the data.frame or sf to store in the workspace.
#' @param name name associated with the data.frame, if an workspace file with this name exists
#' already it will be replaced.
#' @param timestamp A timestamp string to associate with the entry in the workspace.
#' @details
#'
#' If the input dataset is `sf` then a metadata yaml file is also written to the workspace
#' in the `assets/sf_metadata/{name}.yaml` location. This contains the `sf` column name
#' and the CRS that can be overidden when writing .gpkg data to disk.
#'
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
#' z
#' @family functions to write in a workspace
store_dataset <- function(x, dataset, name, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {
  UseMethod("store_dataset", dataset)
}

#' @export
store_dataset.data.frame <- function(x, dataset, name, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {
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
#' @importFrom yaml write_yaml
store_dataset.sf <- function(x, dataset, name, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    cli_abort("{.val sf} package must be installed to store dataset of type {.cls sf}.")
  }

  base_file <- stri_trans_general(name, id = "latin-ascii")
  contains_geo_ext <- grepl(
    pattern = "\\.(gpkg|shp|geojson)$", x = base_file, ignore.case = TRUE)
  if (!contains_geo_ext) {
    base_file <- paste0(base_file, ".gpkg")
  } else {
    if (grepl(pattern = "\\.(shp|geojson)$", x = base_file, ignore.case = TRUE)) {
      cli_warn("{.val .shp} or {.val .geojson} extension detected in {.arg name}, file will be writen in {.arg .gpkg} format.")
    }
    base_file <- gsub(
      pattern = "\\.(gpkg|shp|geojson)$",
      replacement = ".gpkg",
      x = base_file,
      ignore.case = TRUE
    )
  }
  filepath <- file.path(x$dir, .datasets_directory, base_file)

  sf::st_write(dataset, filepath, quiet = TRUE, delete_dsn = TRUE)

  objects_desc <- dataset_description(
    file = file.path(.datasets_directory, base_file),
    subdir = .geospatial_directory,
    name = name,
    type = "geospatial",
    timestamp = timestamp
  )
  objects_descriptions <- read_objects_description(x)
  objects_descriptions <- rows_upsert(objects_descriptions, objects_desc, by = "name")
  save_objects_description(x, objs_desc = objects_descriptions)

   # Store metadata for read_dataset_in_workspace()
  # Using gpkg means geometry col is written as geom (not ideal)
  # https://github.com/r-spatial/sf/issues/719
  sf_metadata = list(
    sf_column = attr(dataset, "sf_column"),
    crs = sf::st_crs(dataset)$input
  )
  yaml_file <- gsub("\\.gpkg$", ".yaml", base_file)
  yaml_file <- file.path(x$dir, .assets_directory, "sf_metadata", yaml_file)
  dir.create(dirname(yaml_file), showWarnings = FALSE, recursive = TRUE)
  yaml::write_yaml(x = sf_metadata,file = yaml_file)
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
#' @param name name associated with the object, if an workspace file with this name exists
#' already it will be replaced.
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
#' @param name name associated with the object, if an workspace file with this name exists
#' already it will be replaced.
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
    name = name,
    subdir = subdir,
    type = "rds",
    timestamp = timestamp
  )

  objects_descriptions <- read_objects_description(x)
  objects_descriptions <- rows_upsert(objects_descriptions, objects_desc, by = "name")
  save_objects_description(x, objs_desc = objects_descriptions)

  x
}

#' @export
#' @importFrom tools file_path_sans_ext
#' @importFrom rlang is_list
#' @importFrom yaml write_yaml
#' @title Store a list as YAML in a workspace
#' @description
#' Saves a list object as a YAML file in an existing workspace.
#'
#' This function allows users to save R list objects as YAML files into a specified workspace.
#' The file is saved under the provided filename and can be organized within
#' a specific subdirectory for better management.
#' @param x The workspace object.
#' @param list The R list object to save as YAML in the workspace.
#' @param filename The name of the file used to store the YAML file in the workspace.
#' @param name name associated with the object, if a workspace file with this name exists
#' already it will be replaced.
#' @param timestamp A timestamp string to associate with the entry in the workspace.
#' @param subdir A subdirectory within the asset directory where the YAML file will be stored.
#' @return return the workspace object
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#'
#' config_list <- list(
#'   database = list(
#'     host = "localhost",
#'     port = 5432,
#'     name = "mydb"
#'   ),
#'   settings = list(
#'     debug = TRUE,
#'     max_connections = 100
#'   )
#' )
#' z <- store_yaml(
#'   x = z,
#'   list = config_list,
#'   filename = "config.yaml",
#'   timestamp = "2023-11-12 11:37:41",
#'   subdir = "configs"
#' )
#' z
#' @family functions to write in a workspace
store_yaml <- function(x, list, filename, name = NULL, subdir, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {

  if (!inherits(list, 'list')) {
    cli_abort("Argument {.code list} must be a {.cls list} object.")
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

  contains_yaml_ext <- grepl(pattern = "\\.yaml", x = filename, ignore.case = TRUE)
  if (!contains_yaml_ext) {
    filename <- paste0(filename, ".yaml")
  }

  if (is.null(name)) {
    name <- file_path_sans_ext(filename)
  }
  if (!is_string(name)) {
    cli_abort("Argument {.code name} must be a single character string.")
  }

  yaml_filepath <- file.path(x$dir, .assets_directory, subdir, filename)

  dir.create(path = dirname(yaml_filepath), showWarnings = FALSE, recursive = TRUE)

  yaml::write_yaml(list, yaml_filepath)

  objects_desc <- dataset_description(
    file = file.path(.assets_directory, subdir, filename),
    subdir = subdir,
    name = name,
    type = "yaml",
    timestamp = timestamp
  )

  objects_descriptions <- read_objects_description(x)
  objects_descriptions <- rows_upsert(objects_descriptions, objects_desc, by = "name")
  save_objects_description(x, objs_desc = objects_descriptions)

  x
}

