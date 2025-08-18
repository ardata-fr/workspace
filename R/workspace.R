# utils -----

## static names -----
.datasets_description <- "dataset_description"
.datasets_directory <- "datasets"
.geospatial_directory <- "geospatial"
.raster_directory <- "raster"
.assets_directory <- "assets"
.version_file <- "version"

## dataset_description -----
#' @importFrom tibble tibble
dataset_description <- function(file, name, subdir, type, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {
  tibble(file = file, name = name, subdir = subdir, type = type, timestamp = timestamp)
}

save_objects_description <- function(x, objs_desc) {
  file_objects_description <- file.path(
    x$dir,
    paste0(.datasets_description, ".parquet")
  )
  create_datasets_folder(x)
  write_parquet(objs_desc, file_objects_description)
}

read_objects_description <- function(x) {
  file_objects_description <- file.path(
    x$dir,
    paste0(.datasets_description, ".parquet")
  )
  read_parquet(file_objects_description, mmap = FALSE)
}

create_datasets_folder <- function(x) {
  # create directory .datasets_directory
  dir_datasets <- file.path(x$dir, .datasets_directory)
  dir.create(path = dir_datasets, showWarnings = FALSE, recursive = FALSE)
  invisible(TRUE)
}
create_assets_folder <- function(x) {
  # create directory .assets_directory
  dir_assets <- file.path(x$dir, .assets_directory)
  dir.create(path = dir_assets, showWarnings = FALSE, recursive = FALSE)
  invisible(TRUE)
}

## workspace structure -----
as_workspace_structure <- function(dir, obj_desc, version) {
  x <- list(
    dir = dir,
    version = version
  )
  class(x) <- "workspace"

  create_datasets_folder(x)
  create_assets_folder(x)

  save_objects_description(x, objs_desc = obj_desc)

  x
}

# main functions -----

#' @importFrom rlang new_environment
#' @importFrom cli cli_abort
#' @title Create a new workspace
#' @description
#' Create a new workspace, i.e. an object made of an
#' R environment and a directory where datasets will be
#' stored as parquet files.
#' @param dir directory used to store datasets
#' @export
#' @examples
#' z <- new_workspace()
#' z
#' @family functions to manage workspaces
new_workspace <- function(dir = tempfile(pattern = "ws")) {

  if (dir.exists(dir)) {
    cli_abort("Directory already exists {.path {dir}}.")
  }

  objects_desc <- dataset_description(file = character(), name = character(), subdir = character(), type = character())

  dir.create(path = dir, showWarnings = FALSE, recursive = TRUE)

  version <- pkg_version_str(package_name = "workspace")

  file_version <- file.path(dir, .version_file)
  writeLines(version, file_version, useBytes = TRUE)
  x <- as_workspace_structure(
    dir = dir,
    obj_desc = objects_desc,
    version = version
  )
  x
}

#' @export
#' @title Pack a workspace
#' @description
#' Pack a workspace into a compressed file.
#' @param x workspace to pack
#' @param file File to produce from the workspace
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
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
#' z <- store_rds(
#'   x = z,
#'   obj = mtcars,
#'   filename = "obj.rds",
#'   timestamp = "2023-11-12 11:37:41",
#'   subdir = "r-object"
#' )
#' file <- tempfile(fileext = ".zip")
#' pack_workspace(x = z, file = file)
#' @family functions to manage workspaces
pack_workspace <- function(x, file) {
  pack_folder(x$dir, target = file)
}

#' @importFrom utils head
#' @importFrom rlang env_has env_names
#' @importFrom arrow read_parquet
#' @export
#' @title Unpack a workspace
#' @description
#' Unpack a compressed file into a workspace.
#' @param file Packed workspace
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
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
#' z <- store_rds(
#'   x = z,
#'   obj = mtcars,
#'   filename = "obj.rds",
#'   timestamp = "2023-11-12 11:37:41",
#'   subdir = "r-object"
#' )
#' file <- tempfile(fileext = ".zip")
#' pack_workspace(x = z, file = file)
#'
#' z <- unpack_workspace(file = file)
#' z
#' @family functions to manage workspaces
unpack_workspace <- function(file) {

  dir <- tempfile(pattern = "ws")

  unpack_ok <- TRUE
  tryCatch(
    unpack_folder(file = file, folder = dir),
    error = function(e) {
      unpack_ok <<- FALSE
    }
  )

  file_version <- file.path(dir, .version_file)
  version_str <- "0.0.1"
  if (grepl("\\.ws$", file)) {
    version_str <- "0.0.1"
  } else if (unpack_ok && file.exists(file_version)) {
    version_str <- head(readLines(file_version), n = 1)
  } else if (!unpack_ok) {
    version_str <- "0.0.2"
  } else {
    cli_abort("Could not identify workspace version.")
  }
  version <- numeric_version(version_str)

  if (version >= numeric_version("0.1.0")) {
    file_datasets_description <- file.path(
      dir,
      paste0(.datasets_description, ".parquet")
    )
    objects_desc <- read_parquet(file_datasets_description, mmap = FALSE)
    unlink(file_datasets_description, force = TRUE)
    x <- as_workspace_structure(
      dir = dir,
      obj_desc = objects_desc,
      version = version_str
    )
  } else if (version <= numeric_version("0.0.2")) {
    x <- read_ws_env(file)
  } else {
    cli_abort("Unrecognized workspace.")
  }

  x
}

read_ws_env <- function(file) {
  ws <- readRDS(file)
  x <- new_workspace()
  if (env_has(ws, "data")) {
    obj <- ws$data
    for (env_name_element in env_names(obj)) {
      e <- obj[[env_name_element]]
      if (is.environment(e) && all(c("name", "data") %in% env_names(e))) {
        x <- store_dataset(x = x, dataset = e$data, name = e$name)
      }
    }
  }
  if (env_has(ws, "options")) {
    obj <- ws$options

    for (env_name_element in env_names(obj)) {
      e <- obj[[env_name_element]]
      if (is.list(e) && "pageFlows" %in% env_name_element) {
        if (!is.null(e$rvalues$json_data)) {
          x <- store_json(
            x = x,
            json_str = e$rvalues$json_data,
            filename = "flows.json",
            timestamp = format(e$rvalues$time, "%Y-%m-%d %H:%M:%S"),
            subdir = "flows"
          )
        }
      } else if (is.list(e) && !is.null(e$rvalues)) {
        x <- store_rds(
          x = x,
          obj = e$rvalues,
          filename = env_name_element,
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          subdir = env_name_element
        )
      } else if (is.list(e) && !is.null(e$rv)) {
        x <- store_rds(
          x = x,
          obj = e$rv,
          filename = env_name_element,
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          subdir = env_name_element
        )
      }
    }
  }

  x
}

#' @export
#' @importFrom cli cli_inform
print.workspace <- function(x, ...) {
  objects_descriptions <- read_objects_description(x)
  cli_inform("workspace directory is {.path {x$dir}}")
  print(objects_descriptions)
  invisible(x)
}

#' @export
#' @title Clone a Workspace
#' @description
#' Clone/Copy a workspace. This function is necessary as
#' assignment would not change internal directory that is certainly
#' defined in a temporary directory that will be removed when
#' R session will be stopped.
#' @param x the workspace to copy
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
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
#' z <- store_rds(
#'   x = z,
#'   obj = mtcars,
#'   filename = "obj.rds",
#'   timestamp = "2023-11-12 11:37:41",
#'   subdir = "r-object"
#' )
#'
#' new_z <- workspace_copy(z)
#' @family functions to manage workspaces
workspace_copy <- function(x) {
  file <- tempfile(fileext = ".zip")
  pack_workspace(x = x, file = file)
  z <- unpack_workspace(file = file)
  create_datasets_folder(z)
  create_assets_folder(z)
  z
}

#' @export
#' @importFrom dplyr select all_of semi_join anti_join
#' @importFrom cli cli_warn
#' @title Bind a Workspace into Another Workspace
#' @description
#' Bind a workspace into another workspace.
#' @param x the workspace where y elements will be copied
#' @param y the workspace to add to `x` workspace
#' @param replace scalar logical, set to TRUE if you want to
#' overwrite existing elements.
#' @examples
#' library(workspace)
#' x <- new_workspace()
#' x <- store_dataset(x = x, dataset = iris, name = "iris_dataset")
#' z <- new_workspace()
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
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
#' z <- store_rds(
#'   x = z,
#'   obj = mtcars,
#'   filename = "obj.rds",
#'   timestamp = "2023-11-12 11:37:41",
#'   subdir = "r-object"
#' )
#'
#' new_x <- workspace_bind(x, z)
#' new_x
#' @family functions to manage workspaces
workspace_bind <- function(x, y, replace = FALSE) {

  if (!inherits(x, "workspace")) {
    cli_abort("x must be an object of class {.code 'workspace'}.")
  }
  if (!inherits(y, "workspace")) {
    cli_abort("y must be an object of class {.code 'workspace'}.")
  }

  x <- workspace_copy(x)
  y <- workspace_copy(y)

  x_objects <- list_object_in_workspace(x)
  y_objects <- list_object_in_workspace(y)
  keys <- c("name", "subdir", "type")

  if (replace) {
    x_to_delete <- semi_join(x_objects, select(y_objects, all_of(keys)), by = keys)
    for(i in seq_len(nrow(x_to_delete))) {
        x <- rm_object_in_workspace(
          x = x,
          name = x_to_delete$name[i],
          type = x_to_delete$type[i],
          subdir = x_to_delete$subdir[i]
        )
    }
    x_objects <- list_object_in_workspace(x)
  } else {
    y_objects <- anti_join(y_objects, x_objects, by = keys)
  }

  for (i in seq_len(nrow(y_objects))) {
    current_new <- y_objects[i, ]

    if ("json" %in% current_new$type) {
      json_str <- read_json_str_in_workspace(
        x = y,
        name = current_new$name,
        subdir = current_new$subdir
        )
      timestamp <- read_timestamp(
        x = y,
        name = current_new$name,
        type = "json",
        subdir = current_new$subdir
      )

      x <- store_json(
        x = x,
        json_str = json_str,
        filename = basename(y_objects[i, ]$file),
        timestamp = timestamp,
        subdir = basename(dirname(y_objects[i, ]$file))
      )
    } else if ("dataset" %in% current_new$type) {
      dataset <- read_dataset_in_workspace(
        x = y,
        name = current_new$name
      )
      timestamp <- read_timestamp(
        x = y,
        name = current_new$name,
        type = "dataset",
        subdir = current_new$subdir
      )

      x <- store_dataset(
        x = x,
        dataset = dataset,
        name = current_new$name,
        timestamp = timestamp
      )
    } else if ("rds" %in% current_new$type) {

      r_obj <- read_rds_in_workspace(
        x = y,
        name = current_new$name,
        subdir = current_new$subdir
      )
      timestamp <- read_timestamp(
        x = y,
        name = current_new$name,
        type = "rds",
        subdir = current_new$subdir
      )

      x <- store_rds(
        x = x,
        obj = r_obj,
        filename = basename(y_objects[i, ]$file),
        name = y_objects[i, ]$name,
        timestamp = timestamp,
        subdir = basename(dirname(y_objects[i, ]$file))
      )
    } else {
      cli_warn("unknown type {current_new$type}.")
    }
  }
  x
}

