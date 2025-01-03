#' @export
#' @title List Objects in a Workspace
#' @description
#' List all objects stored in a workspace object and
#' returns results as a tibble.
#' @param x the workspace
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
#' list_object_in_workspace(z)
list_object_in_workspace <- function(x) {
  objects_descriptions <- read_objects_description(x)
  objects_descriptions
}

#' @export
#' @importFrom arrow read_parquet
#' @title Read a Table from a Workspace.
#' @description
#' Read a table from a dataset stored as parquet file
#' in a workspace.
#' @param x the workspace
#' @param name name of the dataset stored in the workspace
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
#' read_dataset_in_workspace(z, name = "mtcars")
read_dataset_in_workspace <- function(x, name) {
  objs <- list_object_in_workspace(x)
  objs <- objs[objs$type %in% "dataset", ]
  objs <- objs[objs$name %in% name, ]

  if (nrow(objs) < 1) {
    cli_abort("Value of {.code name} cannot be found in workspace.")
  }
  if (nrow(objs) > 1) {
    cli_abort("Value of {.code name} is not unique in workspace.")
  }
  file <- file.path(x$dir, objs$file)
  read_parquet(file)
}

#' @export
#' @importFrom arrow read_parquet
#' @title Read an R Object from a Workspace.
#' @description
#' Read an R Object from a dataset stored as a RDS file
#' in a workspace.
#' @param x the workspace
#' @param name name of the object stored in the workspace
#' @param subdir Optional subdirectory used for the asset to retrieve
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
#' z <- store_rds(
#'   x = z,
#'   obj = mtcars,
#'   filename = "obj.rds",
#'   timestamp = "2023-11-12 11:37:41",
#'   subdir = "r-object"
#' )
#' read_rds_in_workspace(z, name = "obj")
read_rds_in_workspace <- function(x, name, subdir = NULL) {
  objs <- list_object_in_workspace(x)
  objs <- objs[objs$type %in% "rds", ]
  objs <- objs[objs$name %in% name, ]

  if (!is.null(subdir)) {
    objs <- objs[objs$subdir %in% subdir, ]
  }

  if (nrow(objs) < 1) {
    cli_abort("Value of {.code name} cannot be found in workspace.")
  }
  if (nrow(objs) > 1) {
    cli_abort("Value of {.code name} is not unique in workspace.")
  }
  file <- file.path(x$dir, objs$file)
  readRDS(file)
}

#' @export
#' @title Read JSON String from a Workspace.
#' @description
#' Read a JSON file a dataset stored as a text file
#' in a workspace.
#' @param x the workspace
#' @param name name associated with the json file stored in the workspace
#' @param subdir Optional subdirectory used for the asset to retrieve
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
#'   name = "an_example",
#'   timestamp = "2023-11-12 11:37:41",
#'   subdir = "blah"
#' )
#' read_json_str_in_workspace(z, "an_example")
read_json_str_in_workspace <- function(x, name, subdir = NULL) {
  objs <- list_object_in_workspace(x)
  objs <- objs[objs$type %in% "json", ]
  objs <- objs[objs$name %in% name, ]

  if (!is.null(subdir)) {
    objs <- objs[objs$subdir %in% subdir, ]
  }

  if (nrow(objs) < 1) {
    cli_abort("Value of {.code name} cannot be found in workspace.")
  }
  if (nrow(objs) > 1) {
    cli_abort("Value of {.code name} is not unique in workspace.")
  }
  file <- file.path(x$dir, objs$file)
  str <- readLines(file, encoding = "UTF-8")
  paste0(str, collapse = "")
}

#' @export
#' @title Read Timestamp associated with a content of a Workspace.
#' @description
#' Read a timestamp associated with a content located in a Workspace.
#' @param x the workspace
#' @param name name of the object stored in the workspace
#' @param type content type
#' @param subdir Optional subdirectory used for the asset to retrieve
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
#' z <- store_rds(
#'   x = z,
#'   obj = mtcars,
#'   filename = "obj.rds",
#'   timestamp = "2023-11-12 11:37:41",
#'   subdir = "r-object"
#' )
#' read_timestamp(z, name = "obj", type = "rds", subdir = "r-object")
read_timestamp <- function(x, name, type, subdir = NULL) {
  objs <- list_object_in_workspace(x)
  objs <- objs[objs$type %in% type, ]
  objs <- objs[objs$name %in% name, ]

  if (!is.null(subdir)) {
    objs <- objs[objs$subdir %in% subdir, ]
  }

  if (nrow(objs) < 1) {
    cli_abort("Value of {.code name} cannot be found in workspace.")
  }
  if (nrow(objs) > 1) {
    cli_abort("Value of {.code name} is not unique in workspace.")
  }
  objs$timestamp
}
