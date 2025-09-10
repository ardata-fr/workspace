#' @export
#' @title List Objects in a Workspace
#' @description
#' List all objects stored in a workspace object and
#' returns results as a tibble.
#' @param x the workspace
#' @return Tibble object containing the following columns:
#' -  `file`: file path to stored object relative to workspace directory
#' -  `name`: name given to object upon storing
#' -  `subdir`: subdirectory of file, such as 'datasets' or 'assets'.
#' -  `type`: file type such as 'dataset', 'geospatial', 'yaml', etc...
#' -  `timestamp`: timestamp of last modification
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
#' list_object_in_workspace(z)
#' @family functions to read in a workspace
#' @seealso [workspace] for package documentation
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
#' @return A `tibble` if the dataset is stored as parquet, and a
#' `sf` object if it is stored as a geospatial dataset
#' (see [store_dataset()] for details).
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
#' z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
#' read_dataset_in_workspace(z, name = "mtcars")
#' @family functions to read in a workspace
#' @seealso [workspace] for package documentation
read_dataset_in_workspace <- function(x, name) {
  objs <- list_object_in_workspace(x)
  objs <- objs[objs$type %in% c("dataset", "geospatial"), ]
  objs <- objs[objs$name %in% name, ]

  if (nrow(objs) < 1) {
    cli_abort("Value of {.code name} cannot be found in workspace.")
  }
  if (nrow(objs) > 1) {
    cli_abort("Value of {.code name} is not unique in workspace.")
  }
  file <- file.path(x$dir, objs$file)

  if (objs$type %in% "dataset"){
    read_parquet(file, mmap = FALSE)

  } else if (objs$type %in% "geospatial") {

    if (!requireNamespace("sf", quietly = TRUE)) {
      cli_abort("{.val sf} package must be installed to read geospatial datasets.")
    }

    geo_sf <- sf::read_sf(file, geometry_column = "geom", quiet = TRUE)

    # Using gpkg means geometry col is written as geom (not ideal)
    # https://github.com/r-spatial/sf/issues/719
    yaml_relative <- file.path(.assets_directory, "sf_metadata", paste0(name, '.yaml'))
    yaml_abs <- file.path(x$dir, yaml_relative)
    if (file.exists(yaml_abs)) {
      metadata <- yaml::read_yaml(yaml_abs)
      sf::st_geometry(geo_sf) <- metadata$sf_column
      sf::st_crs(geo_sf) <- metadata$crs
      geo_sf
    } else {
      cli_warn(paste(
        "No {.arg sf_metadata} file found for name: {.val {name}}.",
        "Geometry column and project may have changed since data was stored."
      ))
      geo_sf
    }
  }
}

#' @export
#' @title Read a raster from a workspace
#' @description
#' Read a raster dataset stored as a TIFF file in a workspace.
#' @param x the workspace object
#' @param name name of the raster dataset stored in the workspace
#' @return The `splatRaster` object that was stored in TIFF format.
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#' # Create and store example raster (requires terra package)
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   r <- terra::rast(ncols=10, nrows=10, vals=1:100)
#'   z <- store_raster(x = z, dataset = r, name = "example_raster")
#'   retrieved_raster <- read_raster_in_workspace(z, name = "example_raster")
#'   retrieved_raster
#' }
#' @family functions to read in a workspace
#' @seealso [workspace] for package documentation
read_raster_in_workspace <- function(x, name) {

  if (!requireNamespace("terra", quietly = TRUE)) {
    cli_abort("{.val terra} package must be installed to read raster data.")
  }
  objs <- list_object_in_workspace(x)
  objs <- objs[objs$type %in% c("raster"), ]
  objs <- objs[objs$name %in% name, ]

  if (nrow(objs) < 1) {
    cli_abort("Value of {.code name} cannot be found in workspace.")
  }
  if (nrow(objs) > 1) {
    cli_abort("Value of {.code name} is not unique in workspace.")
  }
  file <- file.path(x$dir, objs$file)
  raster_data <- terra::rast(file)

  # Restore metadata from YAML file if it exists
  yaml_relative <- file.path(.assets_directory, "raster_metadata", paste0(name, '.yaml'))
  yaml_abs <- file.path(x$dir, yaml_relative)
  if (file.exists(yaml_abs)) {
    metadata <- yaml::read_yaml(yaml_abs)
    if (!is.null(metadata$crs) && metadata$crs != "") {
      terra::crs(raster_data) <- metadata$crs
    }
    if (!is.null(metadata$names) && length(metadata$names) == terra::nlyr(raster_data)) {
      names(raster_data) <- metadata$names
    }
  } else {
    cli_warn(paste(
      "No raster metadata file found for name: {.val {name}}.",
      "CRS and layer names may have changed since data was stored."
    ))
  }

  raster_data
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
#' @return the R object that was stored as an Rds file.
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
#' @family functions to read in a workspace
#' @seealso [workspace] for package documentation
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
#' @return A character string containing the JSON string.
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
#' @family functions to read in a workspace
#' @seealso [workspace] for package documentation
read_json_str_in_workspace <- function(x, name, subdir = NULL) {
  objs <- list_object_in_workspace(x)
  objs <- objs[objs$type %in% "json", ]

  if (nrow(objs) < 1) {
    cli_abort("workspace {.arg x} has no json files.")
  }
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

#' @importFrom yaml read_yaml
#' @export
#' @title Read YAML Object from a Workspace.
#' @description
#' Read a YAML file stored as a list object in a workspace.
#' Returns the YAML content as an R list object.
#' @param x the workspace
#' @param name name associated with the YAML file stored in the workspace
#' @param subdir Optional subdirectory used for the asset to retrieve
#' @return A list object as read from the stored YAML file.
#' @examples
#' library(workspace)
#' dir_tmp <- tempfile(pattern = "ws")
#' z <- new_workspace(dir = dir_tmp)
#'
#' config_list <- list(
#'   database = list(
#'     host = "localhost",
#'     port = 5432
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
#'   name = "app_config",
#'   timestamp = "2023-11-12 11:37:41",
#'   subdir = "configs"
#' )
#' read_yaml_in_workspace(z, "app_config", subdir = "configs")
#' @family functions to read in a workspace
#' @seealso [workspace] for package documentation
read_yaml_in_workspace <- function(x, name, subdir = NULL) {

  objs <- list_object_in_workspace(x)
  objs <- objs[objs$type %in% "yaml", ]

  if (nrow(objs) < 1) {
    cli_abort("workspace {.arg x} has no yaml files.")
  }
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
  yaml_list <- yaml::read_yaml(file)
  yaml_list
}


#' @export
#' @title Read Timestamp associated with an object in a workspace.
#' @description
#' Read a timestamp associated with an object in a workspace.
#' @param x the workspace
#' @param name name of the object stored in the workspace
#' @param type content type
#' @param subdir Optional subdirectory used for the asset to retrieve
#' @return A character string corresponding to the timestamp (date last modified)
#' of the stored object.
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
#' @family functions to read in a workspace
#' @seealso [workspace] for package documentation
read_timestamp <- function(x, name, type, subdir = NULL) {
  objs <- list_object_in_workspace(x)
  objs <- objs[objs$type %in% type, ]
  if (nrow(objs) < 1) {
    cli_abort("Value of {.code type} cannot be found in workspace.")
  }
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
