#' @title Manage Collections of Datasets and Objects
#'
#' @description
#' Create, store, read and manage structured collections of
#' datasets and other objects using a 'workspace', then bundle it into a
#' compressed archive.  Using open and interoperable formats makes it
#' possible to exchange bundled data from 'R' to other languages such as
#' 'Python' or 'Julia'.  Multiple formats are supported 'Parquet',
#' 'JSON', 'yaml', spatial data and raster data are supported.
#'
#' Examples of usage are:
#'
#' * Creating a collection of datasets and parameters to save in a zip file.
#' * Importing and exporting datasets and settings into from a shiny app.
#'
#' To get started with workspace, you will need to create an empty one with
#' [new_workspace()] or unpack an existing workspace zip file with
#' [unpack_workspace()]. These functions return a list with class workspace that
#' which you can interact with using the following functions.
#'
#'
#' @section Storing and reading workspace data:
#'
#' To store data:
#' -  [store_dataset()] for data.frames, sf and splatRaster
#' -  [store_json()] for json strings
#' -  [store_rds()] for R objects
#' -  [store_yaml] for storing a list in YAML format
#'
#' Each object stored in a workspace will have an associated name which can then be used
#' to retrieve the data with the following functions:
#'
#' -  For interactive use, [print()] the workspace to the console to show the directory
#' of the workspace and its contents
#' -  [list_object_in_workspace()] to list objects in a workspace
#' -  [read_timestamp()] to read a timestamp associated with an object of a
#' workspace. Each time an object is stored, the timestamp is recorded
#' -  [read_dataset_in_workspace()] to read a dataset in a workspace
#' -  [read_raster_in_workspace()] to read a raster file in a workspace
#' -  [read_rds_in_workspace()] to read an Rds file in a workspace
#' -  [read_json_str_in_workspace()] to read a JSON string in a workspace
#' -  [read_yaml_in_workspace()] to read a YAML file in a workspace
#'
#' Stored objects can be removed with:
#' -  [rm_object_in_workspace()] for any object
#' -  [delete_dataset()] for datasets

#' @section Workspace management:
#'
#' A workspace an also be compressed into a zip file using [pack_workspace()]
#' and unzipped using [unpack_workspace()], which allows for saving a workspace
#' to reuse later or share a zip file with someone else.
#'
#' Workspaces can be cloned using [workspace_copy()] or merged together with
#' [workspace_bind()].
#'
#' @section Dataset formats:
#'
#' Tabular datasets, such as objects of class `data.frame` or `tibble` are stored
#' as `Parquet` files. Geosptial formats are supported using [store_dataset()]
#' for `splatRaster` objects from the `terra` package and `sf` objects from the
#' `sf` package.
#'
#' @section What is a workspace?:
#'
#' A `workspace` object is a list with class `workspace` with 2 elements; a directory folder
#' and version number.
#'
#' The former is where the files are stored and read from, and the latter,
#' contains a version number for the `workspace` structure in case new versions
#' are required in future. A `workspace` therefore represents a folder and its files.
#'
#' Note, to ensure validity of workspaces, they should be created and modified
#' with functions from this package, not manually.
#'
#' @docType package
#' @name workspace
"_PACKAGE"
