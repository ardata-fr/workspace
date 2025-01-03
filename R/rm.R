#' @export
#' @importFrom dplyr rows_delete
#' @title Remove an Object in a Workspace
#' @description
#' Remove an object stored in a workspace.
#' @param x the workspace
#' @param name name of the object stored in the workspace
#' @param type content type
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
#' rm_object_in_workspace(
#'   x = z,
#'   name = "obj",
#'   type = "rds",
#'   subdir = "r-object"
#' )
rm_object_in_workspace <- function(x, name, type, subdir = NULL) {

  objects_desc <- list_object_in_workspace(x)

  objects_desc <- objects_desc[objects_desc$type %in% type, ]
  if (nrow(objects_desc) < 1) {
    cli_abort("Value of {.code type} cannot be found in workspace.")
  }

  objects_desc <- objects_desc[objects_desc$name %in% name, ]
  if (nrow(objects_desc) < 1) {
    cli_abort("Value of {.code name} cannot be found in workspace.")
  }

  if (!is.null(subdir)) {
    objects_desc <- objects_desc[objects_desc$subdir %in% subdir, ]
    if (nrow(objects_desc) < 1) {
      cli_abort("Value of {.code subdir} cannot be found in workspace.")
    }
  }

  if (nrow(objects_desc) > 1) {
    cli_abort("Value of {.code name} is not unique in workspace.")
  }

  objects_desc$timestamp <- NULL

  objects_descriptions <- read_objects_description(x)
  objects_descriptions <- rows_delete(objects_descriptions, objects_desc, by = c("name", "subdir", "type", "file"))

  filename <- file.path(x$dir, objects_desc$file)
  unlink(filename, force = TRUE)

  remaining_files <- list.files(dirname(filename))
  if (length(remaining_files) < 1) {
    unlink(dirname(filename), recursive = TRUE, force = TRUE)
  }

  save_objects_description(x, objs_desc = objects_descriptions)

  x
}
