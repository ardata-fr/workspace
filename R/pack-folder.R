#' @noRd
#' @importFrom zip zip
#' @title Compress a folder
#' @description Compress a folder to a target file. The
#' function returns the complete path to target file.
#' @param folder folder to compress
#' @param target path of the archive to create
#' @return Returns path to `target`.
pack_folder <- function(folder, target) {
  target <- absolute_path(target)
  dir_fi <- dirname(target)

  if (!dir.exists(dir_fi)) {
    cli_abort("Argument {.code target} must be located in an existing directory.")
  }
  if (file.access(dir_fi) < 0) {
    cli_abort("Can not write to directory {.path dir_fi}.")
  }
  if (file.exists(target) && file.access(target) < 0) {
    cli_abort("File {.path target} already exists and is not writable.")
  }
  if (!file.exists(target)) {
    x <- suppressWarnings(tryCatch(
      {
        cat("", file = target)
        TRUE
      },
      error = function(e) FALSE,
      finally = unlink(target, force = TRUE)
    ))
    if (!x) {
      cli_abort("File {.path target} cannot be written, please check your permissions.")
    }
  }

  tryCatch(
    zip::zipr(
      zipfile = target,
      include_directories = FALSE,
      files = list.files(path = ".", all.files = FALSE),
      recurse = TRUE,
      root = folder
    ),
    error = function(e) {
      cli_abort("Could not write {.path target}.")
    }
  )

  target
}

#' @noRd
#' @importFrom zip unzip
#' @importFrom cli cli_abort
#' @title Extract files from a zip file
#' @description Extract files from a zip file to a folder. The
#' function returns the complete path to destination folder.
#' @param file path of the archive to unzip
#' @param folder folder to create
#' @return Path to unpacked folder.
unpack_folder <- function(file, folder) {

  if (!is_string(file)) {
    cli_abort("Argument {.code file} must be a single character string.")
  }
  if (!file.exists(file)) {
    cli_abort("File {.file file} does not exists.")
  }

  file_type <- gsub("(.*)(\\.[a-zA-Z0-0]+)$", "\\2", file)

  # force deletion if already existing
  unlink(folder, recursive = TRUE, force = TRUE)

  if (l10n_info()$`UTF-8`) {
    zip::unzip(zipfile = file, exdir = folder)
  } else {
    # unable to unzip a file with accent when on windows
    newfile <- tempfile(fileext = file_type)
    file.copy(from = file, to = newfile)
    zip::unzip(zipfile = newfile, exdir = folder)
    unlink(newfile, force = TRUE)
  }

  absolute_path(folder)
}

#' @importFrom rlang is_string
absolute_path <- function(x) {
  if (!is_string(x)) {
    cli_abort("'x' must be a single character string.")
  }

  epath <- path.expand(x)

  if (file.exists(epath)) {
    epath <- normalizePath(epath, "/", mustWork = TRUE)
  } else {
    if (!dir.exists(dirname(epath))) {
      cli_abort("Directory of {.path {x}} does not exist.")
    }
    cat("", file = epath)
    epath <- normalizePath(epath, "/", mustWork = TRUE)
    unlink(epath)
  }
  epath
}
