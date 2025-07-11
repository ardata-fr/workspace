#' @importFrom utils packageVersion globalVariables
pkg_version_str <- function(package_name) {
  pkg_version <- packageVersion(package_name)
  format(pkg_version)
}

globalVariables(c(".data"))
