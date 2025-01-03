#' @importFrom utils packageVersion
pkg_version_str <- function(package_name) {
  pkg_version <- packageVersion(package_name)
  format(pkg_version)
}
