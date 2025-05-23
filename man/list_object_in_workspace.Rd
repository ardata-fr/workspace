% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readers.R
\name{list_object_in_workspace}
\alias{list_object_in_workspace}
\title{List Objects in a Workspace}
\usage{
list_object_in_workspace(x)
}
\arguments{
\item{x}{the workspace}
}
\description{
List all objects stored in a workspace object and
returns results as a tibble.
}
\examples{
library(workspace)
dir_tmp <- tempfile(pattern = "ws")
z <- new_workspace(dir = dir_tmp)
z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")
list_object_in_workspace(z)
}
\seealso{
Other functions to read in a workspace: 
\code{\link{read_dataset_in_workspace}()},
\code{\link{read_json_str_in_workspace}()},
\code{\link{read_rds_in_workspace}()},
\code{\link{read_timestamp}()}
}
\concept{functions to read in a workspace}
