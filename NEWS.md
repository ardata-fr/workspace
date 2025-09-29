
# workspace 0.1.6

## Features

- In `store_dataset()` method for sf objects, now preserves `agr` and `sf_column` 
and CRS when storing spatial sf datasets.

## Fixes 

- Column names with spaces now preserved when reading sf datasets from workspace.

## Tests

- Improve round-trip test coverage to ensure what is stored in a workspace is 
the same as what is read.

# workspace 0.1.5

- CRAN release

# workspace 0.1.3

## Features

- Add support for storing and reading yaml files using the `yaml` package.
- Add support for storing and reading geospatial datasets using the `sf` package.
- Add support for storing and reading raster datasets using the `terra` package.

## Tests

- Greatly increase test coverage.

# workspace 0.1.2

## Features

- add new function `delete_dataset()` to delete datasets from workspace.
