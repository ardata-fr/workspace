
<!-- README.md is generated from README.Rmd. Please edit that file -->

# workspace

`workspace` is an R package designed to simplify the creation and
management of workspaces. Its main goal is to provide a standardized
solution for organizing and sharing data in applications that require
workspace management.

This package is aimed at developers and analysts seeking to standardize
data management in multi-environment or cross-application contexts. With
`workspace`, creating, manipulating, and sharing organized archives
becomes straightforward.

## Key features

- Generation of structured archives:
  - Supports adding datasets in parquet format.
  - Enables storing JSON files for semi-structured data.
  - Handles R objects stored in RDS format.
- Enhanced interoperability:
  - Provides a well-defined and consistent structure for organizing
    data.
  - Facilitates data exchange and processing across different
    applications or environments.
- Offers tools to easily read and store data within workspaces.

## Installation

You can install the development version of workspace from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ardata-fr/workspace")
```

## Example of workspace creation

This is a basic example which shows you how to create a workspace:

``` r
library(workspace)
z <- new_workspace()

# store datasets
z <- store_dataset(x = z, dataset = iris, name = "iris_dataset")
z <- store_dataset(x = z, dataset = mtcars, name = "mtcars")

# store json
json_str <- paste0("{\"first_name\": \"John\",\"last_name\": \"Smith\",\"is_alive\": true,",
"\"age\": 27, \"address\": { \"street_address\": \"21 2nd Street\",",
"\"city\": \"New York\",\"state\": \"NY\",\"postal_code\": \"10021-3100\"",
"}}")
z <- store_json(
  x = z,
  name = "json-example",
  json_str = json_str,
  filename = "json-example.json",
  timestamp = "2023-11-12 11:37:41",
  subdir = "blah"
)

# pack workspace as a zip to share it
workspace_zip_file <- tempfile(fileext = ".zip")
pack_workspace(x = z, file = workspace_zip_file)
#> [1] "/private/var/folders/08/2qdvv0q95wn52xy6mxgj340r0000gn/T/RtmpDu03jK/file79c764187a07.zip"
```

## Example of workspace data extraction

This is a basic example which shows you how to extract data from a
workspace:

``` r
z <- unpack_workspace(file = workspace_zip_file)
list_object_in_workspace(z)
#> # A tibble: 3 × 5
#>   file                          name         subdir   type    timestamp         
#>   <chr>                         <chr>        <chr>    <chr>   <chr>             
#> 1 datasets/iris_dataset.parquet iris_dataset datasets dataset 2025-01-11 12:30:…
#> 2 datasets/mtcars.parquet       mtcars       datasets dataset 2025-01-11 12:30:…
#> 3 assets/blah/json-example.json json-example blah     json    2023-11-12 11:37:…

dataset <- read_dataset_in_workspace(z, name = "mtcars")
print(head(dataset))
#> # A tibble: 6 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
#> 3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
#> 4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
#> 5  18.7     8   360   175  3.15  3.44  17.0     0     0     3     2
#> 6  18.1     6   225   105  2.76  3.46  20.2     1     0     3     1

# store json
json_str <- read_json_str_in_workspace(z, name = "json-example", subdir = "blah")
substr(json_str, 1, 20) |> print()
#> [1] "{\"first_name\": \"John"
```
