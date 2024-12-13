#' Replace Missing Values with Column Medians
#'
#' This function replaces missing values (NA) in numeric columns of a data frame
#' with the median of the respective column.
#'
#' @param data A data frame. Numeric columns are analyzed for missing values.
#' @return A data frame where NA values in numeric columns are replaced with the column medians.
#' @examples
#' df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, 5, 6, 7), c = c("x", "y", NA, "z"))
#' new_data(df)
#' @export

impute_with_median <- function(dataset) {

  if (!is.data.frame(dataset)) {
    stop("Input must be a data frame.")
  }

  for (col_name in names(data)) {

    if (is.numeric(data[[col_name]])) {

      median_value <- median(data[[col_name]], na.rm = TRUE)
      data[[col_name]][is.na(data[[col_name]])] <- median_value
    }
  }
  return(data)
}


example_data <- data.frame(
  a = c(1, 2, NA, 4),
  b = c(NA, 5, 6, NA),
  c = c("x", "y", "z", "w")
)


imputed_data <- median_imputer(example_data)
print(imputed_data)

