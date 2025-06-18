#' Retrieve FRED Series Data as a Tibble
#'
#' Downloads and parses time series data from the Federal Reserve Economic Data (FRED) API, returning a tidy tibble of observations.
#'
#' @param series_id Character. The FRED series ID to retrieve (e.g., \code{"ATNHPIUS16180Q"}).
#' @param api_key Character. Your FRED API key. If missing, function will look for an environment variable named \code{FRED_API_KEY}.
#' @param numeric Logical. If TRUE (default), converts the 'value' column to numeric.
#'
#' @return A tibble containing the FRED observations for the specified series.
#'
#' @details
#' The function uses the FRED API to retrieve economic time series data. You can supply the API key as an argument, or save it in your environment with \code{Sys.setenv(FRED_API_KEY = "your_api_key")}.
#'
#' @importFrom httr GET content http_type
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' Sys.setenv(FRED_API_KEY = "your_api_key_here")
#' df <- getFRED("ATNHPIUS16180Q")
#' }
#'
#' @export
getFRED <- function(series_id, api_key, numeric = TRUE) {
  # Check for API key: use argument if provided, else check environment
  if (missing(api_key) || is.null(api_key) || api_key == "") {
    api_key <- Sys.getenv("FRED_API_KEY")
    if (identical(api_key, "") || is.null(api_key)) {
      stop("API key not provided. Supply it as `api_key` argument or set the `FRED_API_KEY` environment variable. API key can be obtained from https://fred.stlouisfed.org/docs/api/api_key.html")
    }
  }

  # Build FRED API URL
  url <- sprintf(
    "https://api.stlouisfed.org/fred/series/observations?series_id=%s&api_key=%s&file_type=json",
    series_id, api_key
  )

  # Query FRED API
  response <- httr::GET(url)
  if (httr::http_type(response) != "application/json") {
    stop("FRED API did not return JSON. Check your API key and series_id.")
  }

  # Parse JSON content
  content_raw <- httr::content(response, as = "text", encoding = "UTF-8")
  json_data <- jsonlite::fromJSON(content_raw)

  # Check for errors in API response
  if (!is.null(json_data$error_code)) {
    stop(sprintf("FRED API error: %s", json_data$error_message))
  }

  # Convert observations to tibble and mutate as needed
  df <- tibble::as_tibble(json_data$observations) %>%
    dplyr::mutate(
      date = as.Date(date)
    )

  # Conditionally convert value column to numeric
  if (numeric && "value" %in% names(df)) {
    df <- dplyr::mutate(df, value = as.numeric(value))
  }

  return(df)
}

# Example usage:
# Sys.setenv(FRED_API_KEY = "your_api_key_here")
# df <- getFRED("ATNHPIUS16180Q")

