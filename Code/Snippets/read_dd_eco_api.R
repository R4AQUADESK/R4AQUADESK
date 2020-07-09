#' @title  read_dd_eco_api
#' @description
#' This generic function queries the AquaDesk DD-ECO-API. For more information about the api: https://github.com/DigitaleDeltaOrg/dd-eco-api-specs.
#' The function queries the API and returns the results in the form of a data.frame by repeatedly requesting a next page until no more results are returned.
#' Various filters are available per table in the API. See as an example:
#' https://ddeco.aquadesk.nl/v1/measurementobjects/filters
#'
#' Version 1.0
#' @param api_url URL to the endpoint.
#' @param organisation Code of the organisation. Defaults to empty.
#' @param changedate When set, only retrieves changes sinds the specified date. The date formats are: yyyy-MM-dd, yyyy-MM-dd HH:mm:ss
#' @param use_nocount When set to TRUE, the request will take advantage of the new nocount parameter in AquaDesk. This only works in the new test-version of the DD-ECO-API (https://ddeco-test.aquadesk.nl)
#' @param filter Comma separated list of filter parts for the DD-ECO-API to limit the amount of records to return. For filter syntax, see https://ddeco-test.aquadesk.nl
#' @param skip Comma separated list of field names NOT to return. See the specific filters endpoint for details. Example: changedate.  This only works in the new test-version of the DD-ECO-API (https://ddeco-test.aquadesk.nl)
#' @param pagesize Maximum number of items to return per page. Defaults to 10000.
#' @return dataframe with the query results.
#'
#' @import jsonlite
#'
#' @examples
#' read_dd_eco_api("https://ddeco.aquadesk.nl/v1/monitoringnetworks", organisation = "WAM")
#' read_dd_eco_api("https://ddeco.aquadesk.nl/v1/parameters")
#' read_dd_eco_api("https://ddeco.aquadesk.nl/v1/monitoringnetworks", organisation = "WAM", use_nocount = TRUE)
#' read_dd_eco_api("https://ddeco-test.aquadesk.nl/v1/parameters", use_nocount = TRUE, filter = "type:eq:'TAXON'", skip = "changedate")
#' @author Geri Wolters (EcoSys)
#' @export
read_dd_eco_api <- function(api_url, organisation = "", changedate = "", use_nocount = FALSE, filter = "", skip = "", pagesize = 10000) {
  # Start at page 1.
  page <- 1
  # Create an empty dataframe. This frame will be returned.
  df <- dplyr::bind_rows()

  repeat
  {
    # Construct the url.
    URL <- build_dd_eco_api_url(api_url, organisation, page, pagesize, changedate, use_nocount, filter, skip)
    # Get the JSON from the page, transform the text to JSON, get the 3rd element of the JSON result, and turn it into a dataframe.
    df_request <- data.frame(jsonlite::fromJSON(httr::content(httr::GET(url = URL), "text", encoding = "UTF-8", SimplifyVector = TRUE, SimplifyDataFrame = TRUE))[[3]]) # Skip to the results, bypassing the result headers..
    # Append the retrieved dataframe to the dataframe that should be returned.
    df <- dplyr::bind_rows(df, df_request)
    if (nrow(df_request) < pagesize)
    {
      break # No more data. Break out the loop.
    }
    # Next page.
    page <- page + 1
  }

  #

  return(df)
}

#' @title  build_dd_eco_api_url
#' @description
#' Build a DD-ECO-API url based on paging and filtering. This generic function queries the AquaDesk DD-ECO-API. For more information about the api: https://github.com/DigitaleDeltaOrg/dd-eco-api-specs
#'
#' Version 1.0
#' @param api_url URL to the endpoint.
#' @param organisation Code of the organisation. Defaults to empty.
#' @param page Page number to request. Defaults to 1.
#' @param pagesize Maximum number of items to return per page.
#' @param changedate When set, only retrieves changes sinds the specified date. The date formats are: yyyy-MM-dd, yyyy-MM-dd HH:mm:ss
#' @param use_nocount When set to TRUE, the request will take advantage of the new nocount parameter in AquaDesk. This only works in the new test-version of the DD-ECO-API (https://ddeco-test.aquadesk.nl). Defaults to FALSE.
#' @param filter Additional filter parts. Defaults to empty.
#' @param skip Comma separated list of field names NOT to return. See the specific filters endpoint for details. Example: changedate.  This only works in the new test-version of the DD-ECO-API (https://ddeco-test.aquadesk.nl). Defaults to empty.
#' @return Constructed url
#'
#' @import stringi
#'
#' @export
build_dd_eco_api_url <- function(api_url, organisation = "", page = 1, pagesize = 10000, changedate = "", use_nocount = FALSE, filter = "", skip = "") {
  # Initialize the non-filtering parameters.
  page_filter <- stringr::str_c("?page=", page)
  pagesize_filter <- stringr::str_c("&pagesize=", pagesize)
  skip_filter <- ifelse(skip == "", "", stringr::str_c("&skip=", skip))
  nocount_filter <- ifelse(use_nocount == FALSE, "", "&nocount=true")

  # Assemble the filter to a parameter.
  filter_filter <- ""
  filter_parts <- list()
  if (filter != "") {
    filter_parts <- append(filter_parts, stringr::str_split(filter, ";"))
  }

  if (organisation != "") {
    filter_parts <- append(filter_parts, stringr::str_c("organisation:eq:'", organisation, "'"))
  }

  if (changedate != "") {
    filter_parts <- append(filter_parts, stringr::str_c("changedate:ge:'", changedate, "'"))
  }

  if (length(filter_parts) > 0) {
    filter_filter <- stringr::str_c("&filter=", stringi::stri_paste(filter_parts, ";", collapse = ''))
  }

  # Combine the parameters into an url.
  url <- URLencode(stringr::str_c(api_url, page_filter, pagesize_filter, nocount_filter, skip_filter, filter_filter))
  return(url)
}
