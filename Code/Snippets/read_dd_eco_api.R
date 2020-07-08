#' @title  read_dd_eco_api
#' @description
#' This generic function queries the AquaDesk DD-ECO-API. For more information about the api: https://github.com/DigitaleDeltaOrg/dd-eco-api-specs.
#' The function queries the API and returns the results in the form of a dataframe by repeatedly requesting pages until no more results are returned.
#' Step 1: GET --> The GET-function (httr-package) receives all information available from the url.
#' Step 2: content --> The content-function (httr-package) gives how the information should come back. In this case as text.
#' Step 3: fromJSON --> The fromJSON-function (jsonlite-package) converts the recovered information to JSON.
#' Step 4: data.frame --> The data.frame-function converts the JSON to a data.frame.
#' Various filters are available per table in the API. See as an example:
#' https://ddeco.aquadesk.nl/v1/measurementobjects/filters
#'
#' Version 1.0
#' @param api_url URL to the endpoint.
#' @param organisation - optional - Code of the organisation. Defaults to empty.
#' @param changedate - optional - When set, only retrieves changes sinds tthe specified date. The date formats are: yyyy-MM-dd, yyyy-MM-dd HH:mm:ss
#' @param use_nocount - optional - When set to TRUE, the request will take advantage of the new nocount parameter in AquaDesk. This only works in the new test-version of the DD-ECO-API (https://ddeco-test.aquadesk.nl)
#' @param filter - optional - Comma separated list of filter parts for the DD-ECO-API to limit the amount of records to return. For filter syntax, see https://ddeco-test.aquadesk.nl 
#' @param skip - optional - Comma separated list of fieldnames NOT to return. See the specific filters endpoint for details. Example: changedate.  This only works in the new test-version of the DD-ECO-API (https://ddeco-test.aquadesk.nl) 
#' @param pagesize Maximum number of items to return per page. Defaults to 10000.
#' @return dataframe with the query results.
#' 
#' @import tidyverse, httr, jsonlite
#' @importFrom stringr str_interp
#' 
#' @examples 
#' read_dd_eco_api("https://ddeco.aquadesk.nl/v1/monitoringnetworks", organisation = "WAM")
#' read_dd_eco_api("https://ddeco.aquadesk.nl/v1/parameters")
#' read_dd_eco_api("https://ddeco.aquadesk.nl/v1/monitoringnetworks", organisation = "WAM")
#' read_dd_eco_api("https://ddeco-test.aquadesk.nl/v1/parameters", use_nocount = TRUE, filter = "type:eq:'TAXON"', skip = "changedate")
#' @author Geri Wolters (EcoSys)
#' @export
read_dd_eco_api <- function(api_url, organisation = "", changedate = "", use_nocount = FALSE, filter = "", skip = "", pagesize = 10000) {
  page <- 1
  df <- bind_rows()
  
  repeat
  {
    URL <- build_dd_eco_api_url(api_url, organisation, page, pagesize, changedate, use_nocount, filter, skip)
    df_request <- data.frame(jsonlite::fromJSON(httr::content(httr::GET(url = URL), "text", encoding = "UTF-8"), flatten = TRUE)[3])
    df <- bind_rows(df, df_request)
    if (nrow(df_request) < pagesize)
    {
      break
    }
    page <- page + 1
  }
  return(df)
}

#' @title  build_dd_eco_api_url
#' @description
#' Worker function to build a DD-ECO-API url based on paging and filtering. This generic function queries the AquaDesk DD-ECO-API. For more information about the api: https://github.com/DigitaleDeltaOrg/dd-eco-api-specs
#' https://ddeco.aquadesk.nl/v1/measurementobjects/filters
#'
#' Version 1.0
#' @param base_url URL to the endpoint.
#' @param organisation Code of the organisation. Defaults to empty.
#' @param page Page number to request. Defaults to 1.
#' @param pagesize Maximum number of items to return per page.
#' @param changedate When set, only retrieves changes sinds the specified date. The date formats are: yyyy-MM-dd, yyyy-MM-dd HH:mm:ss
#' @param use_nocount When set to TRUE, the request will take advantage of the new nocount parameter in AquaDesk. This only works in the new test-version of the DD-ECO-API (https://ddeco-test.aquadesk.nl). Defaults to FALSE.
#' @param filter Additional filter parts. Defaults to empty.
#' @param skip Comma separated list of field names NOT to return. See the specific filters endpoint for details. Example: changedate.  This only works in the new test-version of the DD-ECO-API (https://ddeco-test.aquadesk.nl). Defaults to empty.
#' @return Constructed url
#' 
#' @importFrom stringr str_c
#' 
#' @export
build_dd_eco_api_url <- function(base_url, organisation = "", page = 1, pagesize = 10000, changedate = "", use_nocount = FALSE, filter = "", skip = "") {
  page_filter <- stringr::str_c("?page=", page)
  pagesize_filter <- stringr::str_c("&pagesize=", pagesize)
  skip_filter <- ifelse(skip == "", "", stringr::str_c("&skip=", skip))
  nocount_filter <- ifelse(use_nocount == FALSE, "", "&nocount=true")
  
  # Assemble the filter
  filter_filter <- ""
  filter_parts <- list()
  if (filter != "") {
    filter_parts <- append(filter_parts, str_split(filter, ";"))    
  }
  
  if (organisation != "") {
    filter_parts <- append(filter_parts, stringr::str_c("organisation:eq:'", organisation, "'"))
  }
  
  if (changedate != "") {
    filter_parts <- append(filter_parts, stringr::str_c("changedate:ge:'", changedate, "'"))
  }
  
  if (length(filter_parts) > 0) {
    filter_filter <- stringr::str_c("&filter=", stri_paste(filter_parts, ";", collapse = ''))
  }
  
  url <- str_replace_all(stringr::str_c(base_url, page_filter, pagesize_filter, nocount_filter, skip_filter, filter_filter), "'", "%27")
  return(url)
}

