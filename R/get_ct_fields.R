


#' request a vector of the names of fields available from the clinicaltrials API
#'
#' @param all developer's note: this may be a leftover from previous code - ignore it
#' @param starts_with_str get fields that start with this character substring
#' @param contains_str get fields that contain this character substring
#' @param case_sensitive is it case sensitive?
#'
#' @return a vector naming the fields available via the ClinicalTrials API, optionally filtered
#' @import magrittr
#' @export
#'
get_ct_fields = function(all = F, starts_with_str = NULL, contains_str = NULL, case_sensitive=F){
  search_result = httr::GET("https://clinicaltrials.gov/api/info/study_fields_list")
  if (search_result$status != 200) {stop(httr::http_status(search_result)$message)}
  parsed_result <- XML::xmlParse(httr::content(search_result, as = "text"))
  result_list <- XML::xmlToList(parsed_result)
  x = all_chr = result_list$FieldList %>% flatten_chr()

  if(all){
    x = x %>% as.df %>% .[["x"]]
    return(x)
  }

  x = x %>% as.df %>% t %>% as.df
  x = x %>% stats::setNames(x[1,]) %>% .[-1,] %>% tibble::remove_rownames()

  orig_names = tibble(orig = names(x), lower = tolower(names(x)))

  if(!case_sensitive){
    x = x %>% stats::setNames(tolower(names(x)))
    starts_with_str = tolower(starts_with_str)
    contains_str = tolower(contains_str)
  }

  if(length(starts_with_str) && nzchar(starts_with_str)){
    x = x %>% dplyr::select(tidyselect::starts_with(starts_with_str))
  }

  if(length(contains_str) && nzchar(contains_str)){
    x = x %>% dplyr::select(tidyselect::contains(contains_str))
  }

  new_names = orig_names %>% dplyr::filter(lower %in% names(x))

  x = x %>% stats::setNames(new_names$orig)

  return( names(x))
}

## If all = T, returns a c() of all field names available via the clinicaltrials API.
## If all = F, returns a df of fields available via the clinicaltrials api.  ** no longer true after code evolved **
##  As a df, the col names are the field names -- can be piped forward eg. to use select(starts_with("cond"))

# all_fields = get_ct_fields() # c()

# cond_fields = get_ct_fields(starts_with_str = "cond") # df


