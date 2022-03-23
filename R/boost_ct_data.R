
#
# search_col_name_str =  "NCTId"
#
# request_fields = c(
#   "NCTId",
#   # get_ct_fields(starts_with_str = "cond")
#   "Condition"
# )
#
# api_terms_max = 150
# df = df

# df = df %>%
#   boost_ct_data(
#     search_col_name_str = "NCTId",
#     request_fields = c(
#       "NCTId",
#       # get_ct_fields(starts_with_str = "cond")
#       "Condition"
#       ),
#     api_terms_max = 150
#     )


# df = df %>%  select("NCTId")
# search_col_name_str = "NCTId"
# request_fields = "Keyword"

###############################################

#' Enhance a dataframe by searching the clinicaltrials API for values in a specified column and requesting additional fields of results from the API to add as new columns to your data.frame
#'
#' @param df starting dataframe
#' @param search_col_name_str character string naming the column with values to search for
#' @param request_fields vector of field names to request from the API
#' @param api_terms_max default 150, but reduce this is the fields are data-heavy and the API throws an error
#'
#' @return
#' @export
#'
boost_ct_data = function(
  df,
  search_col_name_str,
  request_fields ,
  api_terms_max = 150
) {

  suppressWarnings( if(all(c(request_fields) == c(get_ct_fields()))){
    request_fields = get_ct_fields()[get_ct_fields()!= search_col_name_str]
  }
  )

  ## Inner function
  round_up = function (x, digits=0) { posneg = sign(x);  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10 ; z = floor(z * posneg + 0.5) / 10 ^ digits ; return(z) }

  orig_names = names(df)

  df = df %>%
    rename ("search_col" = all_of(search_col_name_str))

  all_possible_search_terms = df %>%
    select(search_col) %>%
    distinct %>%
    na.omit() %>%
    .[["search_col"]]

  calls_req = round_up( length(all_possible_search_terms)/api_terms_max )

  message("Getting data... 1 of ", calls_req + 1 )

  ## Starters:
  the_search_terms = all_possible_search_terms %>% .[1: min(api_terms_max, length(.)) ] %>% paste0(collapse = "+OR+")

  results = get_ct_data(
    search_expr = the_search_terms,
    request_fields = c(search_col_name_str, request_fields) ,
    remove_null_results = F
  )

  for(i in 1:calls_req ) {

    the_search_terms = all_possible_search_terms [(api_terms_max * i ): (min(api_terms_max * i + api_terms_max, length(all_possible_search_terms))) ] %>% paste0(collapse = "+OR+") %>% gsub("&", " ",.) %>% gsub("\\(|\\)", " ", .)

    message("Getting data... ",i +1 ," of ", calls_req + 1 )

    more_results = get_ct_data(
      search_expr = the_search_terms,
      request_fields = c(search_col_name_str, request_fields),
      remove_null_results = F
    )

    if(length(names(more_results))){
      results = bind_rows(results,more_results)
    }

  }

  results = results %>%
    na_if(y = "list()") %>%
    na_if(y = "character(0)") %>%
    setNames(c(search_col_name_str, request_fields))



  df = df %>%
    setNames( all_of(orig_names))
  df = df %>%
    left_join(results, by = search_col_name_str) %>%

    return(df)
}
