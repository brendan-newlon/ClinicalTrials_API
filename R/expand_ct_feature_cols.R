## testing

# df = x3




#' Unnest_longer the vector values for sets of columns related to condition and intervention that should remain correlated
#'
#' @param df the input data.frame
#' @param features either "Intervention" or "Condition". It may be necessary to cut long dfs to fewer rows if the amount of data would exceed ram limits
#'
#' @return a longer dataframe 
#' @export
#' 
#' @example 
#' df = df %>% 
#    select(c("NCTId", starts_with("Intervention"))) %>% 
#    expand_ct_feature_cols("Intervention")
#'
expand_ct_feature_cols = function(
  df,
  features = c("Intervention", "Condition")
){
  
  
  col_sets = c("Mesh", "Ancestor", "BrowseBranch", "BrowseLeaf")
  col_filters = c("Name", "Term", "Id", "Relevance", "Abbrev")
  
  orig_df = df
  
  df_feat = orig_df %>% 
    dplyr::select(-tidyselect::starts_with("Condition"))
  
  if(isTRUE(
    features == "Intervention"
  ))    {
    
    if(ncol(df_feat)>1){
      for (col_set in seq_along(col_sets)) {
        
        these_cols = names(df_feat[
          stringr::str_detect(
            names(df_feat),
            paste0(
              paste0("Intervention", col_sets[col_set]) ,
              col_filters, collapse = "$|"
            )
          )
        ])
        
        df_feat = df_feat %>%
          split_longer_vector(these_cols)
      }
    }
    df = df_feat %>% dplyr::distinct()
  }
  
  
  if(isTRUE(
    features == "Condition"
  )){
    df_feat = orig_df %>% 
      dplyr::select(-tidyselect::starts_with("Intervention"))
    if(ncol(df_feat)>1){
      for (col_set in seq_along(col_sets)) {
        
        these_cols = names(df_feat[
          stringr::str_detect(
            names(df_feat),
            paste0(
              paste0("Condition", col_sets[col_set]) ,
              col_filters, collapse = "$|"
            )
          )
        ])
        
        df_feat = df_feat %>%
          split_longer_vector(these_cols)
      }
    }
    df = df_feat %>% dplyr::distinct()
  }
  return(df)
  
}
