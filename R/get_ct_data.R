
## API DATA

# https://clinicaltrials.gov/api/gui/ref/api_urls -- api overview
# https://clinicaltrials.gov/api/gui/ref/expr#contextOp  -- operators
# https://clinicaltrials.gov/api/gui/ref/syntax#searchExpr -- seach expr syntax
# https://clinicaltrials.gov/api/info/search_areas?fmt=XML  -- study areas

  # heart attack AND SEARCH[Location](AREA[LocationCountry]United States AND AREA[LocationStatus]Recruiting)
    # XYZ007 AND SEARCH[Study](AREA[IDSearch]XYZ007)
#
#     search_expr = "SP756 AND SEARCH[Study](AREA[IDSearch]XYZ007)"
#     ids = ct_search(search_expr = search_expr)
#

# https://clinicaltrials.gov/api/gui/ref/crosswalks  -- awesome reference for the fields
# https://prsinfo.clinicaltrials.gov/definitions.html  -- even better definitions for what each field/term means




#' search for a term in clinicaltrials data
#'
#' @param search_expr the expression to search for
#' @param request_fields which fields to request from the API
#' @param count developer's note: can't remember why this is here... ignore it
#'
#' @return results as a data.frame
#' @import magrittr
#' @export
#'
ct_search = function (
  search_expr = "fatigue",
  # AND = NULL,
  # OR = NULL, # a c() of optional terms
  request_fields = "NCTId,Condition,BriefTitle,OrgStudyId,EligibilityCriteria",
  count = NULL
){
  query_url <- "https://clinicaltrials.gov/api/query/study_fields?expr="

  search_expr = utils::URLencode(search_expr %>% gsub("&", "", .))
  request_fields = paste(request_fields, collapse = ",")
  query = paste0(query_url, search_expr, "&fields=", request_fields)
  if (is.null(count))  { count <- 1e+06}
  if (!is.integer(as.integer(count)))         {stop("Count must be a number")}

  query = paste0(query,"&min_rnk=1&max_rnk=1000")

    query = paste0(query, "&fmt=JSON")
    search_result = httr::GET(
      query
      )
    if (search_result$status != 200)         { 
      stop(httr::http_status(search_result)$message)
      }
    x = search_result$content %>% 
      rawToChar() %>% 
      jsonlite::fromJSON()
    x = x$StudyFieldsResponse$StudyFields
    x = x[1:min(nrow(x),count),]
    return(if(is.null(x)){NA}else{x})
}

# x = ct_search("XYZ0007")
# x = ct_search("fatigue")
#
# x = ct_search_expr_search("XYZ0007", get_fields = "OrgStudyId,OrgFullName,NCTId,BriefTitle,BriefSummary,Condition,ConditionAncestorId,ConditionAncestorTerm,InterventionName,InterventionDescription,Keyword,LeadSponsorName,Gender,GenderBased,EligibilityCriteria") ; View(x)


#' request result fields from clinicaltrials API
#'
#' @param search_expr term to search for
#' @param request_fields names of the fields to request from the API
#' @param expr_in_field field where the search expression must be found
#' @param count developer note: can't remember why this param is here
#' @param remove_null_results should fields with nothing but null values be removed automatically from the result df?
#'
#' @return results as a data.frame
#' @import magrittr
#' @export
#'
get_ct_data = function(
  search_expr ,
  request_fields = get_ct_fields() ,
  expr_in_field = NULL ,  ## --------- this is currently a post-query filter by colname, should migrate to use the API adv search capabilities eg. # SEARCH[Study](AREA[IdSearch]XYZ0007+OR+UP0100)
  count = NULL,
  remove_null_results = F ## ------ if TRUE, this seems to be broken. Eliminates columns with some empty results, even if some results are there
  ){
  ## api limits to 20 fields requested per query. How many api calls are required to get all fields?
  calls = ((length(request_fields)-min(length(request_fields),20))/19 ) %>% 
    round_up()

  fields = request_fields # a c() of field names as per https://clinicaltrials.gov/api/info/study_fields_list
  if(class(fields) == "data.frame"){fields = names(fields)}

  ##### query it
  x = ct_search(search_expr, request_fields = paste(fields[1:min(length(fields),20)], collapse = ","), count = NULL)
  if(!all(is.na(x))){
    x = x  %>%
    dplyr::arrange(Rank) %>%  
      dplyr::select(-Rank)


# loop through fields requested and join the results of API calls as needed
    if(calls > 0){
      for(i in 1:calls){
        if(length(fields) > 20){     fields = fields[21:length(fields)] }
        remain = length(fields)
        message(paste0("Getting data... API call ",i+1," of ",calls + 1))
        y =x %>% 
          dplyr::bind_cols(
          ct_search(search_expr, request_fields = paste(fields[1:min(20,remain)], collapse = ",") , count = NULL )  %>%
            dplyr::arrange(Rank) %>%  
            dplyr::select(-Rank)
        )
        x = y
      }
    }

    if(remove_null_results){
      ## Remove null values and fields with no data like list()
      nulls = lapply(x,function(i){is_empty(i[[1]])}) %>% as.character()
      x[nulls == "TRUE"] = NULL
    }

  # filter for results where the expression is found in the specified field
  if(!is.null(expr_in_field)){
    x = x %>%
      dplyr::filter(str_detect(.[[expr_in_field]], search_expr))
  }

  x = x %>%
    lapply(as.character) %>% as.df

  x = x[1:min(nrow(x),count),] # count as max results to return, done here after filtering for expression in field

  if(!is.data.frame(x)){
    x = x %>% as.df %>% 
      stats::setNames(request_fields)
  }

  return(x)
  } else {return (NA)}
}

# all_fields = get_ct_fields() # c()

# cond_fields = get_ct_fields(starts_with_str = "cond")

# x = get_ct_data(search_expr = "fatigue",request_fields = c("OrgStudyId","NCTId",cond_fields) , count = 5 )

# x = get_ct_data(search_expr = "XYZ0007",request_fields = c("OrgStudyId","NCTId",cond_fields) , expr_in_field = "OrgStudyId" , remove_null_results = T) # 1 result

# x = get_ct_data(search_expr = "XYZ0007",request_fields = c("OrgStudyId","NCTId",cond_fields) , expr_in_field = "NCTId" , remove_null_results = T) # no results

# x = get_ct_data(search_expr = "fatigue",request_fields = get_ct_fields() , count = 5 )

#
# x = get_ct_data(search_expr = "heart attack AND SEARCH[Location](AREA[LocationCity]Portland AND AREA[LocationState]Maine)",request_fields = c("OrgStudyId","NCTId", "LocationState", "LocationCity",cond_fields) , remove_null_results = T)
#
# x = get_ct_data(search_expr = "heart attack AND SEARCH[Location](AREA[LocationCity]Portland AND AREA[LocationState]Maine)",request_fields = c("OrgStudyId","NCTId", "LocationState", "LocationCity",cond_fields) , remove_null_results = T)
#
#
# x = get_ct_data(search_expr = "XYZ007 AND SEARCH[Study](AREA[IdSearch]XYZ0007)",request_fields = c("OrgStudyId","NCTId", "LocationState", "LocationCity",cond_fields) , remove_null_results = T)
#
# x = get_ct_data(search_expr = "SEARCH[Study](AREA[IdSearch]XYZ0007)",request_fields = c("OrgStudyId","NCTId", "LocationState", "LocationCity",cond_fields) , remove_null_results = T)


# heart attack AND SEARCH[Location](AREA[LocationCity]Portland AND AREA[LocationState]Maine)
#######################################################



#
# ct_all_fields_by_OrgStudyId = function(OrgStudyId = NULL, count = 5, all_fields, as_JSON = T ){
#   calls = ((length(all_fields)-20)/19 )%>% round_up()
#
#   fields = all_fields # a c() of field names as per https://clinicaltrials.gov/api/info/study_fields_list
#   message(paste0("Getting data..."))
#   x = ct_OrgStudyId_search(OrgStudyId, get_fields = paste(fields[1:20], collapse = ",") ) %>% arrange(Rank) %>%  select(-Rank)
#   fields = fields[21:length(fields)]
#   for(i in 1:calls){
#     remain = length(fields)
#     message(paste0("Getting data... API call ",i," of ",calls))
#     y =x %>% bind_cols(
#       ct_OrgStudyId_search(OrgStudyId, get_fields = paste(fields[1:min(20,remain)], collapse = ","))  %>% arrange(Rank) %>%  select(-Rank)
#     )
#     fields = fields[21:length(fields)]
#     x = y
#   }
#
#   nulls = lapply(x,function(i){is_empty(i[[1]])}) %>% as.character()
#   x[nulls == "TRUE"] = NULL
#   x
#
# }
#
# y = ct_all_fields_by_OrgStudyId(OrgStudyId = "XYZ0007", count = 5, all_fields = all_fields)
#
# y %>% t

######################################################
#
#
# ## Not a great approach. better to use data from the other functions since those have cleaner data, eg. condition ancestor taxonomy...
#
# ct_find_related_terms = function(
#   search_expr = "myalgic encephalomyelitis",
#   search_field = "Condition"
# ){
#   query_url <- "https://clinicaltrials.gov/api/query/field_values?expr="
#   search_expr = search_expr %>% URLencode()
#   search_field = paste0("&field=",search_field)
#   query <- paste0(query_url, search_expr,search_field, "&fmt=JSON")
#
#   x = httr::GET(query )
#   x = x$content %>% rawToChar() %>% fromJSON
#   x = x$FieldValuesResponse
#   x = x$FieldValues
#   x = x %>%
#     rename("RelatedTerm" = "FieldValue") %>%
#
#     # mutate(
#
#        # rank = dense_rank(paste(x$))
#
#       # rank = dense_rank(paste(x$NStudiesWithValue, x$NStudiesFoundWithValue))
#
# #       df = x %>% select(NStudiesWithValue,NStudiesFoundWithValue, RelatedTerm)
# # rank =      dense_rank(ave(order(do.call(order, df[,2:1])), df[,2:1]))
# # rank2  =  dense_rank(ave(order(do.call(order, df[,1:2])), df[,1:2]))
# # df$rank = rank
# # df$rank2 = rank2
# #     # ) %>%
# #
# #
# # df = df %>%
#     mutate(
#       ExprRelevanceToRelatedTerm = NStudiesFoundWithValue/NStudiesWithValue * 100 ,
#       RelatedTermRelToExpr = NStudiesWithValue/NStudiesFoundWithValue * 100 ,
#       gap = NStudiesWithValue - NStudiesFoundWithValue
#       )  %>%
#     arrange(NStudiesFoundWithValue == 1,
#       ExprRelevanceToRelatedTerm %>% desc(),
#             NStudiesFoundWithValue %>% desc()
#             # NStudiesWithValue %>% desc(),
#             )
#   x
#   }
#
#
#
#
# heart = x
# x = ct_find_related_terms(search_expr = "heart attack", search_field = "Condition")
#
# x = ct_find_related_terms(search_expr = "Active Smoker", search_field = "Condition")
#
# x = ct_find_related_terms(search_expr = "CFS", search_field = "Condition")
#
#
# x = ct_find_related_terms(search_expr = "alcohol use OR alcohol abuse", search_field = "Condition")

############################################################

## Term relevance search:

# https://clinicaltrials.gov/api/query/field_values?expr=heart+attack&field=Condition



# x = get_ct_data(search_expr = c("Ischemia", "Pathologic Processes", "Necrosis", "Myocardial Ischemia", "Heart Diseases", "Cardiovascular Diseases", "Vascular Diseases")
# ,request_fields = c("OrgStudyId","NCTId",cond_fields)  ) %>% distinct



