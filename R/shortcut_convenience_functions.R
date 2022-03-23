`%notin%` = Negate(`%in%`)

#' as.df shortcut function
#'
#' @param x object to convert to data.frame without factors
#'
#' @return df
#' @export
as.df = function(x){df = as.data.frame(x,stringsAsFactors=F); return(df)}

#' Remove columns from a data.frame that have only NA values
#'
#' @param df input data.frame
#'
#' @return
#' @import magrittr
#' @export
#'
no_na_cols = function(df){  
  df%>%.[,colSums(is.na(.))<nrow(.)]}

#' Remove rows from a data.frame that have only NA values
#'
#' @param df input data.frame
#'
#' @return
#' @import magrittr
#' @export
#'
no_na_rows = function(df){
  df%>%.[rowSums(is.na(.))!= ncol(.),]}

#' Generate a datetime stamp
#'
#' @return
#' @export
#'
# datetimestamp = function(){x = gsub("-|:| ","",now()); return(x)} # timestamp

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#' round up to a number of digits
#'
#' @param x input value
#' @param digits number of digits for rounding precision
#'
#' @return
#' @export
#'
round_up <- function (x, digits=0)
{
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  return(z)
}
# round_up(0.5)
# round_up(17.6)



#' take a variable input as text and parse/eval it
#'
#' @param x input value
#'
#' @return
#' @export
#'
eval_as_text = function(x){eval(parse(text=x))}


#' convert vector-like text into a real vector
#'
#' @param x input value
#'
#' @return
#' @import magrittr
#' @export
#'
unvector = function(x) {
  truth = try({
    parse(text = x) %>% eval %>% length() > 1
  }, silent = T)
  if (is.logical(truth)) {
    x = parse(text = x) %>% eval
  }
  return(x)
}


#' paste a prefix before value or values
#'
#' @param x input value
#' @param prefix character string to add as prefix
#'
#' @return
#' @import magrittr
#' @export
#'
prefix = function(x, prefix) {
  x = if(!is.na(x) && x != ""){
    paste0(prefix,x)
  } else {x = x}
  return(x)
}



#------------- split_longer()
#' split and unnest_longer a column in a df by some regex value
#'
#' @param df input data.frame
#' @param col_to_split the column to split
#' @param by_regex regex used as the splitter
#' @param encoding character encoding
#'
#' @return
#' @import magrittr
#' @export
#'
split_longer = function(df,col_to_split,by_regex, encoding = "UTF-8") {
  df[[col_to_split]] = df[[col_to_split]] %>% lapply(., function(x) { stringr::str_split(x, by_regex) })
  while(class(df[[col_to_split]]) == "list"){
    df = df %>% tidyr::unnest_longer(col_to_split)}
  return(df)
}


#' split and unnest_longer the values in a column that are vectors posing as text
#'
#' @param df input dataframe
#' @param col_name_chr name of the column to split
#'
#' @return
#' @export
#'
split_longer_vector = function( df, col_name_chr ){
  for(the_col in 1:length(col_name_chr)){
    df[[tidyselect::all_of(col_name_chr)[[the_col]]]] = purrr::map(df[[tidyselect::all_of(col_name_chr)[[the_col]]]], function(i){
      if(isTRUE(stringr::str_detect(i, "^c\\("))){  # This could be adapted to handle "list\\(" too -----------------------
        i = eval_as_text(i) %>% paste0(collapse = ":::sep:::") # treats it as real vector, then pastes it to a string with the sep
      } else {i = i }
    })
  }

  df = df %>%
    tidyr::separate_rows(tidyselect::all_of(col_name_chr), sep = ":::sep:::")  ## This keeps the set of columns coordinated for the split-longer so there's no hash of their values
  return(df)
}



#' split and unnest_longer all the columns of a df that contain vectors posing as text values
#'
#' @param df input data.frame
#'
#' @return
#' @export
#'
split_longer_any_vector_cols = function(df){
  for (any_col in seq_along(df)){
    this_col = names(df[any_col])
    if(
      stringr::str_detect(df[[this_col]], "^c\\(") %>% any %>% isTRUE()
    ){
      df = df %>% split_longer_vector(this_col)
    }
  }
  return(df)
}


