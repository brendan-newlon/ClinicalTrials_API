`%notin%` = Negate(`%in%`)

#' as.df shortcut function
#'
#' @param x object to convert to data.frame without factors
#'
#' @return df
#' @examples x = as.df(c(1,2,3))
#' @export
as.df = function(x){df = as.data.frame(x,stringsAsFactors=F); return(df)}

#' Remove columns from a data.frame that have only NA values
#'
#' @param df input data.frame
#'
#' @return
#' @examples df = data.frame(first = c(1,2,3,NA), second = c("a","b","c",NA), third = c(NA,NA,NA,NA)) %>% no_na_cols()
#' @export
#'
no_na_cols = function(df){
  df %>%.[,colSums(is.na(.)) < nrow(.)]}

#' Remove rows from a data.frame that have only NA values
#'
#' @param df input data.frame
#'
#' @return
#' @examples df = data.frame(a = c(1,2,3,NA), b = c("a","b","c",NA)) %>% no_na_rows()
#' @export
#'
no_na_rows = function(df){
  df%>%.[rowSums(is.na(.))!= ncol(.),]}


#' get the last n characters from the end of a string
#'
#' @param x
#' @param n
#'
#' @return a character substring
#' @export
#'
#' @examples substrRight("malfunction", 8)
#'
substrRight = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}



#' round up to a number of digits
#'
#' @param x input value
#' @param digits number of digits for rounding precision
#'
#' @return
#' @export
#' @examples round_up(3.14159, 2)
#'
round_up = function (x, digits=0)
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
#' @examples eval_as_text("c(\"x\", \"y\")")
#'
eval_as_text = function(x){eval(parse(text=x))}


#' convert vector-like text into a real vector
#'
#' @param x input value
#'
#' @return
#' @import magrittr
#' @export
#' @examples unvector("c(\"x\", \"y\")")
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
#' @examples prefix(c("data", "breakfast"), prefix = "my_")
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
#' @examples df = data.frame(a = c("1, 2", "3, 4")) %>% split_longer(col_to_split = "a", by_regex = ", ")
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
#' @examples df = get_ct_data("myalgic encephalomyelitis", request_fields = c("NCTId", "Condition")) %>% split_longer_vector("Condition")
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
#' @examples df = get_ct_data("myalgic encephalomyelitis", request_fields = c("NCTId", "Condition")) %>% split_longer_any_vector_cols()
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


