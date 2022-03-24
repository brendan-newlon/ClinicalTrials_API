getwd()

usethis::use_package("magrittr")
usethis::use_package("purrr")
usethis::use_package("tibble")
usethis::use_package("httr")
usethis::use_package("jsonlite")
usethis::use_package("XML")
usethis::use_package("stringr")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_pipe(export = TRUE)

devtools::document()

devtools::install_github("brendan-newlon/ClinicalTrials_API", force = T)

library(ClinicalTrialsAPI)

get_ct_fields()

x = ct_search("myalgic encephalomyelitis", request_fields = c("NCTId", get_ct_fields(starts_with_str = "Condition")))

x = get_ct_data("myalgic encephalomyelitis", request_fields = c("NCTId", get_ct_fields(starts_with_str = "Condition")))

x = get_ct_data("myalgic encephalomyelitis", request_fields = c("NCTId", "Condition")) %>% split_longer_vector("Condition")

y = x %>%
  boost_ct_data(search_col_name_str = "NCTId", request_fields = get_ct_fields(starts_with_str = "Intervention"))
#
# z_condition = y %>%
#   dplyr::select(c("NCTId", tidyselect::starts_with("Condition"))) %>%
#   expand_ct_feature_cols("Condition")
#
# z_intervention = y %>%
#    dplyr::select(c("NCTId", tidyselect::starts_with("Intervention"))) %>%
#    expand_ct_feature_cols("Intervention")
