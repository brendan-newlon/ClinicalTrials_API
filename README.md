# R_ClinicalTrials_API
R functions for getting and cleaning data from the clinicaltrials.gov API

```
devtools::install_github("brendan-newlon/R_ClinicalTrials_API", force = T)
library(ClinicalTrialsAPI)
```

Use ```get_ct_fields()``` to return a character vector of all available fields from the clinicaltrials.gov API

If you wanted to get a data.frame of all of the study IDs and Condition-related data about myalgic encephalomyelitis, for example:  

```x = get_ct_data("myalgic encephalomyelitis", request_fields = c("NCTId", get_ct_fields(starts_with_str = "Condition")))```

Then, if you wanted to enhance the data you got by adding the Intervention-related fields to it:

```y = x %>% boost_ct_data(search_col_name_str = "NCTId", request_fields = get_ct_fields(starts_with_str = "Intervention"))```
  
Some fields will return values that are c() vectors rather than tidy single values per cell. You can manually unnest that data to a longer form (see documentation for ```split_longer_vector()```) but some columns need to be unnested in sync. To simplify this for certain Condition and Intervention -related fields, you can do this:

```interventions = y %>%
 dplyr::select(c("NCTId", starts_with("Intervention"))) %>%
 expand_ct_feature_cols("Intervention")```
