# ClinicalTrials_API
R functions for getting and cleaning data from the clinicaltrials.gov API

```
install.packages("devtools")
devtools::install_github("brendan-newlon/ClinicalTrials_API")
library(ClinicalTrialsAPI)
```

Use ```get_ct_fields()``` to return a character vector of all available fields from the clinicaltrials.gov API. 
To be more selective, you can search for all the fields starting with "Condition" for example, like this: ```get_ct_fields(starts_with_str = "Condition")``` or all the fields that contain "Arm" somewhere in the name, like this: ```get_ct_fields(contains_str = "Arm")```

If you wanted to get a data.frame of all of the study IDs and Condition-related data about myalgic encephalomyelitis, for example:  

```x = get_ct_data("myalgic encephalomyelitis", request_fields = c("NCTId", get_ct_fields(starts_with_str = "Condition")))```

Then, if you wanted to enhance the data about the studies you found by adding Intervention-related fields to it:

```y = x %>% boost_ct_data(search_col_name_str = "NCTId", request_fields = get_ct_fields(starts_with_str = "Intervention"))```
  
Some fields will return values that are c() vectors rather than tidy single values per cell. You can manually unnest that data to a longer form (see documentation for ```split_longer_vector()```) but some columns need to be unnested in sync. To simplify this for certain Condition and Intervention -related fields, you can do this:

```interventions = y %>%
 dplyr::select(c("NCTId", starts_with("Intervention"))) %>%
 expand_ct_feature_cols("Intervention")```
