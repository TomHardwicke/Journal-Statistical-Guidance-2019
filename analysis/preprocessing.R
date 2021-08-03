# pre-processing code goes here...
# load data from 'primary' folder
# do any necessary munging
# save processed data (in csv format and R format)
library(tidyverse)
library(assertr)

# Load data
d_coding <- read_csv(here::here("data", "primary", "data_coding.csv"))
d_journals <- read_csv(here::here("data", "primary", "data_journals.csv"))

d_coding <-
  d_coding %>% 
  
  # Rename variables
  rename(
    coder = "Choose coder initials",
    journal = "Enter journal name:",
    has_guidance = "Does the journal provide any guidance related to statistics or data analysis? (including referring to external guidelines)",
    has_external_guidance = "Are any external statistical guidelines referred to? (this includes external reporting guidelines, such as CONSORT; publisher guidelines shared by many journals, such as Nature life sciences reporting checklist; and references to specific papers covering statistical issues)",
    external_guidance = "If \"Yes\", which external statistical guidelines are referred to? (NOTE that these external guidelines should not be included in your coding of mentioned topics for this journal)",
    has_internal_guidance = "Other than referring to external guidelines, does the journal have its own statistical guidelines?",
    has_p_value = "Is any guidance provided about p-values?",
    p_value = "If YES, please copy and paste all guidance related to p-values here:",
    has_significance = "Is any guidance provided about statistical significance?",
    significance = "If YES, please copy and paste all guidance related to statistical significance here:",
    has_null_hypo = "Is any guidance provided about null hypotheses?",
    null_hypo = "If YES, please copy and paste all guidance related to null hypotheses here:",
    has_sample_size = "Is any guidance provided about sample size justification (including statistical power)?",
    sample_size = "If YES, please copy and paste all guidance related to sample size justification (including statistical power) here:",
    has_conf_int = "Is any guidance provided about confidence intervals?",
    conf_int = "If YES, please copy and paste all guidance related to confidence intervals here:",
    has_effect_size = "Is any guidance provided about effect sizes?",
    effect_size = "If YES, please copy and paste all guidance related to effect sizes here:",
    has_multi_compare = "Is any guidance provided about multiple comparisons?",
    multi_compare = "If YES, please copy and paste all guidance related to multiple comparisons here:",
    has_subgroup = "Is any guidance provided about sub-group analyses?",
    subgroup = "If YES, please copy and paste all guidance related to sub-group analyses here:",
    has_baseline_covar = "Is any guidance provided about baseline covariates?",
    baseline_covar = "If YES, please copy and paste all guidance related to baseline covariates here:",
    has_non_param = "Is any guidance provided about non-parametric tests?",
    non_param = "If YES, please copy and paste all guidance related to non-parametric tests here:",
    has_sensitivity = "Is any guidance provided about sensitivity analysis?",
    sensitivity = "If YES, please copy and paste all guidance related to sensitivity analysis here:",
    has_model_assume = "Is any guidance provided about checking model assumptions?",
    model_assume = "If YES, please copy and paste all guidance related to checking model assumptions here:",
    has_exclusion = "Is any guidance provided about data exclusions?",
    exclusion = "If YES, please copy and paste all guidance related to data exclusions here:",
    has_outliers = "Is any guidance provided about handling outliers?",
    outliers = "If YES, please copy and paste all guidance related to handling outliers here:",
    has_missing = "Is any guidance provided about handling missing data?",
    missing = "If YES, please copy and paste all guidance related to handling missing data here:",
    has_one_sided = "Is any guidance provided about one-sided tests?",
    one_sided = "If YES, please copy and paste all guidance related to one-sided tests here:",
    has_bayes = "Is any guidance provided about Bayesian methods?",
    bayes = "If YES, please copy and paste all guidance related to Bayesian methods here:",
    has_secondary = "Is any guidance provided about secondary outcomes?",
    secondary = "If YES, please copy and paste all guidance related to secondary outcomes here:",
    has_prespecify = "Is any guidance provided about pre-specification of analyses?",
    prespecify = "If YES, please copy and paste all guidance related to pre-specification of analyses here:",
    has_cat_continuous = "Is any guidance provided about categorization of continuous variables?",
    cat_continuous = "If YES, please copy and paste all guidance related to categorization of continuous variables here:"
  ) %>% 
  
  # Remove raw coding and keep consensus only. Consensus rows include "/" between coder initials
  filter(str_detect(coder, "/")) %>% 
  
  # Check for expected coder combinations
  assert(in_set("DS/TH", "MM/TH", "MSH/TH", "TB/TH"), coder) %>% 
  
  # Check that there is one row per journal
  # verify(nrow(.) == nrow(d_journals)) %>% 
  
  # Check that journals are unique
  assert(is_uniq, journal) %>% 
  
  # Check that `has_internal_guidance` is an expected value
  assert(
    in_set(
      "Yes - there is a dedicated section specifically providing statistical guidance",
      "Yes - but there is no dedicated section specifically providing statistical guidance",
      "No",
      NA_character_), 
    has_internal_guidance
  ) %>% 
  
  # Internal guidance can be in dedicated section, so separate out variable
  mutate(
    has_internal_guidance_section = case_when(
      has_internal_guidance == "Yes - there is a dedicated section specifically providing statistical guidance" ~ "Yes",
      has_internal_guidance == "Yes - but there is no dedicated section specifically providing statistical guidance" ~ "No",
      TRUE ~ NA_character_
    ),
    has_internal_guidance = 
      if_else(str_detect(has_internal_guidance, "Yes"), "Yes", has_internal_guidance),
    .after = has_internal_guidance
  ) %>% 
    
  # Manual entry errors in "Lancet Respiratory Medicine"
  # For now, correct here but asking Tom to check in manual entry
  mutate(
    subgroup = if_else(journal == "Lancet Respiratory Medicine", has_subgroup, subgroup),
    has_subgroup = if_else(journal == "Lancet Respiratory Medicine", "YES", has_subgroup),
    prespecify = if_else(journal == "Lancet Respiratory Medicine", has_prespecify, prespecify),
    has_prespecify = if_else(journal == "Lancet Respiratory Medicine", "YES", has_prespecify)
  ) %>% 
  
  # Check that all booleans "Yes"/"No"/NA and convert to TRUE/FALSE/NA
  assert(
    in_set("Yes", "YES", "No", "NO", NA),
    starts_with("has_")
  ) %>% 
  mutate(across(starts_with("has_"),
                ~ case_when(. %in% c("Yes", "YES") ~ TRUE,
                            . %in% c("No", "NO") ~ FALSE,
                            TRUE ~ NA)
  ))

# Check that free-text provided for all, and only, expected booleans
vars_bool <-
  d_coding %>% 
  
  # No free-text for guidance and internal_guidance booleans
  select(starts_with("has_") & (!contains("guidance") | contains("external"))) %>% 
  colnames()

# Text should be provided if boolean TRUE
for (var_bool in vars_bool) {
  
  var_text <- str_remove(var_bool, "has_")
  
  pointblank::col_vals_not_null(
    d_coding, vars(!!var_text), ~. %>% filter(.data[[var_bool]])
  )
  
}

# Text should NOT be provided if boolean FALSE
# TODO: Manual data entry error for external_guidelines; asked Tom; currently skipping this check, so change once resolved
for (var_bool in vars_bool[2:length(vars_bool)]) {
  
  var_text <- str_remove(var_bool, "has_")
  
  pointblank::col_vals_null(
    d_coding, vars(!!var_text), ~. %>% filter(!.data[[var_bool]])
  )
}

# d_coding <-
#   d_coding %>%
# 
#   # Check that all journals with internal guidance have TRUE/FALSE (not NA) for booleans
#   # TODO: fix 2 manual entry errors in BRITISH JOURNAL OF PHARMACOLOGY; asked Tom; for now commented out
#   # filter(journal != "BRITISH JOURNAL OF PHARMACOLOGY")
#   pointblank::col_vals_not_null(
#     d_coding, vars(!!!vars_bool), ~. %>% filter(has_internal_guidance)
#   )

readr::write_csv(d_coding, file =  here::here("data", "processed", "d_coding.csv"))
save(d_coding, file =  here::here("data", "processed", "d_coding.rds"))