# pre-processing code goes here...
# load data from 'primary' folder
# do any necessary munging
# save processed data (in csv format and R format)

# Load data
d_coding <- read_csv(here("data", "primary", "data_coding.csv"))
d_journals <- read_csv(here("data", "primary", "data_journals.csv"))

# Clean up column names
d_journals <-
  d_journals %>% 
  
  # Rename variables
  clean_names() %>% 
  rename(journal = "full_journal_title")

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


## the tests below use the pointblank package which is causing a problem when loaded - the appendix won't render (comes back as NA). So there's some kind of clash with knitr or papaja I can't figure out.
# therefore the text below is commented out, but the tests have been run independently of the full document and passed.

# library(pointblank) # for testing

# # Check that free-text provided for all, and only, expected booleans
# vars_bool <-
#   d_coding %>%
# 
#   # No free-text for guidance and internal_guidance booleans
#   select(starts_with("has_") & (!contains("guidance") | contains("external"))) %>%
#   colnames()
# 
# # Text should be provided if boolean TRUE
# for (var_bool in vars_bool) {
# 
#   var_text <- str_remove(var_bool, "has_")
# 
#   pointblank::col_vals_not_null(
#     d_coding, vars(!!var_text), ~. %>% filter(.data[[var_bool]])
#   )
# 
# }
# 
# # Text should NOT be provided if boolean FALSE
# for (var_bool in vars_bool) {
# 
#   var_text <- str_remove(var_bool, "has_")
# 
#   pointblank::col_vals_null(
#     d_coding, vars(!!var_text), ~. %>% filter(!.data[[var_bool]])
#   )
# }
# 
# # Check that all journals with internal guidance have TRUE/FALSE (not NA) for booleans
# d_coding <-
#   d_coding %>%
#   pointblank::col_vals_not_null(
#     vars(!!!vars_bool), ~. %>% filter(has_internal_guidance)
#   )

# SHARED PUBLISHER-LEVEL GUIDANCE -------------------------------------------------------

# remove shared guidance from d_coding and retain in separate dataframe
d_coding_shared <- d_coding %>% 
  filter(str_detect(journal, "SHARED")) %>%
  filter(journal %in% c("SHARED: Nature consolidated", "SHARED: Frontiers", "SHARED: STAR Methods", "SHARED: Scientific Data Consolidated")) # retain only the shared guidance we need (i.e., drop individual Nature guidelines which are covered by Nature consolidated)

d_coding <- d_coding %>% 
  filter(!str_detect(journal, "SHARED"))

# Identify journals that inherit publisher guidance 
# NB - there is one exception - the journal Scientific Data inherits the publisher guidance, but also has its own journal specific guidance
# to address this, we have created a special row for the journal Scientific Data 
natureJournals <- d_coding %>%
  filter(str_detect(external_guidance, regex("nature life", ignore_case = T))) %>%
  filter(journal != "Scientific Data") %>%
  pull(journal)

cellJournals <- d_coding %>%
  filter(str_detect(external_guidance, regex("\\bstar\\b", ignore_case = T))) %>%
  pull(journal)

frontiersJournals <- d_coding %>%
  filter(str_detect(external_guidance, regex("frontiers", ignore_case = T))) %>%
  pull(journal)

# For each publisher (nature, cell, frontiers) create a new dataframe from d_coding that identifies coders and journals
# create a new dataframe from d_coding shared that represents the publisher guidance
# bind these two dataframes

nature_df1 <- d_coding %>% filter(journal %in% natureJournals) %>%
  select(coder, journal)

nature_df2 <- d_coding_shared %>% filter(journal == "SHARED: Nature consolidated") %>%
  select(-coder, -journal)

nature_shared <- bind_cols(nature_df1, nature_df2) %>%
  mutate(publisher_guidance = "Nature") # add a marker to say these journals share Nature publisher guidance

cell_df1 <- d_coding %>% filter(journal %in% cellJournals) %>%
  select(coder, journal)

cell_df2 <- d_coding_shared %>% filter(journal == "SHARED: STAR Methods") %>%
  select(-coder, -journal)

cell_shared <- bind_cols(cell_df1, cell_df2) %>%
  mutate(publisher_guidance = "Cell") # add a marker to say these journals share Cell publisher guidance

frontiers_df1 <- d_coding %>% filter(journal %in% frontiersJournals) %>%
  select(coder, journal)

frontiers_df2 <- d_coding_shared %>% filter(journal == "SHARED: Frontiers") %>%
  select(-coder, -journal)

frontiers_shared <- bind_cols(frontiers_df1, frontiers_df2) %>%
  mutate(publisher_guidance = "Frontiers") # add a marker to say these journals share Frontiers publisher guidance

scientific_data_consolidated <- d_coding_shared %>% filter(journal == "SHARED: Scientific Data Consolidated") %>% # extract the special row for Scientific Data Consolidated
  mutate(journal = "Scientific Data")

allPublishers_shared <- bind_rows(nature_shared, cell_shared, frontiers_shared, scientific_data_consolidated) %>%
  mutate(has_publisher_guidance = T) # add a marker to say these journals have shared publisher guidance

# remove journals from d_coding and replace with the dataframes where they are attached to publisher guidance
d_coding <- d_coding %>% filter(journal %notin% allPublishers_shared$journal) %>%
  mutate(has_publisher_guidance = F, publisher_guidance = NA) # add a marker to say these journals do not have shared publisher guidance
d_coding <- bind_rows(d_coding,allPublishers_shared)

# Check that there is one row per journal in d_coding
d_coding <- d_coding %>% verify(nrow(.) == nrow(d_journals))
  


# EXTERNAL GUIDANCE -------------------------------------------------------

# Tidy external guidance

# For manual tidying, get all unique external guidance lines for visual inspection
unique_ext_guidance_groups <-
  d_coding  %>% 
  select(external_guidance) %>%
  filter(!is.na(external_guidance)) %>% 
  distinct()

# Manually edit some external guidance via lookup table
cleaned_external_guidances <- tribble(
  ~external_guidance, ~external_guidance_clean,
  "We encourage all authors to utilize the EQUATOR network to identify appropriate reporting guidelines based upon study type. Authors should refer to the BRISQ reporting guidelines for any study in which human biospecimens are used. For authors reporting animal research, we encourage use of the ARRIVE guidelines. For studies using cell lines, authors should report the source of the line, whether the line has been authenticated and how, and the mycoplasma contamination status.",
  "EQUATOR; BRISQ; ARRIVE",
  
  "CONSORT; STROBE; ARRIVE; The statistical guidelines advocated by the International Committee of Medical Journal Editors (Ann Intern Med 1988; 108: 266-73) should be followed.",
  "CONSORT; STROBE; ARRIVE; ICMJE",
  
  "\"Design and Analysis Transparency: Authors should follow field standards for disclosing key aspects of research design and data analysis, and should report the standards used in their study. See the Equator Network for information about standards across disciplines.\";  International Union of Crystallography (IUCr) Guidelines; Standards for Reporting Enzymology Data (STRENDA) Guidelines",
  "EQUATOR; IUCr; STRENDA",
  
  "EQUATOR, Controlled Trials: CONSORT\nCost-effectiveness Analyses: CHEERS\nDiagnostic Test Studies: STARD 2015,\nObservational Studies: STROBE\nMolecular Epidemiology: STROBE-ME\nQualitative Research: COREQ\nGenetic Risk Prediction Studies: GRIPS\nQuality Improvement Studies: SQUIRE\nMultivariable Prediction Model for Individual Prognosis Or Diagnosis: TRIPOD",
  "EQUATOR; CONSORT; STARD 2015; STROBE; STROBE-ME; COREQ; GRIPS; SQUIRE; TRIPOD",
  
  "EQUATOR, SAMPL, Randomized controlled trials (CONSORT) and protocols (SPIRIT) Systematic reviews and meta-analyses (PRISMA) and protocols (PRISMA-P) Observational studies (STROBE) Case reports (CARE) Qualitative research (COREQ) Diagnostic/prognostic studies (STARD and TRIPOD) Economic evaluations (CHEERS) Pre-clinical animal studies (ARRIVE)",
  "EQUATOR; SAMPL; CONSORT; SPIRIT; PRISMA; PRISMA-P; STROBE; CARE; COREQ; STARD; TRIPOD; CHEERS; ARRIVE",
  
  "EQUATOR, ARRIVE, REMARK, STARD, MOOSE, PRISMA, STROBE, STREGA, BRISQ, Tumor marker studies (Simon et al., 2009), Rodent model studies (Hollingshead, 2008), Microarray-based studies for clinical outcomes, Table 3 in Dupuy & Simon, 2007)",
  "EQUATOR, ARRIVE, REMARK, STARD, MOOSE, PRISMA, STROBE, STREGA, BRISQ, Tumor marker studies (Simon et al. 2009), Rodent model studies (Hollingshead 2008), Microarray-based studies for clinical outcomes (Table 3 in Dupuy & Simon 2007)",
  
  "Cummings & Rivara (2003; doi:10.1001/archpedi.157.4.321); Kempen (2011; doi:  https://doi.org/10.1016/j.ajo.2010.08.047); CONSORT; EQUATOR; PRISMA; TREND; MOOSE; STROBE; CHEERS; ISPOR; STREGA; SQUIRE; AAPOR; SRQR; COREQ",
  "Cummings & Rivara 2003 (10.1001/archpedi.157.4.321); Kempen 2011 (https://doi.org/10.1016/j.ajo.2010.08.047); CONSORT; EQUATOR; PRISMA; TREND; MOOSE; STROBE; CHEERS; ISPOR; STREGA; SQUIRE; AAPOR; SRQR; COREQ",
  
  "\"Reporting Statistical Information in Medical Journal Articles\" Cummings P, Rivara FP. Reporting statistical information in medical journal articles. Arch Pediatr Adolesc Med. 2003;157(4):321-324. doi:10.1001/archpedi.157.4.321; EQUATOR; PRISMA; CONSORT; STROBE; CHEERS; ISPOR; STREGA; STARD; TRIPOD; AAPOR; SPQR; COREQ",
  "Reporting Statistical Information in Medical Journal Articles (Cummings & Rivara 2003); EQUATOR; PRISMA; CONSORT; STROBE; CHEERS; ISPOR; STREGA; STARD; TRIPOD; AAPOR; SPQR; COREQ",
  
  "CONSORT; GNOSIS guidelines (published in the October 2005 issue of Neuro-Oncology [Vol. 7, Issue 4] [PDF])",
  "CONSORT; GNOSIS",
  
  "FDA Guidelines (https://perma.cc/W2EY-MSTA); For a review of some common errors associated with statistical analyses and reports, plus guidelines on how to avoid them, see the articles by Olsen (Infect Immun 71:6689–6692, 2003; Infect Immun 82:916–920, 2014). For a review of basic statistical considerations for virology experiments, see the article by Richardson and Overbaugh (J Virol 79:669 – 676, 2005).",
  "FDA Guidelines (https://perma.cc/W2EY-MSTA); Olsen 2003 (Infect Immun 71:6689–6692); Olsen 2014 (Infect Immun 82:916–920); Richardson & Overbaugh 2005 (J Virol 79:669 – 676)",
  
  "Altman DG., Gore SM, Gardner, MJ. Pocock SJ. (1983).  Statistical guidelines for contributors to medical journals. British Medical Journal 286, 1489-1493.",
  "Statistical guidelines for contributors to medical journals (Altman et al. 1983)",
  
  "https://www.ncbi.nlm.nih.gov/books/NBK153593/ http://www.biostathandbook.com/ http://www.utdallas.edu/~serfling/3332/Biology_statistics_made_simple_using_Excel.pdf",
  "fay & gerow 2014 (https://www.ncbi.nlm.nih.gov/books/NBK153593/); mcdonald 2014. Handbook of Biological Statistics (http://www.biostathandbook.com/)",
  
  "NIH Preclinical CONSORT PRISMA STARD",
  "NIH Preclinical; CONSORT; PRISMA; STARD",
  
  "ICMJE, CONSORT, Human Genome Epidemiology Network (HuGENet) guidelines MIAME ARRIVE",
  "ICMJE, CONSORT, Human Genome Epidemiology Network (HuGENet) guidelines, MIAME, ARRIVE",
  
  "For candidate gene research see Hewitt (2012; https://www.psychologicalscience.org/redesign/wp-content/uploads/2013/10/Candidate-Gene-Studies-Behavioral-Genetics-2012.pdf). For fMRI see Poldrack et al. (2008; https://doi.org/10.1016/j.neuroimage.2007.11.048)",
  "candidate gene research (Hewitt 2012) https://www.psychologicalscience.org/redesign/wp-content/uploads/2013/10/Candidate-Gene-Studies-Behavioral-Genetics-2012.pdf); fMRI (Poldrack et al. 2008) https://doi.org/10.1016/j.neuroimage.2007.11.048",
  
  "EQUATOR, Randomised trials: CONSORT, Observational studies: STROBE, Systematic reviews: PRISMA, Case reports: CARE, Qualitative research: SRQR, Diagnostic / prognostic studies: STARD, Quality improvement studies: SQUIRE, Economic evaluations: CHEERS, Study protocols: SPIRIT, Clinical practice guidelines: AGREE",
  "EQUATOR; CONSORT; STROBE; PRISMA; CARE; SRQR; STARD; SQUIRE; CHEERS; SPIRIT; AGREE",
  
  "Additional resources:\n\nStatistics for Biologists (Nature)\n\nUse of ANOVA versus T-test (The Plant Cell)\n\nHow Robust Are Your Data? (Nature)\n\nError Bars in Experimental Biology (J. Exp. Bot.)\n\nKnow When Your Numbers Are Significant (Nature)\n\nTen Simple Rules for Reproducible Computational Research",
  "Statistics for Biologists (Nature); Use of ANOVA versus T-test (The Plant Cell); How Robust Are Your Data? (Nature); Error Bars in Experimental Biology (J. Exp. Bot.); Know When Your Numbers Are Significant (Nature); Ten Simple Rules for Reproducible Computational Research",
  
  "EQUATOR\nRandomised trials: CONSORT\nObservational studies: STROBE\nSystematic reviews: PRISMA\nCase reports: CARE\nQualitative research: SRQR\nDiagnostic / prognostic studies: STARD\nQuality improvement studies: SQUIRE\nEconomic evaluations: CHEERS\nAnimal pre-clinical studies: ARRIVE\nStudy protocols: SPIRIT\nClinical practice guidelines: AGREE\nMIBBI",
  "EQUATOR; CONSORT; STROBE; PRISMA; CARE; SRQR; STARD; SQUIRE; CHEERS; ARRIVE; SPIRIT; AGREE; MIBBI",
  
  "ARRIVE, Uniform Requirements for manuscripts submitted to Biomedical journals, http://www.icmje.org",
  "ARRIVE, ICMJE",
  
  "ARRIVE,CONSORT, ICMJE guidelines (Recommendations for the Conduct, Reporting, Editing\nand Publication of Scholarly Work in Medical Journals)",
  "ARRIVE, CONSORT, ICMJE",
  
  "ICMJE; ARRIVE; EQUATOR; CONSORT; PRISMA; STROBE; STARD/TRIPOD; SAMPL",
  "ICMJE; ARRIVE; EQUATOR; CONSORT; PRISMA; STROBE; STARD; TRIPOD; SAMPL",
  
  "\"For a review of some common errors associated with statistical analyses and reports, plus guidelines on how to avoid them, see these 2003 and 2014 articles by Olsen. For a review of basic statistical considerations for virology experiments, see the article by Richardson and Overbaugh\" [papers preserved here: Link 1: https://perma.cc/37SH-3RQL ; Link 2: https://perma.cc/U8JV-VRG3 ; Link 3: https://perma.cc/H4TB-955H]",
  "Olsen 2003 (https://perma.cc/37SH-3RQL); Olsen 2014 (https://perma.cc/U8JV-VRG3); virology Richardson and Overbaugh 2005 (https://perma.cc/H4TB-955H)"
)

d_coding <-
  d_coding %>% 
  
  # Incorporate cleaned external guidance and rename original
  left_join(cleaned_external_guidances, by = "external_guidance") %>%
  rename(external_guidance_raw = external_guidance) %>% 
  mutate(
    external_guidance = coalesce(external_guidance_clean, external_guidance_raw),
    .after = external_guidance_raw
  ) %>% 
  relocate(external_guidance_clean, .after = external_guidance_raw) %>% 
  
  # Get one row per guidance, per journal
  separate_rows(external_guidance, sep = ",|;") %>% 
  
  # Tidy guidance
  mutate(
    external_guidance = str_trim(external_guidance),
    external_guidance = tolower(external_guidance)
  ) %>%
  
  # Manually tidy guidance
  mutate(external_guidance = case_when(
    
    # Guidelines
    str_detect(external_guidance, "star methods") ~ "cell star methods",
    str_detect(external_guidance, "stard") ~ "stard",
    str_detect(external_guidance, "cope") ~ "cope",
    str_detect(external_guidance, "equator") ~ "equator",
    str_detect(external_guidance, "gather") ~ "gather",
    str_detect(external_guidance, "icmje|icjme|uniform requirements for manuscripts") ~ "icmje",
    str_detect(external_guidance, "consort 2010") ~ "consort",
    str_detect(external_guidance, "jars") ~ "apa jars",
    str_detect(external_guidance, "ispor") ~ "ispor", # TODO: confirm ispor-smdm is same as ispor
    str_detect(external_guidance, "miame") ~ "miame",
    str_detect(external_guidance, "mibbi") ~ "mibbi",
    str_detect(external_guidance, "miqe") ~ "miqe",
    str_detect(external_guidance, "p?risma(?!-p)") ~ "prisma",
    str_detect(external_guidance, "st?rega") ~ "strega",
    str_detect(external_guidance, "strobe(?!-me)") ~ "strobe",
    str_detect(external_guidance, "arrive guidelines") ~ "arrive",
    str_detect(external_guidance, "https://www.nlm.nih.gov/services/research_report_guide.html") ~ "nlm research reporting guidelines and initiatives (https://www.nlm.nih.gov/services/research_report_guide.html)",
    str_detect(external_guidance, "nih preclinical|^nih$") ~ "nih principles and guidelines for reporting preclinical research",
    str_detect(external_guidance, "https://www.amstat.org/asa/your-career/ethical-guidelines-for-statistical-practice.aspx") ~ "American Statistical Association Ethical Guidelines for Statistical Practice (https://www.amstat.org/asa/your-career/ethical-guidelines-for-statistical-practice.aspx)",
    
    # Publications
    str_detect(external_guidance, "altman") ~ "Altman et al. 1983 (10.1136/bmj.286.6376.1489)",
    str_detect(external_guidance, "boushey harris bruemmer and archer 2008") ~ "Boushey et al. 2008 (10.1016/j.jada.2008.01.002)",
    str_detect(external_guidance, "boushey harris bruemmer archer and van horn 2006") ~ "Boushey et al. 2006 (10.1016/j.jada.2005.11.007)",
    str_detect(external_guidance, "bruemmer harris gleason et al 2009") ~ "Bruemmer et al. 2009 (10.1016/j.jada.2009.07.011)",
    str_detect(external_guidance, "cummings & rivara 2003|https://jamanetwork.com/journals/jamapediatrics/fullarticle/481292") ~ "Cummings & Rivara 2003 (10.1001/archpedi.157.4.321)",
    str_detect(external_guidance, "error") ~ "Cumming et al. 2007 (10.1083/jcb.200611141)",
    str_detect(external_guidance, "gleason harris sheean boushey and bruemmer 2010") ~ "Gleason et al. 2010 (10.1016/j.jada.2009.11.022)",
    str_detect(external_guidance, "gleason boushey and zoellner 2015") ~ "Gleason et al. 2015 (10.1016/j.jand.2015.03.011)",
    str_detect(external_guidance, "harris and raynor 2017") ~ "Harris & Raynor 2017 (10.1016/j.jand.2017.03.017)",
    str_detect(external_guidance, "harris boushey bruemmer archer 2008") ~ "Harris et al. 2008 (10.1016/j.jada.2008.06.426)",
    str_detect(external_guidance, "harris gleason sheean boushey beto bruemmer 2009") ~ "Harris et al. 2009 (10.1016/j.jada.2008.10.018)",
    str_detect(external_guidance, "harris sheean gleason bruemmer and boushey 2012") ~ "Harris et al. 2012 (10.1016/j.jada.2011.09.037)",
    str_detect(external_guidance, "hewitt 2012") ~ "Hewitt 2012 (10.1007/s10519-011-9504-z)",
    str_detect(external_guidance, "hollingshead 2008") ~ "Hollingshead 2008 (10.1093/jnci/djn351)",
    str_detect(external_guidance, "kempen 2011") ~ "Kempen 2011 (10.1016/j.ajo.2010.08.047)",
    str_detect(external_guidance, "http://jpet.aspetjournals.org/content/351/1/200") ~ "Motulsky 2014 (10.1124/jpet.114.219170)",
    str_detect(external_guidance, "olsen 2003") ~ "Olsen 2003 (10.1128/iai.71.12.6689-6692.2003)",
    str_detect(external_guidance, "olsen 2014") ~ "Olsen 2014 (10.1128/iai.00811-13)",
    str_detect(external_guidance, "fmri") ~ "Poldrack et al. 2008 (10.1016/j.neuroimage.2007.11.048)",
    str_detect(external_guidance, "richardson (and|&) overbaugh 2005") ~ "Richardson & Overbaugh 2005 (10.1128/jvi.79.2.669-676.2005)",
    str_detect(external_guidance, "sheean bruemmer gleason harris boushey van horn 2011") ~ "Sheean 2011 (10.1016/j.jada.2010.10.010)",
    str_detect(external_guidance, "tumor") ~ "Simon et al. 2009 (10.1093/jnci/djp335)",
    str_detect(external_guidance, "https://www.ahajournals.org/doi/10.1161/jaha.116.004142") ~ "Sullivan et al. 2016 (10.1161/jaha.116.004142)",
    str_detect(external_guidance, "zoellner van horn gleason boushey 2015") ~ "Zoellner et al. 2015 (10.1016/j.jand.2015.03.010)",
    str_detect(external_guidance, "zoellner and harris 2017") ~ "Zoellner & Harris 2017 (10.1016/j.jand.2017.01.018)",
    str_detect(external_guidance, "ten simple rules for reproducible computational research") ~ "Sandve et al. 2013 (10.1371/journal.pcbi.1003285)",
    str_detect(external_guidance, "use of anova versus t-test") ~ "Brady et al. 2015 (10.1105/tpc.15.00238)",
    str_detect(external_guidance, "know when your numbers are significant") ~ "Vaux 2012 (10.1038/492180a)",
    str_detect(external_guidance, "dupuy & simon 2007") ~ "Dupuy & Simon 2007 (10.1093/jnci/djk018)",
    str_detect(external_guidance, "how robust are your data") ~ "how robust are your data 2009 (10.1038/ncb0609-667a)",
    str_detect(external_guidance, "apa manual") ~ "American Psychological Association Publication Manual",
    str_detect(external_guidance, "fay & gerow 2014 (https://www.ncbi.nlm.nih.gov/books/NBK153593/)") ~ "Fay & Gerow 2014 (https://www.ncbi.nlm.nih.gov/books/NBK153593/)",
    str_detect(external_guidance, "mcdonald 2014. Handbook of Biological Statistics (http://www.biostathandbook.com/)") ~ "McDonald 2014. Handbook of Biological Statistics (http://www.biostathandbook.com/)",
    
    TRUE ~ external_guidance
  )) %>% 
  
  group_by(journal) %>% 
  mutate(
    external_guidance_list = toString(external_guidance),
    .after = external_guidance
  ) %>% 
  ungroup() %>% 
  mutate(external_guidance_list = na_if(external_guidance_list, "NA")) %>% 
  select(-external_guidance) %>% 
  distinct()

# Create (lookup) table of external guidance, with counts
lookup_external_guidance <-
  d_coding %>% 
  select(external_guidance = external_guidance_list) %>%
  filter(!is.na(external_guidance)) %>%
  separate_rows(external_guidance, sep = ",") %>% 
  mutate(external_guidance = str_trim(external_guidance)) %>% 
  count(external_guidance, name = "n_journals") %>% 
  arrange(desc(n_journals), external_guidance) %>% 

  # Add type of guidance
  mutate(guidance_type = case_when(
    
    # Papers have doi (mostly)
    str_detect(external_guidance, "10\\.\\d{4,9}/[-.;()/:\\w\\d]+$") ~ "other",
    str_detect(external_guidance, "statistics for biologists|ncbi.nlm.nih.gov/books/nbk153593|biostathandbook|biology_statistics_made_simple_using_excel") ~ "other", 
    
    # Reporting guidelines are mostly a single word, possibly with a dash
    str_detect(external_guidance, "^[\\w-]+$") ~ "reporting guideline",
    str_detect(external_guidance, "consort|apa jars") ~ "reporting guideline",
    str_detect(external_guidance, "^nih|^nlm|^fda") ~ "reporting guideline",
    str_detect(external_guidance, "hugenet") ~ "reporting guideline", #unsure
    str_detect(external_guidance, "cell star|frontiersin") ~ "publisher",
    str_detect(external_guidance, "American Psychological Association|American Statistical Association") ~ "other",
    TRUE ~ NA_character_
  )) %>%
  filter(external_guidance != "equator") # as per protocol - we do not count general references to EQUATOR guidelines, so remove

d_coding <- d_coding %>% select(-external_guidance_clean)

# bind d_coding and d_journals
d_all <- inner_join(d_coding,d_journals, by = 'journal')

write_csv(d_all, file =  here("data", "processed", "d_all.csv"))
save(d_all, file =  here("data", "processed", "d_all.rds"))

write_csv(lookup_external_guidance, file =  here("data", "processed", "lookup_external_guidance.csv"))
save(lookup_external_guidance, file =  here("data", "processed", "lookup_external_guidance.rds"))