# Make parrot scores
setwd("/")

message("loading packages and defining helper functions...\n")

# load packages
library(data.table)
library(tidyverse)
library(dtplyr)
library(parrot)
library(stm)
library(fixest)

source("/home/rer/proj/gssurvey/notebooks/final/pivot_scores/parrot_functions.R")

message("reading 2018 data...")

# read in 2018 data
users <- data.table::fread("/net/data/yougov/gssurvey/data/user_summary.csv")
queries <- data.table::fread("/net/data/yougov/gssurvey/data/search/queries.csv")
serps_sum <- data.table::fread("/net/data/yougov/gssurvey/data/search/serp_summary.csv")
survey <- data.table::fread("/net/data/yougov/gssurvey/data/survey/survey_data.csv")

message("merging 2018 data...")

# make YouGov df
merge_yg <- 
    lazy_dt(serps_sum) %>%
    dplyr::select(ugov_id, caseid, qry_id, qry_score) %>%
inner_join(
    lazy_dt(queries) %>%
    dplyr::select(id, qry, nwords),
by = c("qry_id" = "id")) %>%
unique() %>%
mutate(index = 1:n()) %>%
left_join(as_tibble(survey) %>%
         dplyr::select(caseid, pid3_reduced, pid7),
         by = "caseid") %>%
left_join(as_tibble(users) %>% 
         dplyr::select(caseid, search_mean_qry_bias),
         by = "caseid") %>%
as_tibble() %>%
mutate(party7 = case_when(
            pid7 == "Strong Democrat" ~ 1,
            pid7 == "Not very strong Democrat" ~ 2,
            pid7 == "Lean Democrat" ~ 3,
            pid7 %in% c("Independent","Not sure") ~ 4,
            pid7 == "Lean Republican" ~ 5,
            pid7 == "Not very strong Republican" ~ 6,
            pid7 == "Strong Republican" ~ 7))


scalequery_2018 <- flatten_text_pivot(df = merge_yg, 
                                      text_col = "qry", 
                                      flatten_column = "party7")

# merge data
out_df_18 <- scalequery_2018$appended_data %>% 
              mutate(caseid = as.character(caseid)) %>%
              group_by(caseid) %>%
              summarise_at(.vars = dplyr::vars(starts_with("X")),
                           .funs = mean)

message("writing 2018 output to file...")
textmodel <- scalequery_2018$textmodel
write.csv(out_df_18, file = "/net/data/yougov/gssurvey/data/user_query_avgpivots_2018.csv")
save(textmodel, file = "/net/data/yougov/gssurvey/data/query_pivot_scale_2018.rdata")
rm(list = ls())

message("reading 2020 data...")

# 2020 next
dat20 <- readr::read_tsv("/net/data/yougov/webusage/data/qualtrics/user/active.tsv")
serps20 <- readr::read_tsv("/net/data/yougov/webusage/data/qualtrics/activity/google_search/serp_summary.tsv")
meta20 <- readr::read_csv("/home/jongreen/search_stuff/survey_ids_timing.csv")

# make output df
wave_meta <- meta20 %>%
group_by(wave) %>%
summarise(mindate = min(date),
         maxdate = max(date))

dat20 <- dat20 %>% arrange(install_date)

dat20$id_wave <- sapply(dat20$install_date, function(x){
    which_start <- wave_meta %>% filter(!maxdate < x) %>% slice(1) %>% pull(wave)
})

message("merging 2020 data...")
merge_ps <- 
    lazy_dt(serps20) %>%
    # remove serps with one result
    filter(n_results > 1) %>% 
    dplyr::select(user_id, qry) %>%
    unique() %>%
    mutate(index = 1:n()) %>%
inner_join(as_tibble(meta20) %>%
           filter(!duplicated(psid)) %>%
         dplyr::select(psid, party7) %>% 
           unique(),
         by = c("user_id" = "psid")) %>%
as_tibble()

scalequery_2020 <- flatten_text_pivot(df = merge_ps, 
                                      text_col = "qry", 
                                      flatten_column = "party7")

out_df_20 <- scalequery_2020$appended_data %>% 
              mutate(user_id = as.character(user_id)) %>%
              group_by(user_id) %>%
              summarise_at(.vars = dplyr::vars(starts_with("X")),
                           .funs = mean)

message("writing 2020 output to file...")
textmodel <- scalequery_2020$textmodel

write.csv(out_df_20, file = "/net/data/yougov/webusage/data/qualtrics/user/user_query_avgpivots_2020.csv")
save(textmodel, file = "/net/data/yougov/webusage/data/qualtrics/user/query_pivot_scale_2020.rdata")
