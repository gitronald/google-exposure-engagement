### Modeling and associated output for “User choice outweighs algorithmic curation for news on Google Search”
### Contact Jon Green (jo.green@northeastern.edu) with questions

setwd("/")

# load packages
library(data.table)
library(tidyverse)
library(dtplyr)
library(MASS)
library(modelsummary)
library(fixest)
library(dineq)

options("modelsummary_format_numeric_latex" = "plain")
set.seed(11111)

dir_main = "/home/rer/proj/gssurvey/notebooks/final/"
dir_tables = paste0(dir_main, "tables/regressions/")

source(paste0(dir_main, "/regressions/helper_functions.R"))

# read in data
dat_2018 <- readr::read_csv(paste0(dir_main, "data/users2018.csv"))
dat_2020 <- readr::read_csv(paste0(dir_main, "data/users2020.csv"))

dat_2018$white <- with(dat_2018, as.numeric(race == "White"))
dat_2020$white <- with(dat_2020, as.numeric(race_r == 1))

dat_2018$pid7_reduced <- with(dat_2018, factor(pid7_reduced, levels = c("Independent/Not sure",
                                                                        "Strong Democrat",
                                                                        "Not very strong Democrat",
                                                                       "Lean Democrat",
                                                                       "Lean Republican",
                                                                        "Not very strong Republican",
                                                                       "Strong Republican")))

dat_2020$party_cat <- with(dat_2020, factor(party_cat, levels = c("No party",
                                                                        "Strong D",
                                                                        "Not very strong D",
                                                                       "Ind/Lean D",
                                                                       "Ind/Lean R",
                                                                        "Not very strong R",
                                                                       "Strong R")))


# specify formulae
formulae_2018 <- c("pid7_reduced", 
                   "age_group",
                   "pid7_reduced + age_group",
                  "pid7_reduced + age_group + high_news + college + female + white",
                  "pid7_reduced + age_group + high_news + college + female + white + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9")

vars_map_2018 <- c("pid7_reducedStrong Democrat" = "Strong Democrat",
                   "pid7_reducedNot very strong Democrat" = "Not very strong Democrat",
                   "pid7_reducedLean Democrat" = "Lean Democrat",
                   "pid7_reducedIndependent/Not sure" = "Independent/Not sure",
                   "pid7_reducedLean Republican" = "Lean Republican",
                   "pid7_reducedNot very strong Republican" = "Not very strong Republican",
                   "pid7_reducedStrong Republican" = "Strong Republican",
                   "age_group25-44" = "Age Group: 25-44",
                   "age_group45-64" = "Age Group: 45-64",
                   "age_group65+" = "Age Group: 65+",
                   "high_news" = "High News Interest",
                   "college" = "College",
                   "female" = "Female",
                   "white" = "White",
                   "query_right" = "Search Query Partisanship",
                  "X1" = "Query Pivot: 1",
                  "X2" = "Query Pivot: 2",
                  "X3" = "Query Pivot: 3",
                  "X4" = "Query Pivot: 4",
                  "X5" = "Query Pivot: 5",
                  "X6" = "Query Pivot: 6",
                  "X7" = "Query Pivot: 7",
                  "X8" = "Query Pivot: 8",
                  "X9" = "Query Pivot: 9")

formulae_2020 <- c("party_cat", 
                   "age_group",
                   "party_cat + age_group",
                  "party_cat + age_group + high_interest + college + female + white",
                  "party_cat + age_group + high_interest + college + female + white + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9")

vars_map_2020 <- c("party_catStrong D" = "Strong Democrat",
                   "party_catNot very strong D" = "Not very strong Democrat",
                   "party_catInd/Lean D" = "Lean Democrat",
                   "party_catNo party" = "Other/No party",
                   "party_catInd/Lean R" = "Lean Republican",
                   "party_catNot very strong R" = "Not very strong Republican",
                   "party_catStrong R" = "Strong Republican",
                   "age_group25-44" = "Age Group: 25-44",
                   "age_group45-64" = "Age Group: 45-64",
                   "age_group65+" = "Age Group: 65+",
                   "high_interest" = "High Political Interest",
                   "college" = "College",
                   "female" = "Female",
                   "white" = "White",
                   "query_right" = "Search Query Partisanship",
                  "X1" = "Query Pivot: 1",
                  "X2" = "Query Pivot: 2",
                  "X3" = "Query Pivot: 3",
                  "X4" = "Query Pivot: 4",
                  "X5" = "Query Pivot: 5",
                  "X6" = "Query Pivot: 6",
                  "X7" = "Query Pivot: 7",
                  "X8" = "Query Pivot: 8",
                  "X9" = "Query Pivot: 9")

# for each combination of year, behavior, and outcome:
    # run model, 
    # output full regression table,
    # make dataframe of age/party ID coefficients, 
    # and make table of age/party ID group N

# ==============================================================================
# Unreliable News 

# ------------------------------------------------------------------------------
# 2020

message("2020, Browser History, Unreliable News")
mods_browse_2020 <- lapply(formulae_2020, function(f){
    m <- mgcv::gam(as.formula(paste0("browser_history_n_fake_either ~ offset(log(1+browser_history_n_news)) + ", f)),
          data = dat_2020 %>% filter(!is.na(browser_history_window)),
          family = "nb")
    m_prep <- gam_to_ms(m)
    return(m_prep)
})

modelsummary::modelsummary(mods_browse_2020[1:4], 
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           coef_map = vars_map_2020,
                           title = "Unreliable URLs (Browser History) (2020)",
                           output = paste0(dir_tables, "unreliable_browse_2020.tex"))

tabs_browse_2020<- extract_age_pid_coefs(modlist = mods_browse_2020[1:4],
                                               year = 2020, 
                                               behavior = "Browser History", 
                                               outcome = "Unreliable URLs")

age_n_browse_2020 <- 
    party_age_n_func(dat_2020, browser_history_window, party_cat, age_group,
                     filter_vec = c(party_cat, age_group, high_interest, college, female, white, browser_history_n_fake_either, browser_history_n_news),
                    year = 2020, behavior = "Browser History")

message("2020, Search Results, Unreliable News")
mods_search_2020 <- lapply(formulae_2020, function(f){
    m <- mgcv::gam(as.formula(paste0("activity_gs_search_n_urls_fake_either ~ offset(log(1+activity_gs_search_n_urls_news)) + ", f)),
          data = dat_2020 %>% filter(!is.na(activity_gs_search_window)),
          family = "nb")
    m_prep <- gam_to_ms(m)
    return(m_prep)
})

modelsummary::modelsummary(mods_search_2020, 
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           coef_map = vars_map_2020,
                           title = "Unreliable URLs (Search Results) (2020)",
                           output = paste0(dir_tables, "unreliable_search_2020.tex"))

tabs_search_2020<- extract_age_pid_coefs(modlist = mods_search_2020,
                                               year = 2020, 
                                               behavior = "Search Results", 
                                               outcome = "Unreliable URLs")

age_n_search_2020 <- 
    party_age_n_func(dat_2020, activity_gs_search_window, party_cat, age_group,
                     filter_vec = c(party_cat, age_group, high_interest, college, female, white, activity_gs_search_n_urls_fake_either, activity_gs_search_n_urls_news),
                    year = 2020, behavior = "Search Results")

message("2020, Follows from Search, Unreliable News")
mods_follows_2020 <- lapply(formulae_2020, function(f){
    m <- mgcv::gam(as.formula(paste0("activity_gs_follow_n_fake_either ~ offset(log(1+activity_gs_follow_n_news)) + ", f)),
          data = dat_2020 %>% filter(!is.na(activity_gs_search_window)),
          family = "nb")
    m_prep <- gam_to_ms(m)
    return(m_prep)
})

modelsummary::modelsummary(mods_follows_2020, 
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           coef_map = vars_map_2020,
                           title = "Unreliable URLs (Follows from Search) (2020)",
                           output = paste0(dir_tables, "unreliable_follows_2020.tex"))

tabs_follow_2020<- extract_age_pid_coefs(modlist = mods_follows_2020,
                                               year = 2020, 
                                               behavior = "Follows from Search", 
                                               outcome = "Unreliable URLs")

age_n_follows_2020 <- 
    party_age_n_func(dat_2020, activity_gs_search_window, party_cat, age_group,
                    filter_vec = c(party_cat, age_group, high_interest, college, female, white, activity_gs_follow_n_fake_either, activity_gs_follow_n_news),
                    year = 2020, behavior = "Follows from Search")

# ------------------------------------------------------------------------------
# 2018

message("2018, Browser History, Unreliable News")
mods_browse_2018 <- lapply(formulae_2018, function(f){
    m <- mgcv::gam(as.formula(paste0("browse_n_total_fake_either ~ offset(log(1+browse_n_total_news)) + ", f)),
          data = dat_2018 %>% filter(!is.na(browse_window)),
          family = "nb")
    m_prep <- gam_to_ms(m)
    return(m_prep)
})

modelsummary::modelsummary(mods_browse_2018[1:4], 
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           coef_map = vars_map_2018,
                           title = "Unreliable URLs (Browser History) (2018)",
                           output = paste0(dir_tables, "unreliable_browse_2018.tex"))

tabs_browse_2018<- extract_age_pid_coefs(modlist = mods_browse_2018[1:4],
                                               year = 2018, 
                                               behavior = "Browser History", 
                                               outcome = "Unreliable URLs")

age_n_browse_2018 <- 
    party_age_n_func(dat_2018, browse_window, pid7_reduced, age_group,
                    filter_vec = c(pid7_reduced, age_group, high_news, college, female, white, browse_n_total_fake_either, browse_n_total_news),
                    year = 2018, behavior = "Browser History")

message("2018, Search Results, Unreliable News")
mods_search_2018 <- lapply(formulae_2018, function(f){
    m <- mgcv::gam(as.formula(paste0("search_n_urls_fake_either ~ offset(log(1+search_n_urls_news)) + ", f)),
          data = dat_2018 %>% filter(!is.na(search_window)),
          family = "nb")
    m_prep <- gam_to_ms(m)
    return(m_prep)
})

modelsummary::modelsummary(mods_search_2018, 
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           coef_map = vars_map_2018,
                           title = "Unreliable URLs (Search Results) (2018)",
                           output = paste0(dir_tables, "unreliable_search_2018.tex"))

tabs_search_2018<- extract_age_pid_coefs(modlist = mods_search_2018,
                                               year = 2018, 
                                               behavior = "Search Results", 
                                               outcome = "Unreliable URLs")

age_n_search_2018 <- 
    party_age_n_func(dat_2018, search_window, pid7_reduced, age_group,
                    filter_vec = c(pid7_reduced, age_group, high_news, college, female, white, search_n_urls_fake_either, search_n_urls_news),
                    year = 2018, behavior = "Search Results")

message("2018, Follows from Search, Unreliable News")
mods_follows_2018 <- lapply(formulae_2018, function(f){
    m <- mgcv::gam(as.formula(paste0("follow_n_fake_either ~ offset(log(1+follow_n_news)) + ", f)),
          data = dat_2018 %>% filter(!is.na(follow_window)),
          family = "nb")
    m_prep <- gam_to_ms(m)
    return(m_prep)
})

modelsummary::modelsummary(mods_follows_2018, 
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           coef_map = vars_map_2018,
                           title = "Unreliable URLs (Follows from Search) (2018)",
                           output = paste0(dir_tables, "unreliable_follows_2018.tex"))

tabs_follow_2018<- extract_age_pid_coefs(modlist = mods_follows_2018,
                                               year = 2018, 
                                               behavior = "Follows from Search", 
                                               outcome = "Unreliable URLs")

age_n_follow_2018 <- 
    party_age_n_func(dat_2018, follow_window, pid7_reduced, age_group,
                    filter_vec = c(pid7_reduced, age_group, high_news, college, female, white, follow_n_fake_either, follow_n_news),
                    year = 2018, behavior = "Follows from Search")

# ==============================================================================
# Partisan News

# ------------------------------------------------------------------------------
# 2020

message("2020, Browser History, Partisan Slant")
mods_browse_bias_2020 <- lapply(formulae_2020, function(f){
    m <- lm(as.formula(paste0("browser_history_mean_bias_news ~ ", f)), 
       data = dat_2020 %>% filter(!is.na(browser_history_window)))
    m_prep <- ols_to_ms(m)
    return(m_prep)
})

modelsummary::modelsummary(mods_browse_bias_2020[1:4], 
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           coef_map = vars_map_2020,
                           title = "Average Domain Audience Score (Browser History) (2020)",
                           output = paste0(dir_tables, "bias_browse_2020.tex"))

tabs_browse_bias_2020<- 
extract_age_pid_coefs(modlist = mods_browse_bias_2020[1:4],
                        year = 2020, 
                      behavior = "Browser History", 
                      outcome = "Partisan Slant")

age_n_browse_bias_2020 <- 
    party_age_n_func(dat_2020, browser_history_window, party_cat, age_group,
                    filter_vec = c(party_cat, age_group, high_interest, college, female, white, browser_history_mean_bias_news),
                    year = 2020, behavior = "Browser History")


message("2020, Search Results, Partisan Slant")
mods_search_bias_2020 <- lapply(formulae_2020, function(f){
    m <- lm(as.formula(paste0("activity_gs_search_mean_average_bias_news ~ ", f)), 
       data = dat_2020 %>% filter(!is.na(activity_gs_search_window)))
    m_prep <- ols_to_ms(m)
    return(m_prep)
})

modelsummary::modelsummary(mods_search_bias_2020, 
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           coef_map = vars_map_2020,
                           title = "Average Domain Audience Score (Search Results) (2020)",
                           output = paste0(dir_tables, "bias_search_2020.tex"))

tabs_search_bias_2020<- extract_age_pid_coefs(modlist = mods_search_bias_2020,
                                               year = 2020, 
                                               behavior = "Search Results", 
                                               outcome = "Partisan Slant")

age_n_search_bias_2020 <- 
    party_age_n_func(dat_2020, activity_gs_search_window, party_cat, age_group,
                    filter_vec = c(party_cat, age_group, high_interest, college, female, white, activity_gs_search_mean_average_bias_news),
                    year = 2020, behavior = "Search Results")


message("2020, Follows from Search, Partisan Slant")
mods_follow_bias_2020 <- lapply(formulae_2020, function(f){
    m <- lm(as.formula(paste0("activity_gs_follow_mean_bias_news ~ ", f)), 
       data = dat_2020 %>% filter(!is.na(activity_gs_follow_window)))
    m_prep <- ols_to_ms(m)
    return(m_prep)
})

modelsummary::modelsummary(mods_follow_bias_2020, 
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           coef_map = vars_map_2020,
                           title = "Average Domain Audience Score (Follows from Search) (2020)",
                           output = paste0(dir_tables, "bias_follows_2020.tex"))

tabs_follow_bias_2020<- extract_age_pid_coefs(modlist = mods_follow_bias_2020,
                                               year = 2020, 
                                               behavior = "Follows from Search", 
                                               outcome = "Partisan Slant")

age_n_follow_bias_2020 <- 
    party_age_n_func(dat_2020, activity_gs_follow_window, party_cat, age_group,
                    filter_vec = c(party_cat, age_group, high_interest, college, female, white, activity_gs_follow_mean_bias_news),
                    year = 2020, behavior = "Follows from Search")

# ------------------------------------------------------------------------------
# 2018

message("2018, Browser History, Partisan Slant")
mods_browse_bias_2018 <- lapply(formulae_2018, function(f){
    m <- lm(as.formula(paste0("browse_mean_bias_news ~ ", f)), 
       data = dat_2018 %>% filter(!is.na(browse_window)))
    m_prep <- ols_to_ms(m)
    return(m_prep)
})

modelsummary::modelsummary(mods_browse_bias_2018[1:4], 
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           coef_map = vars_map_2018,
                           title = "Average Domain Audience Score (Browser History) (2018)",
                           output = paste0(dir_tables, "bias_browse_2018.tex"))

tabs_browse_bias_2018 <- extract_age_pid_coefs(modlist = mods_browse_bias_2018[1:4],
                                               year = 2018, 
                                               behavior = "Browser History", 
                                               outcome = "Partisan Slant")

age_n_browse_bias_2018 <- 
    party_age_n_func(dat_2018, browse_window, pid7_reduced, age_group,
                    filter_vec = c(pid7_reduced, age_group, high_news, college, female, white, browse_mean_bias_news),
                    year = 2018, behavior = "Browser History")


message("2018, Search Results, Partisan Slant")
mods_search_bias_2018 <- lapply(formulae_2018, function(f){
    m <- lm(as.formula(paste0("search_mean_average_bias_news ~ ", f)),
       data = dat_2018 %>% filter(!is.na(search_window)))
    m_prep <- ols_to_ms(m)
    return(m_prep)
})

modelsummary::modelsummary(mods_search_bias_2018, 
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           coef_map = vars_map_2018,
                           title = "Average Domain Audience Score (Search Results) (2018)",
                           output = paste0(dir_tables, "bias_search_2018.tex"))

tabs_search_bias_2018 <- extract_age_pid_coefs(modlist = mods_search_bias_2018,
                                               year = 2018, 
                                               behavior = "Search Results", 
                                               outcome = "Partisan Slant")

age_n_search_bias_2018 <- 
    party_age_n_func(dat_2018, search_window, pid7_reduced, age_group,
                    filter_vec = c(pid7_reduced, age_group, high_news, college, female, white, search_mean_average_bias_news),
                    year = 2018, behavior = "Search Results")

message("2018, Follows from Search, Partisan Slant")
mods_follow_bias_2018 <- lapply(formulae_2018, function(f){
    m <- lm(as.formula(paste0("follow_mean_bias_news ~ ", f)), 
       data = dat_2018 %>% filter(!is.na(follow_window)))
    m_prep <- ols_to_ms(m)
    return(m_prep)
})

modelsummary::modelsummary(mods_follow_bias_2018, 
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           coef_map = vars_map_2018,
                           title = "Average Domain Audience Score (Follows from Search) (2018)",
                           output = paste0(dir_tables, "bias_follows_2018.tex"))

tabs_follow_bias_2018 <- extract_age_pid_coefs(modlist = mods_follow_bias_2018,
                                               year = 2018, 
                                               behavior = "Follows from Search", 
                                               outcome = "Partisan Slant")

age_n_follow_bias_2018 <- 
    party_age_n_func(dat_2018, follow_window, pid7_reduced, age_group,
                    filter_vec = c(pid7_reduced, age_group, high_news, college, female, white, follow_mean_bias_news),
                    year = 2018, behavior = "Follows from Search")

# ==============================================================================
# Save group-level counts

# Stack Group N for Unreliable URLs
pid_age_n_stack <- bind_rows(
    age_n_search_2018,
    age_n_follow_2018,
    age_n_browse_2018,
    age_n_search_2020,
    age_n_follows_2020,
    age_n_browse_2020
) %>%
mutate(Outcome = "Unreliable URLs")

# Stack Group N for Partisan Slant
pid_age_n_stack_bias <- bind_rows(
    age_n_search_bias_2018,
    age_n_follow_bias_2018,
    age_n_browse_bias_2018,
    age_n_search_bias_2020,
    age_n_follow_bias_2020,
    age_n_browse_bias_2020
) %>%
mutate(Outcome = "Partisan Slant")

# Reshape and write to file
bind_rows(pid_age_n_stack, pid_age_n_stack_bias) %>%
pivot_wider(names_from = c("Behavior", "Year"),
            values_from = "N") %>%
write.csv(paste0(dir_main, "data/pid_age_n_by_behavior.csv"))

# ==============================================================================
# Save coefficients table

## Stack Party ID / Age Coefficients
tabs_stack <- 
    bind_rows(
        tabs_search_2018, tabs_follow_2018, tabs_browse_2018,
        tabs_search_2020, tabs_follow_2020, tabs_browse_2020,
        tabs_search_bias_2018, tabs_follow_bias_2018, tabs_browse_bias_2018,
        tabs_search_bias_2020, tabs_follow_bias_2020, tabs_browse_bias_2020
)

# Join coefficients with group N and write to file
tabs_stack %>%
left_join(bind_rows(pid_age_n_stack, pid_age_n_stack_bias), 
          by = c("var" = "Group",
                 "behavior" = "Behavior",
                 "year" = "Year",
                "outcome" = "Outcome")) %>%
write.csv(file = paste0(dir_main, "data/tabs_stack.csv"))



# ==============================================================================
# Save group N table

# Get total group Ns for age group and party ID (not filtered by behavior or outcome)
group_n_age <- 
bind_rows(
    dat_2018 %>%
    group_by(age_group) %>%
    summarise(n = n()) %>%
    mutate(year = 2018),
    dat_2020 %>%
    group_by(age_group) %>%
    summarise(n = n()) %>%
    mutate(year = 2020)
)
group_n_pid <-
bind_rows(
    dat_2018 %>%
    mutate(p7 = pid7_reduced) %>%
    group_by(p7) %>%
    summarise(n = n()) %>%
    mutate(year = 2018),
    dat_2020 %>%
    mutate(p7 = case_when(
        party_cat == "Strong D" ~ "Strong Democrat",
        party_cat == "Not very strong D" ~ "Not very strong Democrat",
        party_cat == "Ind/Lean D" ~ "Lean Democrat",
        party_cat == "No party" ~ "Independent/Other",
        party_cat == "Ind/Lean R" ~ "Lean Republican",
        party_cat == "Not very strong R" ~ "Not very strong Republican",
        party_cat == "Strong R" ~ "Strong Republican")) %>%
    group_by(p7) %>%
    summarise(n = n()) %>%
    mutate(year = 2020)
)

group_n_comb <- bind_rows(
    group_n_age %>%
    mutate(age_group = paste0("Age Group: ", age_group)) %>%
    rename(group = age_group) %>%
    pivot_wider(names_from = year,
                values_from = n),
    group_n_pid %>%
    mutate(p7 = factor(
                    case_when(
                        grepl("Independent", p7) ~ "Independent/Other",
                        T ~ p7), 
                    levels = c("Strong Democrat","Not very strong Democrat","Lean Democrat",
                               "Independent/Other",
                               "Lean Republican","Not very strong Republican","Strong Republican")
                        )
            )%>%
    rename(group = p7) %>%
    pivot_wider(names_from = year,
                values_from = n)
    )
names(group_n_comb) <- c("Group","N_2018","N_2020")

# arrange and write to file
group_n_comb %>%
slice(c(1:4, 10, 8, 6, 5, 7, 9, 11)) %>%
write.csv(file = paste0(dir_main, "data/n_table.csv"))

# ==============================================================================
# Save Gini table

### Gini coefficients for outcomes
gini_table <- data.frame(Year = rep(c("2018","2020"), each = 3), 
                         Outcome = rep(c("Exposure (Search Results)","Engagement (Follows)", "Engagement (Browser History)"),2), 
                         Gini = c(round(dineq::gini.wtd(dat_2018$search_n_urls_fake_either[!is.na(dat_2018$search_window)]),3), 
                                  round(dineq::gini.wtd(dat_2018$follow_n_fake_either[!is.na(dat_2018$follow_window)]),3), 
                                  round(dineq::gini.wtd(dat_2018$browse_n_total_fake_either[!is.na(dat_2018$browse_window)]),3), 
                                  round(dineq::gini.wtd(dat_2020$activity_gs_search_n_urls_fake_either[!is.na(dat_2020$activity_gs_search_window)]),3), 
                                  round(dineq::gini.wtd(dat_2020$activity_gs_follow_n_fake_either[!is.na(dat_2020$activity_gs_follow_window)]),3), 
                                  round(dineq::gini.wtd(dat_2020$browser_history_n_fake_either[!is.na(dat_2020$browser_history_window)]),3))) 

write.csv(gini_table, file = paste0(dir_main, "data/gini_table.csv"))

