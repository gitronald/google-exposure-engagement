library(tidyverse)

dir_main = "/home/rer/proj/gssurvey/notebooks/final/"
tabs_stack <- readr::read_csv(paste0(dir_main, "data/tabs_stack.csv"))


# ==============================================================================
# Coefficient plots

# Plot coefficients for extended data: Strong Party ID, unreliable News
partisan_unreliable_plot <- tabs_stack %>%
reshape2::melt(id.vars = c("var","behavior","outcome","year","model")) %>%
mutate(spec = ifelse(grepl("without", variable), "Without Query Text Features","With Query Text Features"),
      type = gsub("_with_query|_without_query", "", variable)) %>%
dplyr::select(-variable) %>%
pivot_wider(names_from = c("type"),
            values_from = value) %>%
filter(var %in% c("Strong Democrat","Strong Republican") & outcome == "Unreliable URLs") %>%
mutate(behavior = factor(behavior, levels = c("Search Results","Follows from Search","Browser History"))) %>%
ggplot(aes(x = fct_rev(fct_inorder(var)), 
           y = coef, 
           pch = spec))+
facet_wrap(~behavior+year , nrow = 3, ncol = 2)+
geom_pointrange(position = position_dodge(width = .4),
               aes(ymin = lower, 
                   ymax = upper))+
geom_hline(yintercept = 0, lty = "dashed")+
scale_shape_discrete(name = "Model Specification\n(Both include all demographic covariates)")+
coord_flip()+
labs(title = "Partisan differences in unreliable URL exposure, follows, and engagement",
    subtitle = "Estimates for Strong Democrats and Strong Republicans relative to Independents with no party lean",
     caption = "Coefficients and 95% uncertainty intervals shown",
    x = "",
    y = "Negative Binomial Coefficient")+
theme_bw()+
theme(plot.title = element_text(size = 14, face = "bold"),
     strip.text = element_text(face = "bold"))
ggsave(partisan_unreliable_plot, 
       file = paste0(dir_main, "outputs/figures/coef_plot_meta_pid_unreliable.png"), 
       width = 10, height = 6)

# Plot coefficients for extended data: Strong Party ID, partisan news
partisan_slant_plot <- tabs_stack %>%
reshape2::melt(id.vars = c("var","behavior","outcome","year","model")) %>%
mutate(spec = ifelse(grepl("without", variable), "Without Query Text Features","With Query Text Features"),
      type = gsub("_with_query|_without_query", "", variable)) %>%
dplyr::select(-variable) %>%
pivot_wider(names_from = c("type"),
            values_from = value) %>%
filter(var %in% c("Strong Democrat","Strong Republican") & outcome == "Partisan Slant") %>%
mutate(behavior = factor(behavior, levels = c("Search Results","Follows from Search","Browser History"))) %>%
ggplot(aes(x = fct_rev(fct_inorder(var)), 
           y = coef, 
           pch = spec))+
facet_wrap(~behavior+year , nrow = 3, ncol = 2)+
geom_pointrange(position = position_dodge(width = .4),
               aes(ymin = lower, 
                   ymax = upper))+
geom_hline(yintercept = 0, lty = "dashed")+
scale_shape_discrete(name = "Model Specification\n(Both include all demographic covariates)")+
coord_flip()+
labs(title = "Partisan differences in average audience score of exposure, follows, and engagement",
    subtitle = "Estimates for Strong Democrats and Strong Republicans relative to Independents with no party lean",
    caption = "Coefficients and 95% uncertainty intervals shown",
    x = "",
    y = "OLS Coefficient")+
theme_bw()+
theme(plot.title = element_text(size = 14, face = "bold"),
     strip.text = element_text(face = "bold"))
ggsave(partisan_slant_plot, 
       file = paste0(dir_main, "outputs/figures/coef_plot_meta_pid_slant.png"), 
       width = 10, height = 6)

# Plot coefficients for extended data: Age, unreliable news
age_unreliable_plot <- tabs_stack %>%
reshape2::melt(id.vars = c("var","behavior","outcome","year","model")) %>%
mutate(spec = ifelse(grepl("without", variable), "Without Query Text Features","With Query Text Features"),
      type = gsub("_with_query|_without_query", "", variable)) %>%
dplyr::select(-variable) %>%
pivot_wider(names_from = c("type"),
            values_from = value) %>%
filter(grepl("Age", var) & outcome == "Unreliable URLs") %>%
mutate(behavior = factor(behavior, levels = c("Search Results","Follows from Search","Browser History"))) %>%
ggplot(aes(x = fct_rev(fct_inorder(var)), 
           y = coef, 
           pch = spec))+
facet_wrap(~behavior+year , nrow = 3, ncol = 2)+
geom_pointrange(position = position_dodge(width = .4),
               aes(ymin = lower, 
                   ymax = upper))+
geom_hline(yintercept = 0, lty = "dashed")+
scale_shape_discrete(name = "Model Specification\n(Both include partisanship and demographic covariates)")+
coord_flip()+
labs(title = "Age group differences in unreliable URL exposure, follows, and engagement",
    subtitle = "Estimates relative to 18-24 year old age group",
    caption = "Coefficients and 95% uncertainty intervals shown",
    x = "",
    y = "Negative Binomial Coefficient")+
theme_bw()+
theme(plot.title = element_text(size = 14, face = "bold"),
     strip.text = element_text(face = "bold"))
ggsave(age_unreliable_plot, 
       file = paste0(dir_main, "outputs/figures/coef_plot_meta_age_unreliable.png"), 
       width = 10, height = 6)

# Plot coefficients for extended data: Age, partisan slant
age_slant_plot <- tabs_stack %>%
reshape2::melt(id.vars = c("var","behavior","outcome","year","model")) %>%
mutate(spec = ifelse(grepl("without", variable), "Without Query Text Features","With Query Text Features"),
      type = gsub("_with_query|_without_query", "", variable)) %>%
dplyr::select(-variable) %>%
pivot_wider(names_from = c("type"),
            values_from = value) %>%
filter(grepl("Age", var) & outcome == "Unreliable URLs") %>%
mutate(behavior = factor(behavior, levels = c("Search Results","Follows from Search","Browser History"))) %>%
ggplot(aes(x = fct_rev(fct_inorder(var)), 
           y = coef, 
           pch = spec))+
facet_wrap(~behavior+year , nrow = 3, ncol = 2)+
geom_pointrange(position = position_dodge(width = .4),
               aes(ymin = lower, 
                   ymax = upper))+
geom_hline(yintercept = 0, lty = "dashed")+
scale_shape_discrete(name = "Model Specification\n(Both include partisanship and demographic covariates)")+
coord_flip()+
labs(title = "Age differences in average audience score of exposure, follows, and engagement",
    subtitle = "Estimates relative to 18-24 year old age group",
    caption = "Coefficients and 95% uncertainty intervals shown",
     x = "",
    y = "OLS Coefficient")+
theme_bw()+
theme(plot.title = element_text(size = 14, face = "bold"),
     strip.text = element_text(face = "bold"))
ggsave(age_slant_plot, 
       file = paste0(dir_main, "outputs/figures/coef_plot_meta_age_slant.png"), 
       width = 10, height = 6)
