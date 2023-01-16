### Helper functions for modeling and organizing output in “User choice outweighs algorithmic curation for news on Google Search”
### Contact Jon Green (jo.green@northeastern.edu) with questions

# function to calculate confidence interval of coefficient estimate
# function takes:
    # est: coefficient estimate
    # se: coefficient standard error
    # n: number of observations
    # npar: number of parameters
    # alpha: significance level (default is .05)
get_confint <- function(est, se, n, npar, alpha = .05, print_t = TRUE){
  
  # degrees of freedom is number of observations - number of params
  df <- n - npar
  
  # set t critical value from alpha
  t.val <- qt((1-(alpha/2)), df)
    
  # set t stat as how many SEs estimate is from zero
  t.stat <- est / se
    
  # p value from two sided t test
  p.val <- 2*pt(abs(t.stat), df, lower.tail = FALSE)
  
  # bounds are est +/- critical value number of SEs
  lwr <- est - t.val*se
  upr <- est + t.val*se
  
  sig <- 0
  if(lwr > 0 | upr < 0){
    sig <- 1
  }
  if(print_t){
      return(data.frame(lower = lwr,
                    coef = est,
                    upper = upr,
                    p = p.val,
                    statsig = sig,
                    t = t.stat))
  }else{
      return(data.frame(lower = lwr,
                    coef = est,
                    upper = upr,
                    p = p.val,
                    statsig = sig))
      }
  
}

# function to extract coefficients for age and party ID from list of models
# function takes:
    # modlist: list of model objects
    # year: 2018 or 2020
    # behavior: search results, search follows, browser history
    # outcome: unreliable or partisan news
    # alpha: 1-CI level to pass through to get_confint (default is .05)
# outputs a table of coefficients, 95% CIs, and p-values by behavior, year, and outcome (with model type -- OLS or negative binomial -- listed) 
extract_age_pid_coefs <- function(modlist, year, behavior, outcome, alpha = .05, print_t = TRUE){
    
    if(grepl("partisan", tolower(outcome))){
        model_type = "OLS"
    }else{
        model_type = "Negative Binomial"
    }
    
    if(model_type == "Negative Binomial"){
        mod_nq <- modlist[[4]]$tidy %>%
             mutate(term = case_when(
               grepl("Strong D", term) ~ "Strong Democrat",
               grepl("Not very strong D", term) ~ "Not very strong Democrat",
               grepl("Lean D", term) ~ "Ind/Lean Democrat",
               grepl("Lean R", term) ~ "Ind/Lean Republican",
               grepl("Not very strong R", term) ~ "Not very strong Republican",
               grepl("Strong R", term) ~ "Strong Republican",
                T ~ term
           ))
        mod_nq_n <- modlist[[4]]$glance$Num.Obs
    }else{
        mod_nq <- data.frame(
            term = modlist[[4]]$tidy$term,
            estimate = modlist[[4]]$tidy$estimate,
            std.error = modlist[[4]]$tidy$std.error
        ) %>%
         mutate(term = case_when(
               grepl("Strong D", term) ~ "Strong Democrat",
               grepl("Not very strong D", term) ~ "Not very strong Democrat",
               grepl("Lean D", term) ~ "Ind/Lean Democrat",
               grepl("Lean R", term) ~ "Ind/Lean Republican",
               grepl("Not very strong R", term) ~ "Not very strong Republican",
               grepl("Strong R", term) ~ "Strong Republican",
                T ~ term
           ))
        mod_nq_n <- modlist[[4]]$glance$Num.Obs
    }

    df_nq <- map_df(2:10, function(x){
        get_confint(est = mod_nq$estimate[x], 
                se = mod_nq$std.error[x], 
                n = mod_nq_n, 
                npar = nrow(mod_nq), 
                alpha = alpha,
                print_t = print_t)
        }) %>%
    dplyr::select(-statsig) %>%
    mutate(var = mod_nq$term[2:10],
           query_coefs = "without_query")
    
    if(length(modlist) == 5){
       if(model_type == "Negative Binomial"){
        mod_q <- modlist[[5]]$tidy %>%
             mutate(term = case_when(
               grepl("Strong D", term) ~ "Strong Democrat",
               grepl("Not very strong D", term) ~ "Not very strong Democrat",
               grepl("Lean D", term) ~ "Ind/Lean Democrat",
               grepl("Lean R", term) ~ "Ind/Lean Republican",
               grepl("Not very strong R", term) ~ "Not very strong Republican",
               grepl("Strong R", term) ~ "Strong Republican",
                T ~ term
           ))
        mod_q_n <- modlist[[5]]$glance$Num.Obs
    }else{
         mod_q <- data.frame(
            term = modlist[[5]]$tidy$term,
            estimate = modlist[[5]]$tidy$estimate,
            std.error = modlist[[5]]$tidy$std.error
        ) %>%
         mutate(term = case_when(
               grepl("Strong D", term) ~ "Strong Democrat",
               grepl("Not very strong D", term) ~ "Not very strong Democrat",
               grepl("Lean D", term) ~ "Ind/Lean Democrat",
               grepl("Lean R", term) ~ "Ind/Lean Republican",
               grepl("Not very strong R", term) ~ "Not very strong Republican",
               grepl("Strong R", term) ~ "Strong Republican",
                T ~ term
           ))
        mod_q_n <- modlist[[5]]$glance$Num.Obs
    }
    
    df_q <- map_df(2:10, function(x){
        get_confint(est = mod_q$estimate[x], 
                se = mod_q$std.error[x], 
                n = mod_q_n, 
                npar = nrow(mod_q), 
                alpha = alpha,
                print_t = print_t)
        }) %>%
        dplyr::select(-statsig) %>%
    mutate(var = mod_q$term[2:10],
          query_coefs = "with_query")
        
    rdf <- 
        bind_rows(df_nq, df_q) %>%
        pivot_wider(names_from = query_coefs,
                   values_from = c("lower","coef","upper", "p","t")) %>%
        mutate(outcome = outcome,
               model = model_type,
              year = year,
              behavior = behavior) %>%
        dplyr::select(var, behavior, outcome, year, model, ends_with("without_query"), ends_with("with_query"))
    }else{
    rdf <- df_nq %>%
        mutate(outcome = outcome,
               model = model_type,
              year = year,
              behavior = behavior) %>%
        rename(lower_without_query = lower,
              coef_without_query = coef,
              upper_without_query = upper,
              p_without_query = p,
              t_without_query = t) %>%
        dplyr::select(var, behavior, outcome, year, model, ends_with("without_query"))
    }
    
    rdf$var <- gsub("age_group", "Age Group: ", rdf$var)
    
    return(rdf %>%
          slice(c(7:9), 5,3,1,2,4,6))
}

# function to get n of group size for different outcomes by observation window
# function takes:
    # data: survey wave (corresponding with 2018 or 2020)
    # window var: variable defining days respondent was in observation window
    # party_var: variable name for party ID
    # age_var: variable name for age group
    # year: 2018 or 2020
    # behavior: search results, search follows, browser history
    # filter_vec: vector of variables in the model (to filter to complete cases)
# outputs a table with N by behavior, year, and group
party_age_n_func <- function(data, window_var, party_var, age_var,
                             filter_vec,
                            year, behavior){
    
    p <- 
    data %>%
    
    # filter on observation window
    filter(!is.na({{window_var}})) %>%
    
    # filter to complete cases
    dplyr::select({{filter_vec}}) %>%
    na.omit() %>%
    
    # group by party
    group_by({{party_var}}) %>%
    
    # summarise to n by group
    summarise(n = n())
    
    # rename variables to standardize
    names(p) <- c("Var","N")
    
    # standardize PID categories
    p$Var <- with(p, case_when(
               grepl("Strong D", Var) ~ "Strong Democrat",
               grepl("Not very strong D", Var) ~ "Not very strong Democrat",
               grepl("Lean D", Var) ~ "Ind/Lean Democrat",
               grepl("Lean R", Var) ~ "Ind/Lean Republican",
               grepl("Not very strong R", Var) ~ "Not very strong Republican",
               grepl("Strong R", Var) ~ "Strong Republican",
                T ~ "Independent/Other"
           ))
    
    a <- 
    data %>%
    
    # filter on observation window
    filter(!is.na({{window_var}})) %>%
    
    # filter to complete cases
    dplyr::select({{filter_vec}}) %>%
    na.omit() %>%
    
    # group by age group
    group_by({{age_var}}) %>%
    
    # summarise to n by group
    summarise(n = n())
    
    # rename variables to standardize
    names(a) <- c("Var","N")
    
    # standardize age group categories (add descriptor)
    a$Var <- paste0("Age Group: ", a$Var)
    
    # combine, add metadata (year, behavior), rename level name, and return output
    bind_rows(p, a) %>%
    mutate(Year = year,
           Behavior = behavior) %>%
    rename(Group = Var) %>%
    dplyr::select(Behavior, Year, Group, N)
}

# function for putting GAM output in format friendly for {modelsummary}
# function takes:
    # mod_gam: GAM model object
# outputs list of class modelsummary_list
gam_to_ms <- function(mod_gam){
    s <- summary(mod_gam)
    df <- data.frame(s$p.table)
    
    ti <- data.frame(
      term = rownames(df),
      estimate = df$Estimate,
      std.error = df$Std..Error,
      p.value = df$Pr...z..)

    gl <- data.frame(
      Num.Obs = as.integer(length(mod_gam$residuals)),
      `Degrees of Freedom` = as.integer(length(mod_gam$residuals) - nrow(ti)),
      Dev = round(mod_gam$deviance, 3),
      Null.Dev = round(mod_gam$null.deviance, 3),
      AIC = round(mod_gam$aic, 3))  
    
    mod <- list(
          tidy = ti,
          glance = gl)
    class(mod) <- "modelsummary_list"
    return(mod)
}

# function for modelsummary-ing gams
ols_to_ms <- function(mod_ols){
    s <- summary(mod_ols)
    df <- data.frame(s$coefficients)

    ti <- data.frame(
          term = rownames(df),
          estimate = df$Estimate,
          std.error = df$Std..Error,
          p.value = df$Pr...t..)

    gl <- data.frame(
          Num.Obs = as.integer(length(mod_ols$residuals)),
          `Degrees of Freedom` = as.integer(length(mod_ols$residuals) - nrow(ti)),
          R.Squared = round(s$r.squared, 3),
          Adj.R.Squared = round(s$adj.r.squared, 3),
          RMSE = sqrt(mean(mod_ols$residuals^2)))  
    
    mod <- list(
          tidy = ti,
          glance = gl)
    class(mod) <- "modelsummary_list"
    return(mod)
}