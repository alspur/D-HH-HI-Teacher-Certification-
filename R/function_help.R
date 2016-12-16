# function_help.R

# create function to convert chars to nums, handling "," in 1000+ numbers
char_to_num <- function(string = ""){
  as.numeric(str_replace_all(string, ",", ""))
}

# convert all colnames to lower

col_lower <- function(df){
  tolower(colnames(df))
}

# create function to convert percentages to numbers
pct_to_num <- function(string = ""){
  as.numeric(str_replace_all(string, "%", "")) / 100
}

# function to check if cert is active during a given school year
cert_during_sy <- function(effect_date = mdy("7-1-2000"), 
                           end_date = mdy("6-30-2016"), 
                           life_exp_cde = NA, 
                           school_year){
  
  if(!is.na(life_exp_cde) & life_exp_cde == "L"){
    return(1)
  }
  else{
    year_start <- lubridate::mdy(paste("7/1/", school_year -1, sep = ""))
    year_end <- lubridate::mdy(paste("6/30/", school_year, sep = ""))
  
    if(effect_date <= year_start & 
      end_date >= year_end){
     return(1)
    }
    else{
     return(0)
    }

  }
}


# test functions #####
cert_df <- function(df, school_year){
  
  year_start <- lubridate::mdy(paste("7/1/", school_year -1, sep = ""))
  year_end <- lubridate::mdy(paste("6/30/", school_year, sep = ""))
  
  mod_df <- df %>%
    mutate(year = school_year,
           active_cert = ifelse(!is.na(life_exp_cde), "Y",
                         ifelse(effect_date <= year_start &
                                  exp_date >= year_end, "y", "N")))
  
  return(mod_df)
}


cert_check <- function(effect_date, end_date, life_exp_cde){
  
  df <- data_frame(s_year = 2000:2016)

  full_df <- df %>%
    group_by(s_year) %>%
    mutate(had_cert = cert_during_sy(effect_date, end_date,
                                     life_exp_cde, s_year))
  
  return(full_df)
}


cert_checkr <- function(df){
  
  yr_df <- data_frame(s_year = 2000:2016)
  
  full_df <- yr_df %>%
    group_by(s_year) %>%
    mutate(had_cert = cert_during_sy(df$effect_date[1], df$end_date[1],
                                     df$life_exp_cde[1], s_year))
  
  return(full_df)
}
