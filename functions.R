# This file contains all the functions that are used in the main script

# Function for logit scale

logit <- function(p){
  log(p/(1-p))
}

# Function to evaluate coefficients from logit model
expit <- function(lo){
  exp(lo)/(1+exp(lo))
}

# Function to change age categories from AMIS reporting to CDC reporting

age_mutate <- function(df){
  df <- df |> 
    dplyr::mutate(
      age1 = age18.24,
      age2 = age25.29 + 0.5*age30.39,
      age3 = 0.5*age30.39 + 0.5*age40.49,
      age4 = age40.49 + 0.5*age50ge,
      age5 = age50ge
    )
  
  return(df)
}

# function to adjust age intervals for PrEP indication data
pi.age.mutate <- function(df){
  return(df |> 
           dplyr::mutate(
             age1 = age15.24,
             age2 = age25.29 + 0.5*age30.39,
             age3 = 0.5*age30.39 + 0.5*age40ge,
             age4 = age40ge,
             age5 = age40ge
           ))
}