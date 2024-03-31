
# Looking at correlation between parameters and our outcomes

print_param <- function(simset){
  bh.ir <- simset$get(outcome = "incidence", 
                      dimension.values = list(year = '2035', 
                                              race = c("black","hispanic"),
                                              sex = "msm"),
                      keep.dimensions = character())/simset$get(
                        outcome = "population", 
                        dimension.values = list(year = '2035', 
                                                race = c("black","hispanic"), 
                                                sex = "msm"),
                        keep.dimensions = character())
  
  other.ir <- simset$get(outcome = "incidence", dimension.values = list(
    year = '2035', race = "other", 
    sex = "msm"),
    keep.dimensions = character())/simset$get(
      outcome = "population", 
      dimension.values = list(year = '2035', race = "other", 
                              sex = "msm"),
      keep.dimensions = character())
  
  irr <- bh.ir/other.ir
  
  corr.param <- apply(simset$parameters, 1, cor, irr)
  corr.param <- corr.param[!is.na(corr.param)]
  corr.param <- corr.param[order(abs(corr.param))]
  
  param.list <- names(corr.param[abs(corr.param) >= 0.5])
  param.list <- param.list[!grepl("idu", param.list)]
  
  return(param.list)
}

plot_correlation <- function(param.list, param.title, simset){  
  bh.ir <- simset$get(outcome = "incidence", 
                      dimension.values = list(year = '2035', 
                                              race = c("black","hispanic"),
                                              sex = "msm"),
                      keep.dimensions = character())/simset$get(
                        outcome = "population", 
                        dimension.values = list(year = '2035', 
                                                race = c("black","hispanic"), 
                                                sex = "msm"),
                        keep.dimensions = character())
  
  other.ir <- simset$get(outcome = "incidence", dimension.values = list(
    year = '2035', race = "other", 
    sex = "msm"),
    keep.dimensions = character())/simset$get(
      outcome = "population", 
      dimension.values = list(year = '2035', race = "other", 
                              sex = "msm"),
      keep.dimensions = character())
  
  irr <- bh.ir/other.ir
  
  # create a list of 10 ggplots of the parameters in param.list vs irr
  plot.list <- lapply(param.list, function(param){
    ggplot(data.frame(simset$parameters[param,], irr),
           aes(x = simset$parameters[param,], y = irr)) +
      geom_point() + 
      labs(x = param, y = "IRR", 
           title = param.title[param.list == param]) +
      theme_minimal()
  })
  
  # put the plots above in a grid
  cowplot::plot_grid(plotlist = plot.list, ncol = 2)
}

# Boxplot function -----
print_boxplot <- function(simset, city){
  
  ## top 25% quantile -----
  params <- as.data.frame(simset$parameters) 
  final_param <- print_param(simset)
  
  params <- params[final_param,]
  
  quart.3 <- apply(params, 1, quantile, probs = 0.75)
  
  top.25 <- data.frame(
    name = final_param,
    quart.3 = quart.3
  )
  
  print_irr <- function(simset){
    bh.ir <- simset$get(outcome = "incidence", 
                        dimension.values = list(year = '2035', 
                                                race = c("black","hispanic"),
                                                sex = "msm"),
                        keep.dimensions = character())/simset$get(
                          outcome = "population", 
                          dimension.values = list(year = '2035', 
                                                  race = c("black","hispanic"), 
                                                  sex = "msm"),
                          keep.dimensions = character())
    
    other.ir <- simset$get(outcome = "incidence", dimension.values = list(
      year = '2035', race = "other", 
      sex = "msm"),
      keep.dimensions = character())/simset$get(
        outcome = "population", 
        dimension.values = list(year = '2035', race = "other", 
                                sex = "msm"),
        keep.dimensions = character())
    
    irr <- bh.ir/other.ir
    
    return(irr)
  }
  
  param_list <- list()
  
  for(name in final_param){
    index <- which(params[name,] >= top.25[name,2])
    param_list[[name]]$index <- t(params[name, index])
    param_list[[name]]$irr <- cbind(print_irr(simset)[index])
    param_list[[name]]$quantile <- rep("Top 25%", length(index))
  }
  
  # convert param_list to a data frame
  param_list_top25 <- lapply(param_list, function(x) as.data.frame(x))
  
  ## bottom 25% quantile -----
  quart.1 <- apply(params, 1, quantile, probs = 0.25)
  
  bottom.25 <- data.frame(
    name = final_param,
    quart.1 = quart.1
  )
  
  param_list <- list()
  
  for(name in final_param){
    index <- which(params[name,] >= bottom.25[name,2])
    param_list[[name]]$index <- t(params[name, index])
    param_list[[name]]$irr <- cbind(print_irr(simset)[index])
    param_list[[name]]$quantile <- rep("Bottom 25%", length(index))
  }
  
  # convert param_list to a data frame
  param_list_bottom25 <- lapply(param_list, function(x) as.data.frame(x))
  
  ## plotting -----
  # plot boxplot
  ggplot() + 
    geom_boxplot(data = param_list_top25[[1]], 
                 aes(x = irr, y = final_param[1], 
                     group = final_param[1], fill = "Top 25%"),
                 orientation = "y") +
    geom_boxplot(data = param_list_bottom25[[1]],
                 aes(x = irr, y = final_param[1], 
                     group = final_param[1], fill = "Bottom 25%"),
                 orientation = "y", position = position_dodge(25)) +
    geom_boxplot(data = param_list_top25[[2]],
                 aes(x = irr, y = final_param[2], 
                     group = final_param[2], fill = "Top 25%"),
                 orientation = "y") +
    geom_boxplot(data = param_list_bottom25[[2]],
                 aes(x = irr, y = final_param[2], 
                     group = final_param[2], fill = "Bottom 25%"),
                 orientation = "y") +
    geom_boxplot(data = param_list_top25[[3]],
                 aes(x = irr, y = final_param[3], 
                     group = final_param[3], fill = "Top 25%"),
                 orientation = "y") +
    geom_boxplot(data = param_list_bottom25[[3]],
                 aes(x = irr, y = final_param[3], 
                     group = final_param[3], fill = "Bottom 25%"),
                 orientation = "y") +
    geom_boxplot(data = param_list_top25[[4]],
                 aes(x = irr, y = final_param[4],
                     group = final_param[4], fill = "Top 25%"),
                 orientation = "y") +
    geom_boxplot(data = param_list_bottom25[[4]],
                 aes(x = irr, y = final_param[4], 
                     group = final_param[4], fill = "Bottom 25%"),
                 orientation = "y") +
    geom_boxplot(data = param_list_top25[[5]],
                 aes(x = irr, y = final_param[5], 
                     group = final_param[5], fill = "Top 25%"),
                 orientation = "y") +
    geom_boxplot(data = param_list_bottom25[[5]],
                 aes(x = irr, y = final_param[5], 
                     group = final_param[5], fill = "Bottom 25%"),
                 orientation = "y") +
    geom_boxplot(data = param_list_top25[[6]],
                 aes(x = irr, y = final_param[6], 
                     group = final_param[6], fill = "Top 25%"),
                 orientation = "y") +
    geom_boxplot(data = param_list_bottom25[[6]],
                 aes(x = irr, y = final_param[6], 
                     group = final_param[6], fill = "Bottom 25%"),
                 orientation = "y") +
    geom_boxplot(data = param_list_top25[[7]],
                 aes(x = irr, y = final_param[7], 
                     group = final_param[7], fill = "Top 25%"),
                 orientation = "y") +
    geom_boxplot(data = param_list_bottom25[[7]],
                 aes(x = irr, y = final_param[7], 
                     group = final_param[7], fill = "Bottom 25%"),
                 orientation = "y") +
    labs(x = "Primary Outcome of IRR", y = "Parameter", 
         title = paste("IRR in high vs low quintiles of each parameter in ", city),
         fill = "Quantile of Parameter") + 
    scale_fill_manual(values = c("Top 25%" = "orange3", "Bottom 25%" = "turquoise4")) +
    facet_wrap(~quantile, dir = "v") +
    theme_minimal()
}


# Houston
load("../../files/simulations/ehe/init.transmission.ehe-100/C.26420/ehe_init.transmission.ehe-100_C.26420_prepu40p80msm.Rdata")

print(print_param(simset))

# in ascending order of correlation
param.title <- c(
  "MSM Peak transmission rate multiplier",
  "Hispanic MSM transmission rate in 2010",
  "Hispanic MSM transmission rate in 2000",
  "Black MSM transmission rate in 2010",
  "Black MSM transmission rate in 2000",
  "Hispanic MSM transmission rate in 2010",
  "Global transmission rate",
  "Other/Other sexual transmission rate",
  "Black MSM transmission rate in 2000",
  "Hispanic/Hispanic sexual transmission rate"
)

plot_correlation(print_param(simset), param.title, simset)

print_boxplot(simset, "Houston")

# Miami 

load("../../files/simulations/ehe/init.transmission.ehe-100/C.33100/ehe_init.transmission.ehe-100_C.33100_prepu40p80msm.Rdata")

print(print_param(simset))

param.title <- c(
  "Hispanic MSM transmission rate in 2010",
  "Black/Black sexual transmission rate",
  "Other/Other sexual transmission rate",
  "Hispanic/Hispanic sexual transmission rate",
  "Other heterosexual transmission rate in 2010",
  "Black MSM transmission rate in 2010",
  "MSM Peak transmission rate multiplier",
  "Hispanic MSM transmission rate in 2020",
  "Other heterosexual transmission rate in 2000",
  "Global transmission rate",
  "Other heterosexual transmission rate in 2020"
)

plot_correlation(print_param(simset), param.title, simset)

print_boxplot(simset, "Miami")


# Chicago

load("../../files/simulations/ehe/init.transmission.ehe-100/C.16980/ehe_init.transmission.ehe-100_C.16980_prepu40p80msm.Rdata")

print(print_param(simset))

param.title <- c(
  "MSM peak transmission rate multiplier",
  "Hispanic MSM transmission rate in 2020",
  "Black MSM transmission rate in 2010",
  "Black MSM transmission rate in 2000",
  "Hispanic MSM transmission rate in 2000",
  "Other/Other sexual transmission rate",
  "Hispanic/Hispanic sexual transmission rate"
)
plot_correlation(print_param(simset), param.title, simset)

print_boxplot(simset)

print_boxplot(simset, "Chicago")
