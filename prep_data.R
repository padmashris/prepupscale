# PrEP Use ----------------------------

# Function to fit logistic model

logit <- function(p){
  log(p/(1-p))
}
expit <- function(lo){
  exp(lo)/(1+exp(lo))
}

# Function to change age categories to what we need

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

##  MSM -----
p.msm.2017 <- data.frame(
  total = 12.7,
  age18.24 = 5.5,
  age25.29 = 17.6,
  age30.39 = 21.5,
  age40.49 = 13.0,
  age50ge = 13.0, # ages 50 or greater
  black = 14.0,
  hisp = 10.9,
  nbnh = 13.1,
  white = 13.0
)


p.msm.2017 <- age_mutate(p.msm.2017)

p.msm.2017 <- p.msm.2017 / 100

### PrEP Use in 2018 among MSM (American Men's Internet Survey) -------
## https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2018-United-States-Report.pdf 

p.msm.2018 <- data.frame(
  total = 13.8,
  age18.24 = 6.3,
  age25.29 = 16.2,
  age30.39 = 22.3,
  age40.49 = 19.9,
  age50ge = 19.9, # ages 50 or greater
  black = 11.6,
  hisp = 12.0,
  nbnh = 14.4,
  white = 14.6
)

p.msm.2018 <- age_mutate(p.msm.2018)

p.msm.2018 <- p.msm.2018 / 100

### PrEP Use in 2019 (AMIS) ======
## https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2019-United-States-Report.pdf 
## not used - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8488229/ 

p.msm.2019 <- data.frame( 
  total = 15.2,
  age18.24 = 7.8,
  age25.29 = 21.3,
  age30.39 = 25.4,
  age40.49 = 18.9,
  age50ge = 18.9, # ages 50 or greater
  black = 23.0,
  hisp = 12.8,
  nbnh = 14.6,
  white = 14.5
)

p.msm.2019 <- age_mutate(p.msm.2019)
p.msm.2019 <- p.msm.2019 / 100

# PrEP Use in 2021 among MSM (CDC)
## https://www.cdc.gov/hiv/pdf/library/reports/cdc-hiv-surveillance-special-report-number-31.pdf

p.msm.2021 <- data.frame(
  total = 41.6,
  age18.24 = 39.7,
  age25.29 = 40.6,
  age30.39 = 46.2,
  age40.49 = 42.9,
  age50ge = 33.5, # ages 50 or greater
  black = 24.3,
  hisp = 44.1,
  nbnh = 54.8,
  white = 56.7
)

p.msm.2021 <- age_mutate(p.msm.2021)
p.msm.2021 <- p.msm.2021 / 100

p.msm.black <- c(
  p.msm.2017$black,
  p.msm.2018$black,
  p.msm.2019$black
  # p.msm.2021$black
)

p.msm.hisp <- c(
  p.msm.2017$hisp,
  p.msm.2018$hisp,
  p.msm.2019$hisp
  # p.msm.2021$hisp
)

p.msm.nbnh <- c(
  p.msm.2017$nbnh,
  p.msm.2018$nbnh,
  p.msm.2019$nbnh
  # p.msm.2021$nbnh
)

p.msm.age1 <- c(
  p.msm.2017$age1,
  p.msm.2018$age1,
  p.msm.2019$age1
  # p.msm.2021$age1
)

p.msm.age2 <- c(
  p.msm.2017$age2,
  p.msm.2018$age2,
  p.msm.2019$age2
  # p.msm.2021$age2
)

p.msm.age3 <- c(
  p.msm.2017$age3,
  p.msm.2018$age3,
  p.msm.2019$age3
  # p.msm.2021$age3
)

p.msm.age4 <- c(
  p.msm.2017$age4,
  p.msm.2018$age4,
  p.msm.2019$age4
  # p.msm.2021$age4
)

p.msm.age5 <- c(
  p.msm.2017$age5,
  p.msm.2018$age5,
  p.msm.2019$age5
  # p.msm.2021$age5
)

p.msm.total <- c(
  p.msm.2017$total,
  p.msm.2018$total,
  p.msm.2019$total
)


#### creating a combined model for msm -----
p.msm.df <- data.frame(
  year = c(2017:2019),
  black = p.msm.black,
  hisp = p.msm.hisp,
  nbnh = p.msm.nbnh,
  age1 = p.msm.age1,
  age2 = p.msm.age2,
  age3 = p.msm.age3,
  age4 = p.msm.age4,
  age5 = p.msm.age5,
  total = p.msm.total
)

# p.max and anchor year ----------
p.max <- 0.6
anchor.year <- 2009

library(tidyr)
p.msm.df.long <- pivot_longer(p.msm.df, cols = c(black, hisp, nbnh, age1, age2, age3, age4, age5, total),
                              names_to = "variable", values_to = "p")

p.msm.df.long$raceid <- ifelse(grepl("black", p.msm.df.long$variable), "black",
                               ifelse(grepl("hisp", p.msm.df.long$variable), "hispanic", 
                                      ifelse(grepl("nbnh", p.msm.df.long$variable), "other", "ALL")))
p.msm.df.long$ageid <- ifelse(grepl("age1", p.msm.df.long$variable), "age1",
                              ifelse(grepl("age2", p.msm.df.long$variable), "age2",
                                     ifelse(grepl("age3", p.msm.df.long$variable), "age3",
                                            ifelse(grepl("age4", p.msm.df.long$variable), "age4",
                                                   ifelse(grepl("age5", p.msm.df.long$variable), "age5", "ALL")))))
p.msm.df.long$risk <- rep("msm", length(p.msm.df.long$raceid))
p.msm.df.long$year <- p.msm.df.long$year - anchor.year

# fit.p.msm <- lm(logit(p.msm.df.long$p/p.max) ~ year + factor(raceid) + factor(ageid), 
#                 data = p.msm.df.long)

# years <- c(2017:2019,2021) - 2020
# p.max <- 0.6
# 
# # race
# fit.msm.black <- lm(logit(p.msm.black/p.max) ~ years)
# fit.msm.hisp <- lm(logit(p.msm.hisp/p.max) ~ years)
# fit.msm.nbnh <- lm(logit(p.msm.nbnh/p.max) ~ years)
# 
# #age 
# fit.msm.age1 <- lm(logit(p.msm.age1/p.max) ~ years)
# fit.msm.age2 <- lm(logit(p.msm.age2/p.max) ~ years)
# fit.msm.age3 <- lm(logit(p.msm.age3/p.max) ~ years)
# fit.msm.age4 <- lm(logit(p.msm.age4/p.max) ~ years)
# fit.msm.age5 <- lm(logit(p.msm.age5/p.max) ~ years)

## PWID ------

### PrEP Use in 2015 among PWID (NHBS) ----
## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-18.pdf

# numerator: # took PrEP
# denominator: # any receptive sharing

p.idu.2015 <- data.frame(
  total = 33/5867,
  male = 24/4169,
  female = 8/1677,
  age18.24 = 3/440,
  age25.29 = 6/837,
  age30.39 = 13/1588,
  age40.49 = 6/1285,
  age50ge = 5/1717, 
  black = 7/1586,
  hisp = 9/1320,
  nbnh = 17/(64+19+10+2620+240)
)

p.idu.2015 <- (age_mutate(p.idu.2015))

### PrEP Use in 2017 among PWID (NHBS) -------
## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-24.pdf

# numerator: # took PrEP
# denominator: # any receptive sharing

p.idu.2017 <- data.frame(
  total = 120/6350,
  male = 65/4287,
  female = 49/2015,
  age18.24 = 7/290,
  age25.29 = 13/812,
  age30.39 = 37/1922,
  age40.49 = 33/1448,
  age50ge = 30/1878, 
  black = 25/1698,
  hisp = 30/1255,
  nbnh = (3+1+53+8)/(77+20+5+2927+364)
)

p.idu.2017 <- age_mutate(p.idu.2017)

p.idu.black <- c(
  p.idu.2015$black,
  p.idu.2017$black
)

p.idu.hisp <- c(
  p.idu.2015$hisp,
  p.idu.2017$hisp
)

p.idu.nbnh <- c(
  p.idu.2015$nbnh,
  p.idu.2017$nbnh
)

p.idu.age1 <- c(
  p.idu.2015$age1,
  p.idu.2017$age1
)

p.idu.age2 <- c(
  p.idu.2015$age2,
  p.idu.2017$age2
)

p.idu.age3 <- c(
  p.idu.2015$age3,
  p.idu.2017$age3
)

p.idu.age4 <- c(
  p.idu.2015$age4,
  p.idu.2017$age4
)

p.idu.age5 <- c(
  p.idu.2015$age5,
  p.idu.2017$age5
)

p.idu.male <- c(
  p.idu.2015$male,
  p.idu.2017$male
)

p.idu.female <- c(
  p.idu.2015$female,
  p.idu.2017$female
)


#### big model for IDU -----

p.idu.df <- data.frame(
  year = c(2015,2017),
  p.idu.black,
  p.idu.hisp,
  p.idu.nbnh,
  p.idu.age1,
  p.idu.age2,
  p.idu.age3,
  p.idu.age4,
  p.idu.age5,
  p.idu.male,
  p.idu.female
)

p.idu.df.long <- gather(p.idu.df, key = "group", value = "p", -year)
p.idu.df.long$year <- p.idu.df.long$year - anchor.year

p.idu.df.long$raceid <- ifelse(p.idu.df.long$group == "p.idu.black", "black", 
                               ifelse(p.idu.df.long$group == "p.idu.hisp", "hispanic", 
                                      ifelse(p.idu.df.long$group == "p.idu.nbnh", "other", "ALL")))
p.idu.df.long$ageid <- ifelse(p.idu.df.long$group == "p.idu.age1", "age1", 
                              ifelse(p.idu.df.long$group == "p.idu.age2", "age2", 
                                     ifelse(p.idu.df.long$group == "p.idu.age3", "age3", 
                                            ifelse(p.idu.df.long$group == "p.idu.age4", "age4", 
                                                   ifelse(p.idu.df.long$group == "p.idu.age5", "age5", "ALL")))))
p.idu.df.long$sexid <- ifelse(p.idu.df.long$group == "p.idu.male", "male",
                              ifelse(p.idu.df.long$group == "p.idu.female", "female", "ALL"))


## Heterosexual -----
### PrEP Use in 2016 ----
## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-19.pdf

# denominator: any STI

p.het.2016 <- data.frame(
  total = 11/507,
  male = 2/176,
  female = 9/331,
  age18.24 = 2/173,
  age25.29 = 2/114,
  age30.39 = 2/86,
  age40.49 = 2/68,
  age50ge = 3/66, 
  black = 9/415,
  hisp = 1/55,  # there were 0 people who took PrEP this year among the hispanic ethnicity category. This threw an Inf error in the fit and so I've changed it to 1 for now.
  nbnh = 2/(4+3+1+8+20)
)

p.het.2016 <- (age_mutate(p.het.2016))


### PrEP Use in 2019 ----
## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-26.pdf

# denominator: any STI

p.het.2019 <- data.frame(
  total = 42/658,
  male = 18/218,
  female = 24/440,
  age18.24 = 5/216,
  age25.29 = 6/120,
  age30.39 = 14/143,
  age40.49 = 12/86,
  age50ge = 5/93, 
  black = 32/499,
  hisp = 4/102,
  nbnh = (1+2+3)/(6+2+1+14+34)
)

p.het.2019 <- (age_mutate(p.het.2019))

years.het <- c(2016,2019) - anchor.year

p.het.black <- c(
  p.het.2016$black,
  p.het.2019$black
)

p.het.hisp <- c(
  p.het.2016$hisp,
  p.het.2019$hisp
)

p.het.nbnh <- c(
  p.het.2016$nbnh,
  p.het.2019$nbnh
)

p.het.age1 <- c(
  p.het.2016$age1,
  p.het.2019$age1
)

p.het.age2 <- c(
  p.het.2016$age2,
  p.het.2019$age2
)

p.het.age3 <- c(
  p.het.2016$age3,
  p.het.2019$age3
)

p.het.age4 <- c(
  p.het.2016$age4,
  p.het.2019$age4
)

p.het.age5 <- c(
  p.het.2016$age5,
  p.het.2019$age5
)

p.het.male <- c(
  p.het.2016$male,
  p.het.2019$male
)

p.het.female <- c(
  p.het.2016$female,
  p.het.2019$female
)

p.het.df <- data.frame(
  years.het,
  p.het.black,
  p.het.hisp,
  p.het.nbnh,
  p.het.age1,
  p.het.age2,
  p.het.age3,
  p.het.age4,
  p.het.age5,
  p.het.male,
  p.het.female
)

#### het model -----
p.het.df.long <- gather(p.het.df, key = "group", value = "p", -years.het)
p.het.df.long$year <- p.het.df.long$years.het 
p.het.df.long <- p.het.df.long[,-1]

p.het.df.long$raceid <- ifelse(p.het.df.long$group == "p.het.black", "black", 
                               ifelse(p.het.df.long$group == "p.het.hisp", "hispanic", 
                                      ifelse(p.het.df.long$group == "p.het.nbnh", "other", "ALL")))
p.het.df.long$ageid <- ifelse(p.het.df.long$group == "p.het.age1", "age1", 
                              ifelse(p.het.df.long$group == "p.het.age2", "age2", 
                                     ifelse(p.het.df.long$group == "p.het.age3", "age3", 
                                            ifelse(p.het.df.long$group == "p.het.age4", "age4", 
                                                   ifelse(p.het.df.long$group == "p.het.age5", "age5", "ALL")))))
p.het.df.long$sexid <- ifelse(p.het.df.long$group == "p.het.female", "female",
                              ifelse(p.het.df.long$group == "p.het.male", "male", "ALL"))


# One big model (PrEP Use) ------

p.het.df.long <- p.het.df.long |> dplyr::select(-group) |> 
  dplyr::mutate(risk = rep("het", length(p.het.df.long$ageid)))
p.msm.df.long <- p.msm.df.long |> dplyr::select(-variable) |> 
  dplyr::mutate(sexid = rep("msm", length(p.msm.df.long$ageid)), 
                risk = rep("msm", length(p.msm.df.long$ageid)))
p.idu.df.long <- p.idu.df.long |> dplyr::select(-group) |> 
  dplyr::mutate(risk = rep("idu", length(p.idu.df.long$ageid)))

big.df <- rbind(p.idu.df.long, p.het.df.long, p.msm.df.long)

big.df$raceid <- relevel(factor(big.df$raceid), ref = "ALL")
big.df$ageid <- relevel(factor(big.df$ageid), ref = "ALL")
big.df$sexid <- relevel(factor(big.df$sexid), ref = "ALL")
big.df$risk <- relevel(factor(big.df$risk), ref = "msm")
big.df$sexid[big.df$sexid=="msm"] <- "male"

big.df$female <- as.numeric(big.df$sexid=="female")

big.df$nonmsm <- as.numeric(big.df$risk!="msm")
big.df$idu <- as.numeric(big.df$risk=="idu")


# making 2 separate big models -- nonmsm and msm -------

msm.bigp.df <- p.msm.df.long
msm.bigp.df$ageid <- relevel(factor(msm.bigp.df$ageid), ref = "ALL")
msm.bigp.df$raceid <- relevel(factor(msm.bigp.df$raceid), ref="ALL")
nonmsm.big.df <- subset(big.df, nonmsm == 1)

idu.big.df <- subset(big.df, risk == "idu")
het.big.df <- subset(big.df, risk == "het")

# fit.p.msm <- lm(logit(p/p.max) ~ year + raceid + ageid,
#                 data = msm.bigp.df)

fit.p.msm <- lm(p ~ year + raceid + ageid,
                data = msm.bigp.df)
fit.p.msm

fit.p.nonmsm <- lm(p ~ year + raceid + ageid + female + idu,
                   data = nonmsm.big.df)

fit.p.idu <- lm(p ~ year + raceid + ageid + female, data = idu.big.df)
fit.p.het <- lm(p ~ year + raceid + ageid + female, data = het.big.df)


# PrEP persistence ------

# [2012-2017 Persistence Data]; SF; 12 months of observation
# https://academic.oup.com/ofid/article/6/4/ofz101/5365426
# sample size - 364

pp.2012 <- c(
  total = 38.0,
  age1 = 30,
  age2 = 35,
  age3 = 35,
  age4 = 44,
  age5 = 60/134*100,
  black = 33,
  hispanic = 41,
  nbnh = (12+56+16)/(29+136+59)*100,
  het = 41,
  msm = 40,
  idu = 33
)

pp.2012 <- pp.2012/100
pp.2012

pp.df <- data.frame(pp = pp.2012)
pp.df$group <- rownames(pp.df)

pp.df$raceid <- ifelse(pp.df$group == "black", "black",
                       ifelse(pp.df$group == "hispanic", "hispanic",
                              ifelse(pp.df$group == "nbnh", "other", "ALL")))
pp.df$riskid <- ifelse(pp.df$group == "msm", "msm", 
                       ifelse(pp.df$group == "idu", "idu",
                              ifelse(pp.df$group == "het", "het", "ALL")))
pp.df$ageid <- ifelse(pp.df$group == "age1", "age1",
                      ifelse(pp.df$group == "age2", "age2",
                             ifelse(pp.df$group == "age3", "age3",
                                    ifelse(pp.df$group == "age4", "age4",
                                           ifelse(pp.df$group == "age5", "age5",
                                                  "ALL")))))

pp.df$ageid <- relevel(factor(pp.df$ageid), ref = "ALL")
pp.df$raceid <- relevel(factor(pp.df$raceid), ref = "ALL")
pp.df$riskid <- relevel(factor(pp.df$riskid), ref = "ALL")

fit.pp <- lm(pp ~ 1 + raceid + riskid + ageid, data = pp.df)
fit.pp
