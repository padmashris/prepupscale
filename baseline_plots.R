if(1==2){
  source("../jheem_analyses/applications/prep_upscale/prep_data.R")
  source("../jheem_analyses/applications/prep_upscale/prepindication_hetGon.R")
  source("prep_data.R")
  source("prepindication_hetGon.R")
}

library(ggplot2)
library(dplyr)

# PrEP Use ------

# Remove columns "risk" and "sexid"
msm.bigp.df2 <- select(msm.bigp.df, -c(risk, sexid)) 

levels(msm.bigp.df2$ageid) <- c("ALL", "13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years")
levels(msm.bigp.df2$raceid) <- c("ALL", "Black", "Hispanic", "Other")


msm.bigp.df2 <- msm.bigp.df2 %>%
  mutate(
    raceid = as.character(raceid),
    ageid = as.character(ageid),
    strata = ifelse(raceid == "ALL", ageid, raceid),
    is_race = ifelse(raceid != "ALL", "Race Groups", "Age Groups")
  ) %>% 
  filter(strata != "ALL")

# Plotting
msm_prepuse <- ggplot(msm.bigp.df2, aes(x = as.factor(year + anchor.year), y = p, color = strata, group = strata)) +
  geom_point() +
  geom_line() +
  facet_wrap(~is_race) +
  labs(title = "PrEP Use among MSM",
       x = "Year",
       y = "PrEP Use Proportion",
       color = "Legend") +
  # scale_color_manual(values = c("black", "red"), labels = c("Age", "Race")) +
  theme_minimal()

# Remove columns 'risk' and 'sexid'
nonmsm.big.df2 <- select(nonmsm.big.df, -c(risk, sexid))

# Rename levels for 'ageid' and 'raceid' columns
levels(nonmsm.big.df2$ageid) <- c("ALL", "13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years")
levels(nonmsm.big.df2$raceid) <- c("ALL", "Black", "Hispanic", "Other")

# Create 'strata' column based on 'raceid' and 'ageid'
nonmsm.big.df2 <- nonmsm.big.df2 %>%
  mutate(
    raceid = as.character(raceid),
    ageid = as.character(ageid),
    strata = ifelse(raceid == "ALL", ageid, raceid),
    is_race = ifelse(raceid != "ALL", "Race Groups", "Age Groups"),
    idu = ifelse(idu == 1, "PWID", "Heterosexual")
  ) %>%
  filter(strata != "ALL")

nonmsm_prepuse <- ggplot(nonmsm.big.df2, aes(x = as.factor(year + anchor.year), y = p, 
                           color = strata, group = strata)) +
  geom_point() +
  geom_line() +
  facet_wrap(~idu + is_race, scales = "free_y") +
  labs(title = "PrEP Use among non-MSM",
       x = "Year",
       y = "PrEP Use Proportion",
       color = "Legend") +
  theme_minimal()

cowplot::plot_grid( msm_prepuse, nonmsm_prepuse, nrow = 2)

# PrEP indication ------
msm.pi.df2 <- select(msm.pi.df, -c(riskid, sexid)) 

levels(msm.pi.df2$ageid) <- c("ALL", "13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years")
levels(msm.pi.df2$raceid) <- c("ALL", "Black", "Hispanic", "Other")


msm.pi.df2 <- msm.pi.df2 %>%
  mutate(
    raceid = as.character(raceid),
    ageid = as.character(ageid),
    strata = ifelse(raceid == "ALL", ageid, raceid),
    is_race = ifelse(raceid != "ALL", "Race Groups", "Age Groups"),
    dataid = ifelse(dataid == "cdc", "NHBS", "AMIS")
  ) %>% 
  filter(strata != "ALL")

# Plotting
msm_prepind <- ggplot(msm.pi.df2, aes(x = as.factor(years + anchor.year), y = pi,
                                      color = strata, group = strata)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ dataid + is_race) +
  labs(title = "PrEP Indication among MSM",
       x = "Year",
       y = "PrEP Indication Proportion",
       color = "Legend") +
  # scale_color_manual(values = c("black", "red"), labels = c("Age", "Race")) +
  theme_minimal()


# pi.big.df2 <- select(pi.big.df, -c(sexid))
pi.big.df2 <- pi.big.df %>% filter(riskid != "msm")

levels(pi.big.df2$ageid) <- c("ALL", "13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years")
levels(pi.big.df2$raceid) <- c("ALL", "Black", "Hispanic", "Other")


pi.big.df2 <- pi.big.df2 %>%
  mutate(
    raceid = as.character(raceid),
    ageid = as.character(ageid),
    strata = ifelse(raceid == "ALL", ageid, raceid),
    is_race = ifelse(raceid != "ALL", "Race Groups", "Age Groups"),
    dataid = ifelse(dataid == "cdc", "NHBS", "AMIS"),
    riskid = ifelse(riskid == "idu", "PWID", "Heterosexual")
  ) %>% 
  filter(strata != "ALL")

# Plotting
nonmsm_prepind <- ggplot(pi.big.df2, aes(x = as.factor(years + anchor.year), y = pi,
                                      color = strata, group = strata)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ riskid + is_race, ncol = 2, scales = "free_y") +
  labs(title = "PrEP Indication among non-MSM",
       x = "Year",
       y = "PrEP Indication Proportion",
       color = "Legend") +
  # scale_color_manual(values = c("black", "red"), labels = c("Age", "Race")) +
  theme_minimal()

cowplot::plot_grid(msm_prepind, nonmsm_prepind, nrow = 2)

# PrEP persistence ------


levels(pp.df$ageid) <- c("ALL", "13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years")
levels(pp.df$riskid) <- c("ALL", "Heterosexual", "MSM", "PWID")
levels(pp.df$raceid) <- c("ALL", "Black", "Hispanic", "Other")

pp.df <- pp.df %>% mutate(
  raceid = as.character(raceid),
  ageid = as.character(ageid),
  riskid = as.character(riskid),
  group = ifelse(raceid != "ALL", raceid, 
                 ifelse(ageid != "ALL", ageid, riskid)),
  strata = ifelse(raceid != "ALL", "race",
                  ifelse(ageid != "ALL", "age", 
                         ifelse(group=="total", "total", "risk"))),) %>%
  filter(group != "ALL")

pp.df$group <- factor(pp.df$group, levels = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years', 'Black', 'Hispanic', 'Other', 'Heterosexual', 'MSM', 'PWID'))

# Creating separate plots for each strata level
age_plot <- ggplot(subset(pp.df, strata == "age"), aes(x = group, y = pp, color = strata, group = strata)) +
  geom_point(position = position_dodge(width = 0.5)) +
  theme_minimal() +
  labs(
    title = "PrEP Persistence",
    x = "Age Group",
    y = "PrEP Persistence Proportion"
  ) +
  guides(color = "none") 

race_plot <- ggplot(subset(pp.df, strata == "race"), aes(x = group, y = pp, color = strata, group = strata)) +
  geom_point(position = position_dodge(width = 0.5)) +
  theme_minimal() +
  labs(
    x = "Race Group",
    y = "PrEP Persistence Proportion"
  ) +
  guides(color = "none")

risk_plot <- ggplot(subset(pp.df, strata == "risk"), aes(x = group, y = pp, color = strata, group = strata)) +
  geom_point(position = position_dodge(width = 0.5)) +
  theme_minimal() +
  labs(
    x = "Risk Group",
    y = "PrEP Persistence Proportion"
  ) +
  guides(color = "none")

# Arranging plots
cowplot::plot_grid(age_plot, race_plot, risk_plot, nrow = 3, align = 'v')
