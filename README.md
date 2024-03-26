# PrEP Upscale
This repository contains files used for my Master of Health Science in Epidemiology thesis completed in May 2024. Specifically, it contains data and code to compute the baseline regression models for PrEP use, indication and persistence.

Title: Evaluating the impact of increasing pre-exposure prophylaxis (PrEP) uptake and persistence among Black and Hispanic men who have sex with men on future reductions and racial disparities in HIV incidence in the United States

Advisors: Drs. Parastu Kasaie and Todd Fojo 

Model calibration repository: [github.com/tfojo1/jheem_analyses](https://github.com/tfojo1/jheem_analyses)

The PrEP input manager can be found [here](https://github.com/tfojo1/jheem_analyses/blob/master/input_managers/prep_input_manager.R).

A small snapshot of the data sources and models used:

## PrEP Use Model Formulation

| Subpopulation        | Model                                              | Scale/Type | Data Source                                         |
|----------------------|----------------------------------------------------|------------|-----------------------------------------------------|
| Men who have sex with men (MSM)                  | PrEP Use among MSM = β₀ + β₁ * year + β₂ * race + β₃ * age | Linear     | American Men’s Internet Survey 2017, 2018, 2019  |
| People who inject drugs (PWID)                 | PrEP Use among PWID = β₀ + β₁ * year + β₂ * race + β₃ * age + β₄ * female |  Linear | National HIV Behavioral Surveillance (NHBS) data from 2015, 2017 |
| Heterosexual Individuals (HET) | PrEP Use among HET = β₀ + β₁ * year + β₂ * race + β₃ * age + β₄ * female |  Linear | NHBS data from 2016, 2019 |

## PrEP indication Model Formulation 

| Subpopulation               | Model                                                                             | Scale/Type | Data Source                                                      |
|-----------------------------|-----------------------------------------------------------------------------------|------------|------------------------------------------------------------------|
| MSM                         | logit((PrEP indication among MSM)/(Maximum PrEP indication)) = β₀ + β₁ * year + β₂ * race + β₃ * age | Logit Scale | American Men’s Internet Survey 2017, 2018, 2019; NHBS 2014, 2017, 2021 |
| PWID                        | logit(PrEP indication among PWID) = β₀ + β₁ * year + β₂ * race + β₃ * age + β₄ * female  | Logit Scale  | NHBS 2009, 2012, 2015                                                    |
| HET | logit(PrEP indication among HET) = β₀ + β₁ * year + β₂ * race + β₃ * age + β₄ * female  | Logit Scale | NHBS 2010, 2013, 2016                                                   |

## PrEP persistence Model Formulation

| Subpopulation | Model                                         | Scale/Type | Data Source                            |
|---------------|-----------------------------------------------|------------|----------------------------------------|
| All           | PrEP Persistence = β₀ + β₁ * race + β₂ * risk + β₃ * age | Linear     | San Francisco Primary Care Clinics |

