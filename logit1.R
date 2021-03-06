## setwd ----
setwd("C:\\Users\\John Karuitha\\OneDrive - University of Witwatersrand\\Karuitha and Ojah Data\\THESIS\\Dissertation\\Objective 1 Binary\\Chapters in Progress\\Objective2WhyMFIsConvert")
save.image("C:/Users/John Karuitha/OneDrive - University of Witwatersrand/Karuitha and Ojah Data/THESIS/Dissertation/Objective 1 Binary/Chapters in Progress/Objective2WhyMFIsConvert/data.RData")

## Load dataset ----
data <- read.csv("amelia.csv")

## load required packages ----
library(tidyverse)
library(plm)
library(pglm)
library(GGally)
library(ggthemes)
library(Amelia)
library(caret)
library(gridExtra)
library(skimr)
library(survival)

## Do feature engineering ----
## create variable for asset structure by logging 
data$asset_structure_log <- log(data$asset_structure)
data$legal_dummy <- ifelse(data$currentlegalstatus == "NGO", "NGO", "Other")
data$legal_dummy <- factor(data$legal_dummy, levels = c("NGO", "Other"))
data$legal_tradition <- factor(data$legal_tradition, 
                               levels = c("Common", "Civil", "Other"))
data$age <- factor(data$age, levels = c("Mature", "Young", "New"))
data$currentlegalstatus <- factor(data$currentlegalstatus, 
                          levels = c("NGO", "Bank", "NBFI", 
                          "Credit Union/ Cooperative", "Rural Bank"))

## take first difference of financial development 
data$d_fdev <- data$fdev - dplyr::lag(data$fdev)

## map out required variables----
##age legal_tradition size asset_structure findev KKM Educ GDP_growth
data <- data %>% relocate(lassets, asset_structure, fdev,
                          kkm, education, gdp_growth_annual, d_fdev, 
                          asset_structure_log, legal_dummy,
                          .after = legal_tradition)

## define median function ----
median_n <- function(x){median(x, na.rm = TRUE)}

## Visualize the data ----
data %>% select(lassets, asset_structure, 
                fdev, kkm, education, 
                gdp_growth_annual) %>% 
  ggpairs(columnLabels = c("Size (Lassets)", "Asset Structure", "FDEV", 
                           "Institutions (KKM)", "Education", "GDP Growth"), 
          lower = list(continuous = wrap("points", alpha = 0.2, 
                                         color = "blue"), 
                       combo = wrap("dot_no_facet", alpha = 0.1, size = 0.1))) + 
  theme_wsj(base_size = 8) 

## Current legal status vs age
a11 <- data %>% ggplot(aes(x = currentlegalstatus, fill = age)) + geom_bar() +
  theme_wsj(base_size = 8) + 
  labs(y = "Age", title = "MFIs Legal Status vs Age") +
  theme(legend.title = element_blank())

## Current legal status vs legal tradition 
a12 <- data %>% ggplot(aes(x = currentlegalstatus, fill = legal_tradition)) + 
  geom_bar() + theme_wsj(base_size = 8) + 
  labs(y = "Asset Structure", title = "MFIs Legal Status vs Legal Tradition") +
  theme(legend.title = element_blank())

## Current legal status vs size (lassets)
a13 <- data %>% ggplot(aes(x = reorder(currentlegalstatus, lassets, median_n), 
            y = lassets, fill = currentlegalstatus)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count") + 
  theme_wsj(base_size = 8) + theme(legend.position = "none") +
  stat_summary(fun = mean, geom = "point", size = 1, color = "red") +
  labs(y = "Size (lassets)", title = "MFIs Legal Status vs Size")

## Current legal status vs asset structure
a14 <- data %>% ggplot(aes(x = reorder(currentlegalstatus, 
                                       
        asset_structure_log, median_n), y = asset_structure_log, 
        
        fill = currentlegalstatus)) + 
  
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count") + 
  
  theme_wsj(base_size = 8) + theme(legend.position = "none") +
  
  stat_summary(fun = mean, geom = "point", size = 1, color = "red") +
  
  labs(y = "Asset Structure", title = "MFIs Legal Status vs Asset Structure")

## Current legal status vs financial development

a15 <- data %>% 
  
  ggplot(aes(x = fdev, y = as.numeric(currentlegalstatus), col = currentlegalstatus)) + 
  
  geom_point() + theme(legend.position = "bottom") + 
  
  scale_y_log10() + scale_x_log10() + 
  
  labs(x = "Financial Development", y = "Count")

a15
  
## Current legal status vs kkm
a16 <- data %>% 
  
  ggplot(aes(x = kkm, y = ..count..)) + 
  
  geom_histogram(alpha = 0.5, color = "black") + 
  
  theme(legend.position = "bottom") + 
  
  scale_y_log10() + scale_x_log10() + 
  
  labs(x = "Institutional Quality (KKM)", y = "Count") + 
  
  ggthemes::theme_economist() + 
  
  facet_wrap(~ currentlegalstatus)


a16

## Current legal status vs education
a17 <- data %>% ggplot(aes(x = reorder(currentlegalstatus, 
          education, median_n), y = education, 
          fill = currentlegalstatus)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count") + 
  theme_wsj(base_size = 8) +
          theme(legend.position = "none") + 
  stat_summary(fun = mean, 
               geom = "point", size = 1, color = "red") + 
  labs(y = "Education", title = "MFIs Legal Status vs Education")

## Current legal status vs gdp growth
a18 <- data %>% ggplot(aes(x = reorder(currentlegalstatus, 
        gdp_growth_annual, median_n), y = gdp_growth_annual, 
        fill = currentlegalstatus)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count") + 
  theme_wsj(base_size = 8)+ theme(legend.position = "none") + 
  stat_summary(fun = mean, 
               geom = "point", size = 1, color = "red") +
  labs(y = "Annual GDP Growth", title = "MFIs Legal Status vs GDP Growth")

## Combine the graphs into one ----
grid.arrange(a11, a12, a13, a14, nrow = 2)
grid.arrange(a15, a16, a17, a18, nrow = 2)

## Descriptive statistics -----
data %>% select(currentlegalstatus, age, 
                lassets, asset_structure, 
                fdev, kkm, education, 
                gdp_growth_annual) %>% skim() %>% 
                write.csv(., "descriptive.csv")

data %>% select(lassets, asset_structure, 
                fdev, kkm, education, 
                gdp_growth_annual) %>% 
                na.omit() %>% 
                cor() %>% round(4) %>% 
                write.csv(., "correlation.csv")

## Run regression analysis ----
## Create a regression function in pglm ----
data_pglm <- data %>% na.omit()

model_full_data <- pglm(legal_dummy ~ age + lassets + 
    asset_structure + fdev + kkm + education + 
    gdp_growth_annual + factor(year), data = data_pglm, na.action = na.omit, 
    effect = c("twoways"), model = c("random"), 
    family = binomial("logit"), 
    index = c("mfiid", "year"), 
    R = 5, print.level = 5,
    method = 'bfgs') 

summary(model_full_data$hessian)

##############################################################################
## create a pldv function ----
function_pldv <- function(data, depvar, formula, na.action = na.rm,
                          model = c("fd", "random", "pooling"),
                          index = c("mfiid", "year"), R = 20,
                          start = NULL, lower = 0,
                          upper = 1, objfun = c("lsq", "lad"),
                          sample = c("cens", "trunc")){
  
  ## Load libraries
  library(plm)
  library(broom)
  library(pcse)
  library(car)
  library(zoo)
  library(lmtest)
  library(broom)
  library(stargazer)
  
  ## match arguments
  model <- match.arg(model)
  objfun <- match.arg(objfun)
  sample <- match.arg(sample)
  
  ## Run the Model
  unadjusted <- pldv(depvar ~ formula, 
                    data = data,
                    model = model, 
                    index = index, 
                    digits = digits, 
                    objfun = objfun, 
                    sample = sample)
  
  ## Output results
  list(broom::glance(unadjusted),
       
       ## Correct standard errors for heteroscedasticity 
       ## And cross-sectional dependence
       
       coeftest(unadjusted, vcov. = function(x) {
         vcovBK(x, method = "arellano", type="HC1", cluster = "group")
       }))
}



data <- data %>% mutate(legal_dummy = case_when(
  currentlegalstatus == "NGO" ~ 0,
  TRUE ~ 1
))

data$legal_dummy <- factor(data$legal_dummy, levels = c(0, 1))


function_pldv(data = data, depvar = legal_dummy, 
              formula = factor(age) + lassets + 
                asset_structure + fdev + 
                kkm + education + 
                gdp_growth_annual)

data %>% na.omit() %>% 
pldv(legal_dummy ~ age + lassets + 
       asset_structure + fdev + 
       kkm + education + 
       gdp_growth_annual + factor(year), data = ., 
     model = "random", index = c("mfiid", "year"),
     sample = "cens")


general_glm <- glm(legal_dummy ~ factor(age) + lassets + 
     asset_structure + fdev + 
     kkm + education + 
     gdp_growth_annual + 
       factor(year) + factor(mfiid), 
     data = data, family = "binomial")

summary(general_glm)
