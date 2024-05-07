# Load the necessary packages
library(RPostgres)
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(stargazer)
library(broom)
library(tidymodels)
library(glmnet)
library(recipes)
library(xtable)
library(tikzDevice)
library(ggplot2)

#Create a connection to WRDS (Wharton Research Data Service) with Compustat data
WRDS<- dbConnect(RPostgres::Postgres(),
                 host='wrds-pgdata.wharton.upenn.edu',
                 port=9737,dbname='wrds',
                 user=Sys.getenv("wrds_user"),
                 password=Sys.getenv("wrds_pass"))



#Get Financial data from Compustat Fund Annual
comp_funda <- dbSendQuery(WRDS,
                          "select gvkey, cusip, fyear, fyr, sich, tic, at, sale, 
                          txfo, txdfo, datadate, dltt, xrd, ppent, txbcof, tlcf
                   from comp.funda
                   where ((datadate between '1999-01-01' and '2012-12-31')
                      and (fyear between '2000' and '2011')
                      and (indfmt = 'INDL')
                      and (datafmt = 'STD')
                      and (indfmt = 'INDL')
                      and (consol = 'C')
                      and (popsrc = 'D')
                          and (gvkey IS NOT NULL)
                          and (fyear IS NOT NULL)
                          and (at IS NOT NULL)
                          and (sale IS NOT NULL)
                          and (dltt IS NOT NULL)
                          and (at >= 250)
                      )
                   ")
funda <- dbFetch(comp_funda)
# gvkey is the firm identifier in Compustat
# CUSIP is another identifier, which identifies the primary security (likely the common stock) of the firm
# fyear is the fiscal year
# fyr is the month of the fiscal year-end
# sich is the historical SIC code
# ni is net income
# tic is the ticker symbol
# ib is income before extraordinary items
# oancf is operating cash flow (operating activities net cash flow)
# at is total assets (assets total)
# sale is total operating revenue
# prcc_f is the closing stock price of the fiscal year
# csho is the number of common shares outstanding
# salefo is Foreign Sales
# txfo is Foreign Tax
# txdfo is Deferred Foreign Tax

# datadate is a year variable formatted as a date
# datafmt, indfmt, consol, and popsrc are flags that indicate the data format, industry format, consolidation, and population source
# (these just make sure our data is structured to compare apples to apples, like making sure the data is in the same currency)



#Get Segment data from Compustat Segments Historical
comp_wrds_segdtail <- dbSendQuery(WRDS,
                                  "select gvkey, sid, datadate, stype, srcdate,
                                    sales, snms
                            from compseg.wrds_segmerged
                            where ((datadate between '2000-01-01' and '2020-01-01')
                              and (srcdate = datadate)
                              )
                            ")
segdtail <- dbFetch(comp_wrds_segdtail)


# Group by gvkey, datadate, and stype
# Count how many unique values there are for BUSSEG and GEOSEG
# Turn those into separate columns (if no BUSSEG or GEOSEG, set the value to 1)
# Formatting to make it look pretty
segdtail2 <- segdtail %>%
  group_by(gvkey, datadate, stype) %>%
  summarise(sid_count = n_distinct(sid)) %>%
  pivot_wider(names_from = stype, values_from = sid_count, values_fill = 1) %>%
  ungroup()


# Add the domestic sales amount from Compustat Segments so that we can calculate the foreign sales amount in the next step
#Filters on North America and United States
segdtail3 <- segdtail %>%
  filter(snms %in% c("United States", "North America")) %>%
  select(gvkey, datadate, sales, snms)
#supposedly removes North America if there's a United States
segdtail3 <- segdtail3 %>%
  group_by(gvkey, datadate) %>%
  filter(!(any(snms == "North America") & any(snms == "United States"))) %>%
  ungroup()
#Renames sales to include segments (to avoid confusion with funda sales)
segdtail3 <- segdtail3 %>%
  rename(sales_segments = sales)
#Combines into one happy family
segdtail4 <- left_join(segdtail2, segdtail3, by = c("gvkey", "datadate"))



# Merge funda with segdtail2 on gvkey and datadate
firmyears <- merge(funda, segdtail4, by = c("gvkey", "datadate"))
# Calculate the foreign sales amount now and also the combined foreign tax amount
firmyears$ForeignSales <- ifelse(is.na(firmyears$sale - firmyears$sales_segments), 0, firmyears$sale - firmyears$sales_segments)
firmyears$ForeignTax <- ifelse(is.na(firmyears$txdfo + firmyears$txfo), 0, firmyears$txdfo + firmyears$txfo)

firmyears = subset(firmyears, !is.na(snms))

# Create lag and lead values
firmyears <- firmyears %>%
  arrange(gvkey, fyear) %>%
  group_by(gvkey) %>%
  mutate(lag_at = lag(at),
         lead_at = lead(at),
         lag_sale = lag(sale),
         lead_sale = lead(sale),
         lag_dltt = lag(dltt),
         lead_dltt = lead(dltt)) %>%
  ungroup()

frequency_table <- table(firmyears$fyear)
print(frequency_table)

# Calculate the various CIC "points" for each firm-year
# 1. Total assets
firmyears$AssetPoints <- ifelse(firmyears$at < 500, 1,
                         ifelse(firmyears$at <= 1000, 2,
                         ifelse(firmyears$at <= 2000, 3,
                         ifelse(firmyears$at <= 5000, 4,
                         ifelse(firmyears$at <= 8000, 5,
                         ifelse(firmyears$at <= 11000, 6,
                         ifelse(firmyears$at <= 14000, 7,
                         ifelse(firmyears$at <= 17000, 8,
                         ifelse(firmyears$at <= 20000, 9,
                         ifelse(firmyears$at <= 23000, 10,
                         ifelse(firmyears$at <= 26000, 11, 12
                       )))))))))))

# 2. Gross Receipts
firmyears$GrossReceiptsPoints <- ifelse(firmyears$sale < 1000, 1,
                                 ifelse(firmyears$sale <= 2000, 2,
                                 ifelse(firmyears$sale <= 3000, 3,
                                 ifelse(firmyears$sale <= 5000, 4,
                                 ifelse(firmyears$sale <= 10000, 5,
                                 ifelse(firmyears$sale <= 13000, 6,
                                 ifelse(firmyears$sale <= 16000, 7,
                                 ifelse(firmyears$sale <= 19000, 8,
                                 ifelse(firmyears$sale <= 22000, 9,10
                         )))))))))

# 3. Operating Entities
firmyears$GeoSegPoints <- ifelse(firmyears$GEOSEG <= 1, 1,
                          ifelse(firmyears$GEOSEG <= 5, 3,
                          ifelse(firmyears$GEOSEG <= 9, 5,
                          ifelse(firmyears$GEOSEG <= 13, 7, 9
                         ))))

# 4. Multiple Industry Status
firmyears$BusSegPoints <- ifelse(firmyears$BUSSEG <= 1, 1,
                          ifelse(firmyears$BUSSEG <= 5, 3,
                          ifelse(firmyears$BUSSEG <= 9, 5,
                          ifelse(firmyears$BUSSEG <= 13, 7, 9
                         ))))

# 5. Total Foreign Assets
firmyears$ForeignSalesPoints <- ifelse(firmyears$ForeignSales <= 500, 1,
                                ifelse(firmyears$ForeignSales <= 1000, 2,
                                ifelse(firmyears$ForeignSales <= 1500, 3,
                                ifelse(firmyears$ForeignSales <= 2500, 4,
                                ifelse(firmyears$ForeignSales <= 5000, 5,
                                ifelse(firmyears$ForeignSales <= 6500, 6,
                                ifelse(firmyears$ForeignSales <= 8000, 7, 8
                           )))))))

# 6. Total Related Transactions - No publicly available data

# 7. Foreign Tax
firmyears$ForeignTaxPoints <- ifelse(firmyears$ForeignTax < 7, 1,
                              ifelse(firmyears$ForeignTax <= 100, 2,
                              ifelse(firmyears$ForeignTax <= 200, 3,
                              ifelse(firmyears$ForeignTax <= 400, 4,
                              ifelse(firmyears$ForeignTax <= 600, 5,
                              ifelse(firmyears$ForeignTax <= 800, 6,
                              ifelse(firmyears$ForeignTax <= 1000, 7, 8
                           )))))))


# Calculate the total points and create a CICFirm variable
firmyears$TotalPoints <- firmyears$AssetPoints + firmyears$GrossReceiptsPoints +
  firmyears$GeoSegPoints + firmyears$BusSegPoints +
  firmyears$ForeignSalesPoints + firmyears$ForeignTaxPoints

firmyears$CICFirm = ifelse(firmyears$TotalPoints >= 12, 1, 0)


#Calculate other variables of interest
firmyears$Leverage <- firmyears$dltt / firmyears$at
firmyears$RnD <- ifelse(is.na(firmyears$xrd / firmyears$sale), 0, firmyears$xrd / firmyears$sale)
firmyears$CapInt <- firmyears$ppent / firmyears$at
firmyears$ExcessStockBen <- ifelse(ifelse(is.na(firmyears$txbcof), 0, firmyears$txbcof)>0, 1, 0)
firmyears$NOL <- ifelse(ifelse(is.na(firmyears$tlcf), 0, firmyears$tlcf)>0, 1, 0)


frequency_table2 <- table(subset(firmyears, CICFirm ==1)$fyear)
print(frequency_table2)

firmyearsPOST2007 = firmyears[firmyears$fyear >= 2007,]

numeric_vars <- sapply(firmyearsPOST2007, is.numeric)

# These lines split the sample between CIC firm-years and non-CIC firm-years
#firmyearsPOST2007CIC = subset(firmyearsPOST2007, CICFirm == 1)
#firmyearsPOST2007NONCIC = subset(firmyearsPOST2007, CICFirm == 0)

# Create a data frame to store summary statistics
summary_stats <- data.frame(
  Variable = names(firmyearsPOST2007)[numeric_vars],
  Count = sapply(firmyearsPOST2007[numeric_vars], length),
  Median = round(sapply(firmyearsPOST2007[numeric_vars], median), 3),
  Mean = round(sapply(firmyearsPOST2007[numeric_vars], mean), 3),
  SD = round(sapply(firmyearsPOST2007[numeric_vars], sd), 3),
  Min = round(sapply(firmyearsPOST2007[numeric_vars], min), 3),
  Max = round(sapply(firmyearsPOST2007[numeric_vars], max), 3)
)
summary_statsCIC <- data.frame(
  Variable = names(firmyearsPOST2007CIC)[numeric_vars],
  Count = sapply(firmyearsPOST2007CIC[numeric_vars], length),
  Median = round(sapply(firmyearsPOST2007CIC[numeric_vars], median), 3),
  Mean = round(sapply(firmyearsPOST2007CIC[numeric_vars], mean), 3),
  SD = round(sapply(firmyearsPOST2007CIC[numeric_vars], sd), 3),
  Min = round(sapply(firmyearsPOST2007CIC[numeric_vars], min), 3),
  Max = round(sapply(firmyearsPOST2007CIC[numeric_vars], max), 3)
)
summary_statsNONCIC <- data.frame(
  Variable = names(firmyearsPOST2007NONCIC)[numeric_vars],
  Count = sapply(firmyearsPOST2007NONCIC[numeric_vars], length),
  Median = round(sapply(firmyearsPOST2007NONCIC[numeric_vars], median), 3),
  Mean = round(sapply(firmyearsPOST2007NONCIC[numeric_vars], mean), 3),
  SD = round(sapply(firmyearsPOST2007NONCIC[numeric_vars], sd), 3),
  Min = round(sapply(firmyearsPOST2007NONCIC[numeric_vars], min), 3),
  Max = round(sapply(firmyearsPOST2007NONCIC[numeric_vars], max), 3)
)

xtable1 = xtable(summary_stats, digits = 3)
print(xtable1, include.rownames = FALSE, sanitize.text.function = function(x) x)

# now code the models
model1 = glm(CICFirm ~ at + sale + GEOSEG + BUSSEG + ForeignSales + ForeignTax ,
             family = binomial(link = "logit"), data = firmyearsPOST2007)
xtable2 = xtable(model1, digits = 3)
print(xtable2, include.rownames = FALSE, sanitize.text.function = function(x) x)
summary(model1)

model2 = glm(CICFirm ~ at + sale + GEOSEG + BUSSEG + ForeignSales + ForeignTax + Leverage + CapInt + ExcessStockBen + NOL , 
             family = binomial(link = "logit"), data = firmyearsPOST2007)
xtable3 = xtable(model2, digits = 3)
print(xtable3, include.rownames = FALSE, sanitize.text.function = function(x) x)
summary(model2)


# And create a ggplot for comparing CIC and non CIC firm-year observations
firmyearsPOST2007 <- subset(firmyears, fyear >= 2007)

# Create a data frame with counts of observations by year and CICFirm status
observation_counts <- data.frame(
  Year = firmyearsPOST2007$fyear,
  CIC_Status = ifelse(firmyearsPOST2007$CICFirm == 1, "CIC", "Non-CIC")
)

# Create the bar plot
ggplot(observation_counts, aes(x = Year, fill = CIC_Status)) +
  geom_bar(position = "dodge") +
  labs(title = "Observations in CIC Program vs Non-CIC by Year",
       x = "Year",
       y = "Count") +
  theme_minimal()


#I was going to explore the below some more but ran out of time to analyze the results and figure out what they mean. I'm leaving this code here for future reference but it is not included in my analaysis.
######################
# tidymodels time!
######################

# Split the data into training and testing sets
#firm_split <- initial_split(firmyearsPOST2007, prop = 0.8)
#firm_train <- training(firm_split)
#firm_test <- testing(firm_split)

#####################
# Logistic Regression
#####################

#print('Starting LOGIT')

# Set up the task and the engine
#tune_logit_spec <- logistic_reg(
#  penalty = tune(), # Tuning parameter
#  mixture = 1 # 1 = lasso, 0 = ridge
#) %>%
#  set_engine("glmnet") %>%
#  set_mode("classification")

# Define a grid over which to try different values of the regularization parameter lambda
#lambda_grid <- grid_regular(penalty(), levels = 50)

# 3-fold cross-validation
#logit_folds <- vfold_cv(firm_train, v = 3)


# Create a recipe to mutate CICFirm to a factor and impute missing values
#firm_recipe <- recipe(CICFirm ~ AssetPoints + GrossReceiptsPoints + GeoSegPoints + 
#                        BusSegPoints + ForeignSalesPoints + ForeignTaxPoints +
#                        Leverage + CapInt + ExcessStockBen + NOL, 
#                      data = firm_train) %>%
#  step_mutate(CICFirm = as.factor(CICFirm)) %>%
#  step_impute_mean(all_predictors()) # Impute missing values with mean

# Workflow
#logit_wf <- workflow() %>%
#  add_recipe(firm_recipe) %>%
#  add_model(tune_logit_spec)

# Tuning results
#logit_res <- logit_wf %>%
#  tune_grid(
#    resamples = logit_folds,
#    grid = lambda_grid
#  )

# Select the best value of lambda
#top_logit <- show_best(logit_res, metric = "accuracy")
#best_logit <- select_best(logit_res, metric = "accuracy")
#final_logit <- finalize_workflow(logit_wf, best_logit)


#print('*********** LOGISTIC REGRESSION **************')
#logit_test <- last_fit(final_logit, firm_split) %>%
#  collect_metrics()

#logit_test %>% print(n = 1)
#top_logit %>% print(n = 1)

# Combine results into a nice tibble
#logit_ans <- top_logit %>% slice(1)
#logit_ans %<>% left_join(logit_test %>% slice(1), by = c(".metric", ".estimator")) %>%
#  mutate(alg = "logit") %>% select(-starts_with(".config"))

# Fit the final model on the whole training data
#final_logit_fit <- fit(final_logit, data = firm_train)

# Extract the model coefficients and other information
#logit_tidy <- tidy(final_logit_fit$fit$fit)
#logit_tidy

