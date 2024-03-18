# 3. Install required packages
install.packages(c("mice", "modelsummary"))
library(mice)
library(modelsummary)

# 4. Load data
wages <- read.csv(file.choose())


# 5. Drop observations with missing hgc or tenure
wages2 <- wages[complete.cases(wages[, c("hgc", "tenure")]), ]

# 6. Summary table of data
datasummary_skim(wages2) # Prints LaTeX table to console

# Calculate missing rate for logwage
missing_rate <- mean(is.na(wages2$logwage))
cat("The missing rate for logwage is:", missing_rate, "\n")
#Is this MCAR, MAR, or MNAR?
#I'm not sure. Scrolling through the missing values, it doesn't seem like any one of the other variables could explain it (e.g. there are plenty of missing values for high and low tenures, college eduction and no college education, etc.)
#That makes me think it's MCAR, but I'm not sure.

# 7. Imputation methods and regression models
# Complete cases
cc_model <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages2, na.action = na.omit)

# Mean imputation 
wages3 <- wages2
wages3$logwage[is.na(wages3$logwage)] <- mean(wages3$logwage, na.rm = TRUE)
mean_imp_model <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages3)

# Regression imputation
wages4 <- wages2
temp_model <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages4, na.action = na.omit)
wages4$logwage[is.na(wages4$logwage)] <- predict(temp_model, newdata = wages4[is.na(wages4$logwage),])
reg_imp_model <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages4)

# Multiple imputation 
wages5 <- wages2
imp_mod <- mice(wages5, printFlag = FALSE)
imp_data <- complete(imp_mod)
multi_imp_model <- with(imp_data, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))

# Regression table
mod <- list("Complete Cases" = cc_model, 
            "Mean Imputation" = mean_imp_model,
            "Regression Imputation" = reg_imp_model,
            "Multiple Imputation" = multi_imp_model)
regression_table <- modelsummary(mod, output = "latex")

# Print the regression table
cat(regression_table)

# 8. Project progress
# ... discuss project data and modeling approaches

# 9-11. Compile LaTeX, upload files
system("pdflatex PS7_LastName.tex")
# Transfer PDF and scripts to OSCER /DScourseS24/ProblemSets/PS7/
# Git add, commit, push to GitHub