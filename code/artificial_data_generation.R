####################################################################
# Paper Code - Artificial data set
# Yevhen Havrylenko and Julia Heger 2022
####################################################################

# set your working directory via setwd('...')

# use some meaningful names for numerical and categorical variables
num.features <- c("pol_duration", "drv_age", "vh_age", "vh_cyl", 
                  "vh_speed", "vh_sale_begin", 'vh_value', "drv_age_lic")

cat.features <- c("drv_sex", "vh_make")
# in the final version of our paper, we used x_1, x_2, ..., x_10 as the generic names of variables for a concise exposition of our results
# the artificially generated values are the same, just names of variables are generic

# number of observations
n <- 2000000

# define a diagonal matrix
Sigma <- diag(8)

# define a correlation between drv_age & drv_age_lic
Sigma[2,8] <- Sigma[8,2] <- 0.5

# Simulate X ~ N(0,Sigma)
set.seed(123)
X <- mvrnorm(n = n, mu = rep(0, ncol(Sigma)), Sigma = Sigma);
colnames(X) <- c(num.features)
X <- data.frame(X);

# categorical drv_sex feature with 3 levels (M, F, D)
X[,"drv_sex"] <- rbinom(n, 2, 0.3);
# 
# # categorical vh_make feature with 6 levels (1,2,3,4,5,6)
X[,"vh_make"] <- rbinom(n, 5, 0.2);



# create mu, as in Mario Wuethrich's paper
mu <- -3 + 0.5*X[,1] - 0.25*X[,2]^2 + 0.5*abs(X[,3])*sin(2*X[,3]) + 0.5*X[,4]*X[,5] + 
  0.125*X[,5]^2*X[,6] - 0.1 * (X[,"drv_sex"] == 1) - 0.2 * (X[,"drv_sex"] == 2) +
  0.1 * (X[,"vh_make"] == 1) + 0.2 * (X[,"vh_make"] == 2) + 0.3 * (X[,"vh_make"] == 3) +
  0.4 * (X[,"vh_make"] == 4) + 0.5 * (X[,"vh_make"] == 5)

mu_exp <- exp(mu)

# otherwise, we can have extremely large mu_exp and thus Y
mu_exp[mu_exp > 1] = 1


# simulate Y ~ Poi(mu)
Y <- rpois(n = n, lambda = mu_exp);
distribution_table <- as.data.frame(table(Y));
distribution_table$Prop <- distribution_table$Freq / n * 100;
distribution_table
summary(Y)
hist(Y)


#Weighted Average Observed Frequency WAOF = sum(Y*exposure)/sum(exposure)
WAOF <- sum(Y)/n    # 0.060513   

# Y    Freq     Prop
# 1 0 1887159 94.35795
# 2 1  105560  5.27800
# 3 2    6516  0.32580
# 4 3     645  0.03225
# 5 4     103  0.00515
# 6 5      15  0.00075
# 7 6       2  0.00010


# summarize data in data frame
artificial_data <- data.frame(claim_total_nb = Y, annual_exposure = rep(1,n), X)
artificial_data$drv_sex <- as.factor(artificial_data$drv_sex)
artificial_data$vh_make <- as.factor(artificial_data$vh_make)
