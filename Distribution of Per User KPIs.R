# Load the libraries
if(system.file(package == "pacman") == "") {
  install.packages("pacman")
}

pacman::p_load(ggplot2, dplyr, bigrquery, fitdistrplus, gamlss, gamlss.dist, gamlss.add, ggpubr, ggplotify)

# Google authentication token
options(gargle_oauth_email = "omar.elmaria@deliveryhero.com")

# Download the data frame
project_id_data <- "dh-logistics-product-ops"
project_id_billing <- "dh-logistics-product-ops"
dataset_id <- "pricing"
tbl_name_per_user <- "kpis_per_user"
tbl_name_raw_orders <- "raw_orders_data_for_bayesian"
tbl_name_per_day <- "per_day_kpis"

# df per user
bq_conn <- dbConnect(bigquery(),
                     project = project_id_data,
                     billing = project_id_billing,
                     dataset = dataset_id,
                     use_legacy_sql = FALSE)

df_per_user <- tbl(bq_conn, tbl_name_per_user)

df_per_user_tg_1 <- df_per_user %>% 
  filter(target_group == "Target Group 1", !(variant %in% c("Variation5", "Variation6"))) %>% 
  collect()

df_per_user_tg_2 <- df_per_user %>% 
  filter(target_group == "Target Group 2", !(variant %in% c("Variation5", "Variation6"))) %>% 
  collect()

df_non_tg_per_user <- df_per_user %>% 
  filter(target_group == "Non_TG") %>% 
  collect()

##-----------------------------------------------------------##-----------------------------------------------------------##

# df raw orders
bq_conn2 <- dbConnect(bigquery(),
                     project = project_id_data,
                     billing = project_id_billing,
                     dataset = dataset_id,
                     use_legacy_sql = FALSE)

df_raw_orders <- tbl(bq_conn2, tbl_name_raw_orders)

df_raw_orders_tg_1 <- df_raw_orders %>% 
  filter(target_group == "Target Group 1", !(variant %in% c("Variation5", "Variation6"))) %>% 
  collect()

df_raw_orders_tg_2 <- df_raw_orders %>% 
  filter(target_group == "Target Group 2", !(variant %in% c("Variation5", "Variation6"))) %>% 
  collect()

df_non_tg_raw_orders <- df_raw_orders %>% 
  filter(target_group == "Non_TG") %>% 
  collect()

##-----------------------------------------------------------##-----------------------------------------------------------##

# df per day (coming from the TH PKK LB test)
bqconn3 <- dbConnect(bigquery(),
                     project = project_id_data,
                     billing = project_id_billing,
                     dataset = dataset_id, 
                     use_legacy_sql = FALSE)

df_per_day <- tbl(bqconn3, tbl_name_per_day)

df_per_day_tg_1 <- df_per_day %>% 
  filter(target_group == "TG1") %>% 
  collect()

##-----------------------------------------------------------##-----------------------------------------------------------##

# Distribution of metrics "Target Group 1" ONLY

## Order Count Per User
ggplot(data = df_per_user_tg_1, aes(x = order_count_per_user, fill = variant, alpha = 0.1)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left") +
  geom_text(stat = "count", aes(label = ..count..), hjust = -0.25) +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
  facet_wrap(~variant) +
  labs(title = "Variant Distributions of 'Order Count Per User' Incl. Converted and Unconverted Users - Target Group 1",
       subtitle = "The bin width = 1. Every bin is closed on the LEFT") +
  theme(plot.subtitle = element_text(color = "red"))

## Profit Per User
ggplot(data = df_per_user_tg_1, aes(x = profit_per_user, fill = variant, alpha = 0.1)) +
  geom_histogram(binwidth = 25, boundary = 0, closed = "left") +
  scale_x_continuous(breaks = seq(-50, 550, 25), limits = c(-50, 550)) +
  facet_wrap(~variant) +
  labs(title = "Variant Distributions of 'Profit Per User' Incl. Converted and Unconverted Users - Target Group 1",
       subtitle = "The bin width = 25. Every bin is closed on the LEFT") +
  theme(plot.subtitle = element_text(color = "red"))

## Revenue Per User
ggplot(data = df_per_user_tg_1, aes(x = revenue_per_user, fill = variant, alpha = 0.1)) +
  geom_histogram(binwidth = 25, boundary = 0, closed = "left") +
  scale_x_continuous(breaks = seq(0, 550, 25), limits = c(0, 550)) +
  facet_wrap(~variant) +
  labs(title = "Variant Distributions of 'Revenue Per User' Incl. Converted and Unconverted Users - Target Group 1",
       subtitle = "The bin width = 25. Every bin is closed on the LEFT") +
  theme(plot.subtitle = element_text(color = "red"))

##-----------------------------------------------------------##-----------------------------------------------------------##

# Plots and distribution fitting

## Box plots
### Order count per user
ord1_box <- ggplot(data = df_per_user_tg_1, aes(x = order_count_per_user)) +
  geom_boxplot() + # Converted and unconverted
  labs(title = "Order Count Per User Box Plot - Converted + Unconverted Users")
ord2_box <- ggplot(data = df_per_user_tg_1 %>% filter(order_count_per_user > 0), aes(x = order_count_per_user)) +
  geom_boxplot() + # Converted only
  labs(title = "Order Count Per User Box Plot - Converted Users Only")
ord_box_comb <- ggarrange(ord1_box, ord2_box, align = 'h', ncol = 2, nrow = 1)

### Profit per user
profit1_box <- ggplot(data = df_per_user_tg_1, aes(x = profit_per_user)) +
  geom_boxplot() + # Converted and unconverted
  labs(title = "Profit Per User Box Plot - Converted + Unconverted Users")
profit2_box <- ggplot(data = df_per_user_tg_1 %>% filter(profit_per_user > 0), aes(x = profit_per_user)) +
  geom_boxplot() + # Converted only
  labs(title = "Profit Per User Box Plot - Converted Users Only")
profit_box_comb <- ggarrange(profit1_box, profit2_box, align = 'h', ncol = 2, nrow = 1)

### Revenue per user
revenue1_box <- ggplot(data = df_per_user_tg_1, aes(x = revenue_per_user)) +
  geom_boxplot() + # Converted and unconverted
  labs(title = "Revenue Per User Box Plot - Converted + Unconverted Users")
revenue2_box <- ggplot(data = df_per_user_tg_1 %>% filter(revenue_per_user > 0), aes(x = revenue_per_user)) +
  geom_boxplot() + # Converted only
  labs(title = "Revenue Per User Box Plot - Converted Users Only")
revenue_box_comb <- ggarrange(revenue1_box, revenue2_box, align = 'h', ncol = 2, nrow = 1)

##-----------------------------------------------------------##-----------------------------------------------------------##

# Outliers calculation
df_per_user_tg_1 <- df_per_user_tg_1 %>% 
  mutate(order_count_per_user_lower_outlier_bound = quantile(order_count_per_user, probs = 0.25, na.rm = TRUE) - 1.5 * IQR(order_count_per_user, na.rm = TRUE),
         order_count_per_user_upper_outlier_bound = quantile(order_count_per_user, probs = 0.75, na.rm = TRUE) + 1.5 * IQR(order_count_per_user, na.rm = TRUE),
         
         profit_per_user_lower_outlier_bound = quantile(profit_per_user, probs = 0.25, na.rm = TRUE) - 1.5 * IQR(profit_per_user, na.rm = TRUE),
         profit_per_user_upper_outlier_bound = quantile(profit_per_user, probs = 0.75, na.rm = TRUE) + 1.5 * IQR(profit_per_user, na.rm = TRUE),
         
         revenue_per_user_lower_outlier_bound = quantile(revenue_per_user, probs = 0.25, na.rm = TRUE) - 1.5 * IQR(revenue_per_user, na.rm = TRUE),
         revenue_per_user_upper_outlier_bound = quantile(revenue_per_user, probs = 0.75, na.rm = TRUE) + 1.5 * IQR(revenue_per_user, na.rm = TRUE))

share_of_non_outliers_orders_per_user <- length(df_per_user_tg_1$order_count_per_user[df_per_user_tg_1$order_count_per_user >= df_per_user_tg_1$order_count_per_user_lower_outlier_bound & 
                                                                                      df_per_user_tg_1$order_count_per_user <= df_per_user_tg_1$order_count_per_user_upper_outlier_bound]) / length(df_per_user_tg_1$order_count_per_user)

share_of_non_outliers_profit_per_user <- length(df_per_user_tg_1$profit_per_user[df_per_user_tg_1$profit_per_user >= df_per_user_tg_1$profit_per_user_lower_outlier_bound & 
                                                                                 df_per_user_tg_1$profit_per_user <= df_per_user_tg_1$profit_per_user_upper_outlier_bound]) / length(df_per_user_tg_1$profit_per_user)

share_of_non_outliers_revenue_per_user <- length(df_per_user_tg_1$revenue_per_user[df_per_user_tg_1$revenue_per_user >= df_per_user_tg_1$revenue_per_user_lower_outlier_bound & 
                                                                                   df_per_user_tg_1$revenue_per_user <= df_per_user_tg_1$revenue_per_user_upper_outlier_bound]) / length(df_per_user_tg_1$revenue_per_user)

##-----------------------------------------------------------##-----------------------------------------------------------##

# Cullen and Frey Graphs (per user metrics - outliers INCLUDED)
## Converted and unconverted
order_count_per_user_conv_and_unconv_outlier_incl <- df_per_user_tg_1$order_count_per_user
profit_per_user_conv_and_unconv_outlier_incl <- df_per_user_tg_1$profit_per_user
revenue_per_user_conv_and_unconv_outlier_incl <- df_per_user_tg_1$revenue_per_user

descdist(order_count_per_user_conv_and_unconv, discrete = FALSE, boot = 100, graph = TRUE)
descdist(profit_per_user_conv_and_unconv, discrete = FALSE, boot = 100, graph = TRUE)
descdist(revenue_per_user_conv_and_unconv, discrete = FALSE, boot = 100, graph = TRUE)

## Converted only
order_count_per_user_conv_only_outlier_incl <- df_per_user_tg_1$order_count_per_user[df_per_user_tg_1$order_count_per_user > 0]
profit_per_user_conv_only_outlier_incl <- df_per_user_tg_1$profit_per_user[df_per_user_tg_1$profit_per_user > 0]
revenue_per_user_conv_only_outlier_incl <- df_per_user_tg_1$revenue_per_user[df_per_user_tg_1$revenue_per_user > 0]

descdist(order_count_per_user_conv_only_outlier_incl, discrete = FALSE, boot = 100) # You could use  & df_per_user_tg_1$variant == "Variation4"] here
descdist(profit_per_user_conv_only_outlier_incl, discrete = FALSE, boot = 100)
descdist(revenue_per_user_conv_only_outlier_incl, discrete = FALSE, boot = 100)

##-----------------------------------------------------------##-----------------------------------------------------------##

# Cullen and Frey Graphs (per user metrics - outliers EXCLUDED)
## Converted and unconverted
order_count_per_user_conv_and_unconv_outlier_excl <- df_per_user_tg_1$order_count_per_user[df_per_user_tg_1$order_count_per_user >= df_per_user_tg_1$order_count_per_user_lower_outlier_bound & 
                                                                                           df_per_user_tg_1$order_count_per_user <= df_per_user_tg_1$order_count_per_user_upper_outlier_bound]

profit_per_user_conv_and_unconv_outlier_excl <- df_per_user_tg_1$profit_per_user[df_per_user_tg_1$profit_per_user >= df_per_user_tg_1$profit_per_user_lower_outlier_bound & 
                                                                                 df_per_user_tg_1$profit_per_user <= df_per_user_tg_1$profit_per_user_upper_outlier_bound]

revenue_per_user_conv_and_unconv_outlier_excl <- df_per_user_tg_1$revenue_per_user[df_per_user_tg_1$revenue_per_user >= df_per_user_tg_1$revenue_per_user_lower_outlier_bound & 
                                                                                  df_per_user_tg_1$revenue_per_user <= df_per_user_tg_1$revenue_per_user_upper_outlier_bound]

descdist(order_count_per_user_conv_and_unconv_outlier_excl, discrete = FALSE, boot = 100, graph = TRUE)

descdist(profit_per_user_conv_and_unconv_outlier_excl, discrete = FALSE, boot = 100, graph = TRUE)

descdist(revenue_per_user_conv_and_unconv_outlier_excl, discrete = FALSE, boot = 100, graph = TRUE)

## Converted only
order_count_per_user_conv_only_outlier_excl <- df_per_user_tg_1$order_count_per_user[df_per_user_tg_1$order_count_per_user >= df_per_user_tg_1$order_count_per_user_lower_outlier_bound & 
                                                                                     df_per_user_tg_1$order_count_per_user <= df_per_user_tg_1$order_count_per_user_upper_outlier_bound &
                                                                                     df_per_user_tg_1$order_count_per_user > 0]

profit_per_user_conv_only_outlier_excl <- df_per_user_tg_1$profit_per_user[df_per_user_tg_1$profit_per_user >= df_per_user_tg_1$profit_per_user_lower_outlier_bound & 
                                                                           df_per_user_tg_1$profit_per_user <= df_per_user_tg_1$profit_per_user_upper_outlier_bound &
                                                                           df_per_user_tg_1$profit_per_user > 0]

revenue_per_user_conv_only_outlier_excl <- df_per_user_tg_1$revenue_per_user[df_per_user_tg_1$revenue_per_user >= df_per_user_tg_1$revenue_per_user_lower_outlier_bound & 
                                                                            df_per_user_tg_1$revenue_per_user <= df_per_user_tg_1$revenue_per_user_upper_outlier_bound &
                                                                            df_per_user_tg_1$revenue_per_user > 0]

descdist(order_count_per_user_conv_only_outlier_excl, discrete = FALSE, boot = 100, graph = TRUE)

descdist(profit_per_user_conv_only_outlier_excl, discrete = FALSE, boot = 100, graph = TRUE)

descdist(revenue_per_user_conv_only_outlier_excl, discrete = FALSE, boot = 100, graph = TRUE)

##-----------------------------------------------------------##-----------------------------------------------------------##

# Trying out all distributions in the Cullen and Frey graph and calculating their AIC
# Do NOT include "beta" as no KPI is bounded between 0 and 1
# "Log-normal" and "Gamma" do NOT work if you have zeroes or negative numbers in your data
distrib <- c("norm", "unif", "exp", "logis", "lnorm", "gamma")

# Get all variables that contain the data to be used in "fitdist"
data_vectors <- sort(ls(.GlobalEnv)[grep("_excl|_incl", ls(.GlobalEnv))])

# Create an empty list to contain the results of "fitdist"
fit <- vector(mode = "list", length = length(data_vectors))
names(fit) <- data_vectors # Rename the lists according to the data vectors

# Create a function that fits a data set to a specific distribution and calculates the AIC
for (i in data_vectors) {
  final_distrib_used <- c()
  for (j in distrib) {
    # If the data set contains non-converted users and the distribution assessed is either "lnorm" or "gamma", skip "fitdist"
    # If the data set contains profit values (which might be negative) and the distribution assessed is "lnorm", "gamma", or "exp", skip "fitdist"
    if ((grepl("_unconv_", i) & j %in% c("lnorm", "gamma")) | (grepl("profit_per_user_conv_and_unconv", i) & j %in% c("lnorm", "gamma", "exp"))) {
      next
    } else {
      fit[[i]][j] <- summary(fitdist(data = eval(parse(text = i)), distr = j, keepdata = FALSE, discrete = FALSE))$aic
      final_distrib_used <- append(final_distrib_used, j)
    }
  }
  names(fit[[i]]) <- paste0(final_distrib_used, "_aic")
}

# Put the final results of the list into a data frame
model_fitting_results <- data.frame(KPI = names(fit))
model_fitting_results$min_aic <- unlist(lapply(fit, function(x) {round(min(x), 2)}))
model_fitting_results$winning_model <- unlist(lapply(fit, function(x) {names(which(x == min(x)))}))

# Sort the data frame
model_fitting_results$KPI <- factor(model_fitting_results$KPI, levels = c("order_count_per_user_conv_and_unconv_outlier_incl",
                                                                          "profit_per_user_conv_and_unconv_outlier_incl",
                                                                          "revenue_per_user_conv_and_unconv_outlier_incl",
                                                                          "order_count_per_user_conv_only_outlier_incl",
                                                                          "profit_per_user_conv_only_outlier_incl",
                                                                          "revenue_per_user_conv_only_outlier_incl",
                                                                          "order_count_per_user_conv_and_unconv_outlier_excl",
                                                                          "profit_per_user_conv_and_unconv_outlier_excl",
                                                                          "revenue_per_user_conv_and_unconv_outlier_excl",
                                                                          "order_count_per_user_conv_only_outlier_excl",
                                                                          "profit_per_user_conv_only_outlier_excl",
                                                                          "revenue_per_user_conv_only_outlier_excl"))
model_fitting_results <- model_fitting_results %>% 
  arrange(KPI)

# Export to an Excel file
# writexl::write_xlsx(model_fitting_results, "model_fitting_results.xlsx")

##--------------------------------THE CODE BELOW WAS **NOT** USED IN CREATING THE SLIDE DECK-------------------------------##

### PLAYING FIELD (1)
# Cullen and Frey Graphs (per order metrics - outliers INCLUDED)
descdist(df_raw_orders_tg_1$revenue_local[df_raw_orders_tg_1$revenue_local > 0], discrete = FALSE)

# Cullen and Frey Graphs (per order metrics - outliers EXCLUDED)
descdist(df_per_day_tg_1$avg_profit_local[df_per_day_tg_1$avg_profit_local & df_per_day_tg_1$variant == "Var1"], discrete = FALSE)

##-----------------------------------------------------------##-----------------------------------------------------------##

### PLAYING FIELD (2)
# Theoretical normal, uniform, exponential, logistic, beta, log-normal and gamma distributions
## Normal
ggplot(data = data.frame(x = rnorm(100000)), aes(x = x)) +
  geom_histogram(bins = 30)

## Uniform
ggplot(data = data.frame(x = runif(100000)), aes(x = x)) +
  geom_histogram(bins = 30)

## Exponential
ggplot(data = data.frame(x = rexp(100000)), aes(x = x)) +
  geom_histogram(bins = 30)

## Logistic (resembles the normal distribution but with a higher kurtosis)
ggplot(data = data.frame(x = rlogis(100000)), aes(x = x)) +
  geom_histogram(bins = 30)

## Beta
ggplot(data = data.frame(x = rbeta(100000, shape1 = 2, shape2 = 2)), aes(x = x)) +
  geom_histogram(bins = 30)

# Log normal
ggplot(data = data.frame(x = rlnorm(100000, meanlog = 0, sdlog = 1.5)), aes(x = x)) +
  geom_histogram(bins = 30)

# Gamma
ggplot(data = data.frame(x = rgamma(100000, shape = 0.25, rate = 2)), aes(x = x)) +
  geom_histogram(bins = 30)

##-----------------------------------------------------------##-----------------------------------------------------------##

### PLAYING FIELD (3)
# Gamlss (WARNING: THIS CODE TAKES HOURS TO RUN - PLEASE PROCEED WITH CAUTION)

## Converted and unconverted (outliers included)
ord_fit_conv_and_unconv_outlier_incl <- fitDist(order_count_per_user_conv_and_unconv_outlier_incl, k = 2, type = "realplus", try.gamlss = TRUE, parallel = "multicore", ncpus = 4)
profit_fit_conv_and_unconv_outlier_incl <- fitDist(profit_per_user_conv_and_unconv_outlier_incl, k = 2, type = "realline", try.gamlss = TRUE, parallel = "multicore", ncpus = 4)
revenue_fit_conv_and_unconv_outlier_incl <- fitDist(revenue_per_user_conv_and_unconv_outlier_incl, k = 2, type = "realplus", try.gamlss = TRUE, parallel = "multicore", ncpus = 4)

## Converted ONLY (outliers included)
ord_fit_conv_only_outlier_incl <- fitDist(order_count_per_user_conv_only_outlier_incl, 
                                          k = 2, type = "realplus", try.gamlss = TRUE, parallel = "multicore", ncpus = 4)
profit_fit_conv_only_outlier_incl <- fitDist(profit_per_user_conv_only_outlier_incl, 
                                             k = 2, type = "realplus", try.gamlss = TRUE, parallel = "multicore", ncpus = 4)
revenue_fit_conv_only_outlier_incl <- fitDist(revenue_per_user_conv_only_outlier_incl, 
                                              k = 2, type = "realplus", try.gamlss = TRUE, parallel = "multicore", ncpus = 4)

## Converted and unconverted (outliers excluded)
ord_fit_conv_and_unconv_outlier_excl <- fitDist(order_count_per_user_conv_and_unconv_outlier_excl, 
                                                k = 2, type = "realplus", try.gamlss = TRUE, parallel = "multicore", ncpus = 4)

profit_fit_conv_and_unconv_outlier_excl <- fitDist(profit_per_user_conv_and_unconv_outlier_excl, 
                                                   k = 2, type = "realline", try.gamlss = TRUE, parallel = "multicore", ncpus = 4)

revenue_fit_conv_and_unconv_outlier_excl <- fitDist(revenue_per_user_conv_and_unconv_outlier_excl, 
                                                    k = 2, type = "realplus", try.gamlss = TRUE, parallel = "multicore", ncpus = 4)

## Converted only (outliers excluded)
ord_fit_conv_only_outlier_excl <- fitDist(order_count_per_user_conv_only_outlier_excl, 
                                          k = 2, type = "realplus", try.gamlss = TRUE, parallel = "multicore", ncpus = 4)

profit_fit_conv_only_outlier_excl <- fitDist(profit_per_user_conv_only_outlier_excl, 
                                             k = 2, type = "realplus", try.gamlss = TRUE, parallel = "multicore", ncpus = 4)

revenue_fit_conv_only_outlier_excl <- fitDist(revenue_per_user_conv_only_outlier_excl, 
                                              k = 2, type = "realplus", try.gamlss = TRUE, parallel = "multicore", ncpus = 4)
