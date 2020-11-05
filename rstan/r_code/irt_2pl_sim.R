# Stan IRT Example

library(rstan)
library(psych)
library(reshape2)
library(ggplot2)
library(dplyr)

# Simulate Data

sim.dat <- psych::sim.irt(nvar = 10, n = 3000, low=-3, high=3,mu=0,sd=1,mod="logistic")

resp <- data.frame(sim.dat$items)

resp$id <- seq.int(nrow(resp))

# Prepare Data for STAN

long <- melt(resp, id.vars = "id", variable.name = "item", value.name = "response")
head(long)

key <- 1:length(unique(long$item))
names(key) <- unique(long$item)
long$item.id <- key[long$item]
head(long)

# No missing data!

stan_data <- list(I = max(long$item.id), # Number of Items
                  J = max(long$id), # Number of Test-Takers
                  N = nrow(long),  # Nuber of Obersvations
                  ii = long$item.id, 
                  jj = long$id,
                  y = long$response)

# Fit Model

twopl.fit <- stan(file = paste0(getwd(),"/stan_code/2pl_model.stan"), data = stan_data, iter = 4000, chains = 5)

output <- extract(twopl.fit, permuted = TRUE) 

theta <- output$theta

# Model Checking

# Extract y_rep from stanfit object
# 2000 replications = (1000/2) iter * 4 chains; 1000/2 as half is discarded for warmup
# 20*1500

y_rep <- output$y_rep
str(y_rep)


# (1) Compute the observed raw score distribution
score.obs <- tapply(long$response, long$id, sum)
observed <- data.frame(table(score.obs))
colnames(observed) <- c("score", "obs")
observed$score <- as.numeric(as.character(observed$score))

# (2) Compute the replicated (posterior predictive) raw score distributions
# from the y_rep
score.rep <- apply(y_rep, 1, function(x) tapply(x, long$id, sum))
replicated <- apply(score.rep, 2, function(x) table(x))

# (3) Compute 5%, 50% (median), and 95% quantiles of each replicated raw
# score distribution

?apply

replicated <-bind_rows(replicated)

rep.quantile <- apply(replicated, 1, function(x) quantile(x, probs = c(0.05, 0.5, 0.95), na.rm = T))
rep.quantile <- data.frame(t(rep.quantile))
colnames(rep.quantile) <- c("Q5", "Q50", "Q95")

# (4) Reshape the replicated raw score distributions into long format
replicated2 <- data.frame(t(replicated))
colnames(replicated2) <- seq(0, nrow(replicated2), 1)
replicated2$id <- 1:nrow(replicated2)  # attach a ID number to each row.
rep.long <- melt(replicated2, id.vars = "id", variable.name = "score", value.name = "frequency")

# (5) Combine the observed and replicated raw score distribution
df <- cbind(observed, rep.quantile)
print(df)

# Plot observed and replicated score distributions

# (1) Basic settings of plot
p <- ggplot(df, aes(x = score + 1, y = obs)) + theme_bw() + labs(x = "Raw score", 
                                                                 y = "Number of examinees", title = "The observed and replicated raw score distributions")
# (2) Add the entire replicated raw score distributions using violin plot
# and jittered data points
p <- p + geom_violin(data = rep.long, aes(x = score, y = frequency))
p <- p + geom_jitter(data = rep.long, aes(x = score, y = frequency, color = score), 
                     position = position_jitter(width = 0.12), alpha = 0.15) + theme(legend.position = "none")

# (3) Overlay 5%, 50% (median), and 95% quantiles of each replicated raw
# score distribution
p <- p + geom_point(aes(y = Q50), size = 4, shape = 2) + geom_line(aes(y = Q50), 
                                                                   size = 1, linetype = "dashed")
p <- p + geom_line(aes(y = Q5), size = 1, linetype = "dotted")
p <- p + geom_line(aes(y = Q95), size = 1, linetype = "dotted")

# (4) Overlay the observed raw score distribution
p <- p + geom_point(size = 4) + geom_line(size = 1)
print(p)
