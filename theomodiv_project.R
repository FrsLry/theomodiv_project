## Project
library(jagsUI)
rm(list = ls())
data <- readRDS("project/data/jags_data_par.rds")
## Select species
# data <- list("American Crow" = data[["American Crow"]], "Blue Jay" = data[["Blue Jay"]])
# saveRDS(data,"project/data/jags_data_par.rds")
data <- data[["Blue Jay"]]
# data <- data[["American Crow"]]
## Create data for JAGS model
data_jags <- list(obs = ifelse(data$C == 0, 0, 1),
                  n_site = nrow(data$C),
                  n_year = ncol(data$C))
## Specify JAGS models
cat(file = "project/models/simplest_model.txt","
model {
  # Define the underlying model - as well as the observation process (although not used here - d = 1)
  for(i in 1:n_site){
    for(j in 2:n_year){
        obs[i,j] ~ dbern(x[i,j]*detec)
        x[i,j] ~ dbern( x[i,j-1] * (1-e) + (1-x[i,j-1])*c )
    }
  }


  # For first year, init with from psi parameter (because the first needs to be fitted)
  for(i in 1:n_site){
    x[i,1] ~ dbern(psi)
  }

  # Logit link
  logit(e) = e_log
  logit(c) = c_log
  logit(psi) = psi_log
  # logit(detec) = detec_log
  detec = 1


  # Define prior for e/c/psi on logit scale
  e_log ~ dnorm(0, 0.1)
  c_log ~ dnorm(0, 0.1)
  psi_log ~ dnorm(0, 0.1)
  # detec_log ~ dnorm(0, 0.1)
}")
## parameters to save
params <- c('e', 'c', 'psi', 'detec', 'x')
## hyperparameters
na <- 1000 ; ni <- 1000 ; nt <- 5 ; nb <- 500 ; nc <- 3
## Generate initial values
x = data_jags$obs
x[is.na(x)] = sample(c(0,1), sum(is.na(x)), replace = T)
inits <- rep(list(list(x = x)), 3)
## Fit the model
## Call JAGS
out1 <- jagsUI::jags(data_jags, inits, params, "project/models/simplest_model.txt", n.adapt = na, n.chains = nc,
                     n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
