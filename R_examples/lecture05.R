
# CLOUD COMPUTING
# Parallelization on EC2 -----------------------------

# install packages for parallelization
install.packages("parallel", "doSNOW", "stringr")

# load packages
library(parallel)
library(doSNOW)
library(stringr)

# verify no. of cores available
n_cores <- detectCores()
n_cores


# PREPARATION 
# import data
marketing <- read.csv("data/marketing_data.csv")
# clean/prepare data
marketing$Income <- as.numeric(gsub("[[:punct:]]", "", marketing$Income))
marketing$days_customer <- as.Date(Sys.Date())- as.Date(marketing$Dt_Customer, "%m/%d/%y")
marketing$Dt_Customer <- NULL

# all sets of independent vars
indep <- names(marketing)[ c(2:19, 27,28)]
combinations_list <- lapply(1:length(indep),
                            function(x) combn(indep, x, simplify = FALSE))
combinations_list <- unlist(combinations_list, recursive = FALSE)
models <- lapply(combinations_list,
                 function(x) paste("Response ~", paste(x, collapse="+")))

# TEST CODE

# set cores for parallel processing
# ctemp <- makeCluster(ncores)
# registerDoSNOW(ctemp)

# prepare loop
N <- 10 # just for illustration, the actual code is N <- length(models)
# run loop in parallel
pseudo_Rsq <-
     foreach ( i = 1:N, .combine = c) %dopar% {
          # fit the logit model via maximum likelihood
          fit <- glm(models[[i]], data=marketing, family = binomial())
          # compute the proportion of deviance explained by the independent vars (~R^2)
          return(1-(fit$deviance/fit$null.deviance))
     }


# RUN IN PARALLEL (AFTER SCALING UP) --------------------

# set cores for parallel processing
ctemp <- makeCluster(ncores)
registerDoSNOW(ctemp)

# prepare loop
N <- 10 # just for illustration, the actual code is N <- length(models)
# run loop in parallel
pseudo_Rsq <-
     foreach ( i = 1:N, .combine = c) %dopar% {
          # fit the logit model via maximum likelihood
          fit <- glm(models[[i]], data=marketing, family = binomial())
          # compute the proportion of deviance explained by the independent vars (~R^2)
          return(1-(fit$deviance/fit$null.deviance))
     }

