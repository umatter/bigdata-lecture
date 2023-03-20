
# Mass storage/compression -------------------------------
# compress CSV via data.table::fwrite()

library(data.table)

# load example data from basic R installation
data("LifeCycleSavings")

# write data to normal csv file and check size
fwrite(LifeCycleSavings, file="lcs.csv")
file.size("lcs.csv")

# write data to a GZIPPED (compressed) csv file and check size
fwrite(LifeCycleSavings, file="lcs.csv.gz")
file.size("lcs.csv.gz")

# read/import the compressed data
lcs <- data.table::fread("lcs.csv.gz")


# Mass storage/compression -------------------------------
# compress CSV via data.table::fwrite()

# common ZIP compression (independent of data.table package)
write.csv(LifeCycleSavings, file="lcs.csv")
file.size("lcs.csv")
zip(zipfile = "lcs.csv.zip", files =  "lcs.csv")
file.size("lcs.csv.zip")

# unzip/decompress and read/import data
lcs_path <- unzip("lcs.csv.zip")
lcs <- read.csv(lcs_path)




# Memory allocation illustration -------------------------------
# read.csv, sequential reading.


# fix variables
DATA_PATH <- "data/flights.csv"
# load packages
library(pryr) 

# check how much memory is used by R (overall)
mem_used()
# DATA IMPORT 
mem_change(flights <- read.csv(DATA_PATH))
# DATA PREPARATION 
flights <- flights[,-1:-3]
# check how much memory is used by R now
mem_used()



# Memory allocation illustration -------------------------------
# fread/memory mapping

# load packages
library(data.table)

flights <- fread(DATA_PATH, verbose = TRUE)



# Memory allocation illustration -------------------------------
# fread/memory mapping

# housekeeping
rm(flights)
gc()

# check the change in memory due to each step
# DATA IMPORT 
mem_change(flights <- fread(DATA_PATH))





# Parallelization -------------------------------------
# preparatory steps and sequential implementation 

# packages
library(stringr)

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

# COMPUTE REGRESSIONS
N <- 10 # just for illustration, the actual code is N <- length(models)
pseudo_Rsq <- list()
length(pseudo_Rsq) <- N
for ( i in 1:N) {
     # fit the logit model via maximum likelihood
     fit <- glm(models[[i]], data=marketing, family = binomial())
     # compute the proportion of deviance explained by the independent vars (~R^2)
     pseudo_Rsq[[i]] <- 1-(fit$deviance/fit$null.deviance)
}

# SELECT THE WINNER ---------------
models[[which.max(pseudo_Rsq)]]






# Parallelization -------------------------------------
# foreach 

# packages for parallel processing
library(parallel)
library(doSNOW)

# get the number of cores available
ncores <- parallel::detectCores()
# set cores for parallel processing
ctemp <- makeCluster(ncores)
registerDoSNOW(ctemp)

# prepare loop
N <- 10000 # just for illustration, the actual code is N <- length(models)
# run loop in parallel
pseudo_Rsq <-
     foreach ( i = 1:N, .combine = c) %dopar% {
          # fit the logit model via maximum likelihood
          fit <- glm(models[[i]], data=marketing, family = binomial())
          # compute the proportion of deviance explained by the independent vars (~R^2)
          return(1-(fit$deviance/fit$null.deviance))
     }

# SELECT THE WINNER 
models[[which.max(pseudo_Rsq)]]






# Parallelization -------------------------------------
# mclapply/forking

# COMPUTE REGRESSIONS IN PARALLEL
# prepare parallel lapply (based on forking, here clearly faster than foreach)
N <- 10000 # just for illustration, the actual code is N <- length(models)
# run parallel lapply
pseudo_Rsq <- mclapply(1:N,
                       mc.cores = ncores,
                       FUN = function(i){
                            # fit the logit model via maximum likelihood
                            fit <- glm(models[[i]], data=marketing, family = binomial())
                            # compute the proportion of deviance explained by the independent vars (~R^2)
                            return(1-(fit$deviance/fit$null.deviance))
                       })

# SELECT THE WINNER, SHOW FINAL OUTPUT 
best_model <- models[[which.max(pseudo_Rsq)]]
best_model






# GPU -------------------------------------------
# matrix multiplication

# load package
library(bench)
library(gpuR)

# initiate dataset with pseudo random numbers
N <- 10000  # number of observations
P <- 100 # number of variables
X <- matrix(rnorm(N * P, 0, 1), nrow = N, ncol =P)


# prepare GPU-specific objects/settings
gpuX <- gpuMatrix(X, type = "float")  # point GPU to matrix (matrix stored in non-GPU memory)
vclX <- vclMatrix(X, type = "float")  # transfer matrix to GPU (matrix stored in GPU memory)


# compare three approaches
gpu_cpu <- bench::mark(
     
     # compute with CPU 
     cpu <- t(X) %*% X,
     
     # GPU version, GPU pointer to CPU memory (gpuMatrix is simply a pointer)
     gpu1_pointer <- t(gpuX) %*% gpuX,
     
     # GPU version, in GPU memory (vclMatrix formation is a memory transfer)
     gpu2_memory <- t(vclX) %*% vclX,
     
     check = FALSE, memory=FALSE, min_iterations = 20)


# plot benchmarking
plot(gpu_cpu, type = "boxplot")






