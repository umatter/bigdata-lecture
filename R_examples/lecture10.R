
## Simple regression analysis  --------------------------------------------

# Classical R example
# load the data
flights_r <- data.table::fread("data/flights.csv", nrows = 300) 
# specify the linear model
model1 <- arr_delay ~ dep_delay + distance
# fit the model with ols
fit1 <- lm(model1, flights_r)
# compute t-tests etc.
summary(fit1)






# Do the same on a local Spark cluster

# We use sparklyr as an R-interface to Spark
library(sparklyr)
# connect with default configuration
sc <- spark_connect(master="local")


# load data to spark
flights_spark <- copy_to(sc, flights_r, "flights_spark")
# fit the model
fit1_spark <- ml_linear_regression(flights_spark, formula = model1)
# compute summary stats
summary(fit1_spark)

# Alternatively, we can use the `spark_apply()` function to run the regression 
# analysis in R via the original R `lm()`-function.^[Note though, that this approach might take longer.]
# (might be much slower)


# fit the model
spark_apply(flights_spark, function(df) broom::tidy(lm(arr_delay ~ dep_delay + distance, df)),
            names = c("term", "estimate", "std.error", "statistic", "p.value")
)

# Finally, the `parsnip` package (together with the `tidymodels` package) 
# provides a simple interface to run the same model (or similar specifications) on 
# different "engines" (estimators/fitting algorithms)



# load additional packages
library(tidymodels)
library(parsnip)

# simple local linear regression example from above
# via tidymodels/parsnip
fit1 <- fit(linear_reg(engine="lm"), model1, data=flights_r)
tidy(fit1)

# run the same on spark 
fit1_spark <- fit(linear_reg(engine="spark"), model1, data=flights_spark)
tidy(fit1_spark)





## Machine learning for classification -------------------------------------------------


# load into R, # select variables of interest, remove missing
titanic_r <- read.csv("data/titanic3.csv")
titanic_r <- na.omit(titanic_r[, c("survived",
                                   "pclass",
                                   "sex",
                                   "age",
                                   "sibsp",
                                   "parch")])
titanic_r$survived <- ifelse(titanic_r$survived==1, "yes", "no")

# In order to assess the performance of the classifiers later on, 
# we split the sample into training and test data sets. 

library(rsample)
# split into training and test set
titanic_r <- initial_split(titanic_r)
ti_training <- training(titanic_r)
ti_testing <- testing(titanic_r)


# load data to spark
ti_training_spark <- copy_to(sc, ti_training, "ti_training_spark")
ti_testing_spark <- copy_to(sc, ti_testing, "ti_testing_spark")

# "Horse Race"
# models to be used
models <- list(logit=logistic_reg(engine="spark", mode = "classification"),
               btree=boost_tree(engine = "spark", mode = "classification"),
               rforest=rand_forest(engine = "spark", mode = "classification"))
# train/fit the models
fits <- lapply(models, fit, formula=survived~., data=ti_training_spark)



# Assess performance
# run predictions
predictions <- lapply(fits, predict, new_data=ti_testing_spark)
# fetch predictions from Spark, format, add actual outcomes
pred_outcomes <- 
     lapply(1:length(predictions), function(i){
          x_r <- collect(predictions[[i]]) # fetch from spark cluster (load into local R environment)
          x_r$pred_class <- as.factor(x_r$pred_class) # format for predictions
          x_r$survived <- as.factor(ti_testing$survived) # add true outcomes
          return(x_r)
          
     })



acc <- lapply(pred_outcomes, accuracy, truth="survived", estimate="pred_class")
acc <- bind_rows(acc)
acc$model <- names(fits)
acc[order(acc$.estimate, decreasing = TRUE),]

# compare
tidy(fits[["btree"]])
tidy(fits[["rforest"]])






## Building Machine Learning Pipelines with R and Spark -----------------------------

### Set up and data import

# load packages
library(sparklyr)
library(dplyr)
# fix vars
INPUT_DATA <- "data/ga.csv"



# import to local R session, prepare raw data
ga <- na.omit(read.csv(INPUT_DATA))
#ga$purchase <- as.factor(ifelse(ga$purchase==1, "yes", "no"))
# connect to, and copy the data to the local cluster
#sc <- spark_connect(master = "local")
ga_spark <- copy_to(sc, ga, "ga_spark")


# Building the pipeline

# ml pipeline
ga_pipeline <- 
     ml_pipeline(sc) %>%
     ft_string_indexer(input_col="city", 
                       output_col="city_output",
                       handle_invalid = "skip") %>%
     ft_string_indexer(input_col="country", 
                       output_col="country_output",
                       handle_invalid = "skip") %>%
     ft_string_indexer(input_col="source", 
                       output_col="source_output",
                       handle_invalid = "skip") %>%
     ft_string_indexer(input_col="browser", 
                       output_col="browser_output",
                       handle_invalid = "skip") %>%
     ft_r_formula(purchase ~ .) %>% 
     ml_logistic_regression(elastic_net_param = list(alpha=1))



# specify the hyperparameter grid
# (parameter values to be considered in optimization)
ga_params <- list(logistic_regression=list(max_iter=80))

# create the cross-validator object
set.seed(1)
cv_lasso <- ml_cross_validator(sc,
                               estimator=ga_pipeline,
                               estimator_param_maps = ga_params,
                               ml_multiclass_classification_evaluator(sc),
                               num_folds = 5, 
                               parallelism = 2)

# train/fit the model
cv_lasso_fit <- ml_fit(cv_lasso, ga_spark)

# clean up
spark_disconnect(sc)





## Text analysis with Spark (on AWS EMR) ------------------------


# install additional packages
# install.packages("gutenbergr") # to download book texts from Project Gutenberg
# install.packages("dplyr") # for the data preparatory steps

# load packages
library(sparklyr)
library(gutenbergr)
library(dplyr)

# fix vars
TELL <- "https://www.gutenberg.org/cache/epub/6788/pg6788.txt"

# connect rstudio session to cluster
sc <- spark_connect(master = "yarn")





# Data gathering and preparation
# fetch Schiller's Tell, load to cluster
tmp_file <- tempfile()
download.file(TELL, tmp_file)
raw_text <- readLines(tmp_file)
tell <- data.frame(raw_text=raw_text)
tell_spark <- copy_to(sc, tell, "tell_spark", overwrite = TRUE)

# data cleaning
tell_spark <- filter(tell_spark, raw_text!="")
tell_spark <- select(tell_spark, raw_text)
tell_spark <- mutate(tell_spark, 
                     raw_text = regexp_replace(raw_text, "[^0-9a-zA-Z]+", " "))


# split into words
tell_spark <- ft_tokenizer(tell_spark, 
                           input_col = "raw_text",
                           output_col = "words")


# remove stop-words
tell_spark <- ft_stop_words_remover(tell_spark,
                                    input_col = "words",
                                    output_col = "words_wo_stop")


# unnest words, combine in one row
all_tell_words <- mutate(tell_spark, 
                         word = explode(words_wo_stop))
compute(all_tell_words, "all_tell_words")

# final cleaning
all_tell_words <- select(all_tell_words, word)
all_tell_words <- filter(all_tell_words, 2<nchar(word))


# word count and store result in Spark memory
compute(count(all_tell_words, word), "wordcount_tell")









