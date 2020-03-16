# preparation
#------------#
rm(list = ls())
gc()
set.seed(1)

sink(file = "PS1_6.log", split = TRUE)

options(scipen=100) 

library(dplyr)

#Set Sample Sizes
sample_size <- 800

#Create Sample Dataset
dataset <- runif(sample_size, min=0, max=1)
dataset <- dplyr::tbl_df(dataset)
colnames(dataset) = c("d_uni")
dataset <- dplyr::mutate(dataset, 
                         d = ifelse(d_uni < 0.4, 1, 0),
                         y_uni = runif(sample_size, min=0, max=1),
                         y_0 = ifelse(d == 1,
                                      ifelse(y_uni > 0.5, 1, 0),
                                      ifelse(y_uni > 0.5, 1, 0)),
                         y_1 = ifelse(d == 1,
                                      ifelse((y_uni > 0.1 & y_uni <= 0.5) | (y_uni > 0.7), 1, 0),
                                      ifelse((y_uni > 0.25 & y_uni <= 0.5) | (y_uni > 0.75), 1, 0)),
                         d_rct_uni = runif(sample_size, min=0, max=1),
                         d_rct = ifelse(d_rct_uni < 0.5, 1, 0),
                         y =ifelse(d_rct == 1, y_1, y_0)
                         )
  
#(b)
lm_b <- lm(data = dataset, formula = y ~ d_rct)
summary(lm_b)

#(c)
lm_c <- lm(data = filter(dataset, d==1), formula = y ~ d_rct)
summary(lm_c)


sink()
