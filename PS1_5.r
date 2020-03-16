#------------#
# preparation
#------------#
rm(list = ls())
gc()
set.seed(1)

sink(file = "PS1_5.log", split = TRUE)

options(scipen=100)

library(dplyr)

#Set Sample Sizes
sample_size <- c(50, 200, 800)
hist <- list()

#Loop for three sample sizes
for (j in 1:length(sample_size)) {
  
  #Store 1000 OLS Estimates
  beta_hats <- c()
  
  #Run OLS Regression 1000times
  for (i in 1:1000) {
    #Create Sample Dataset
    dataset <- runif(sample_size[j], min=0, max=1)
    dataset <- dplyr::tbl_df(dataset)
    colnames(dataset) = c("d_uni")
    dataset <- dplyr::mutate(dataset, 
                             d = ifelse(d_uni < 0.4, 1, 0),
                             y_uni = runif(sample_size[j], min=0, max=1),
                             y = ifelse(d == 1,
                                        ifelse(y_uni < 0.7, 1, 0),
                                        ifelse(y_uni < 0.5, 1, 0)))
    #Run OLS Regression
    lm <- lm(data = dataset, formula = y ~ d)
    beta_hats[i] <-  lm$coefficients["d"]
  }
  
  #Plot 1000 OLS Estimates
  hist[[j]] <- hist(beta_hats, breaks = c(seq(-0.5, 1, 0.05)))
  
  png(filename=paste("PS1_5_Sample_Size_", sample_size[j], ".png", sep=""), width=500, height=400)
  plot(hist[[j]]$mids,hist[[j]]$density, type ="l", xlab = "OLS Estimates on D", ylab = "Density", col = "blue", main = paste("Sample Size ", sample_size[j],  sep=""))
  dev.off()
}

#Plot 3 Graphs
png(filename="PS1_5_OLS_dist_Sample_Sizes.png", width=500, height=400)
for (j in 1:length(sample_size)) {
  plot(hist[[j]]$mids,hist[[j]]$density, type ="l", main = "The Change of Estimates Distribution for Each Sample Sizes",
       xlab = "OLS Estimates on D", ylab = "Density", 
       xlim = c(-0.5, 1), ylim = c(0, 10), 
       col = j+1, ann = (j == length(sample_size)))
  par(new=T)
}
dev.off()

sink()
