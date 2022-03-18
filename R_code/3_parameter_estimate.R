### Ralfs lactat wert
rm(list = ls())
gc()

## install packages if needed (delete: #, if you need to install them again)
#install.packages(c("xlsx", "minpack.lm"))

## load packages we need
library(xlsx)
library(minpack.lm)
library(dplyr)
library(ggplot2)

## read the data
## since we can't fill something on a specific index in 
## R we need to create an empty vector before the for loop
dat_list <- c()

## we loop now over the process,
## in xlsx is a little argument called sheetIndex which we use to load the 4 sheets 
## (i becomes 1:4, over the iterations)

for(i in 1:4)
{
  dat_list[i] <- list(read.xlsx("C://Users/ma22buky/Documents/ralf_help/Bateman_Daten.xlsx", 
                                sheetIndex = i)) 
  dat_list[[i]]$proband <- i
}

## run the models and hope for the best
mod_list <- c()

##loop 
for(i in 1:length(dat_list))
{
  mod_list[i] <- list(nlsLM(Laktat_real ~ (A * k1/(k2 - k1)) * (exp(-k1 * Zeit_min) - 
                                                                  exp(-k2 * Zeit_min)) + dat_list[[i]][1,2], 
                            data = dat_list[[i]],
                            start = list(A = max(dat_list[[i]][,2]) - dat_list[[i]][1,2],
                                         k1 = max(dat_list[[i]]$Laktat_real - lag(dat_list[[i]]$Laktat_real), na.rm = T), 
                                         k2 = -1 * min(dat_list[[i]]$Laktat_real - lag(dat_list[[i]]$Laktat_real), na.rm = T))))
  
  ## I try to already incorporate the predicted values to the data frames from earlier
  dat_list[[i]]$predi <- predict(mod_list[[i]])
  
  
} 

summary(mod_list[[4]])

dat <- do.call("rbind", dat_list)

ggplot(dat, aes(x = Zeit_min, y = Laktat_real, color = as.character(proband))) + geom_point() +
  geom_line(dat, mapping = aes(x = Zeit_min, y = predi, color = as.character(probant)))








