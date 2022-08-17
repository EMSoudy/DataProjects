library(readr)
library(readxl)
library(dplyr)
library(outliers)
library(psych)

PROTHROM<-
  read_csv("BHIS575/pro_S30.csv") 


#Grubbs Test
grubbs.test(pro_S30$TIME_FULL_S30)

grubbs.test(pro_S30$TIME_PRE_S30)

#summaries for time
summary(pro_S30$TIME_FULL_S30)
summary(pro_S30$TIME_PRE_S30)

#reformat for readability 
describe(pro_S30$TIME_FULL_S30, trim = 0, interp = FALSE, ranges = TRUE,
         IQR = TRUE, quant = c(0.25,0.75))

describe(pro_S30$TIME_PRE_S30, trim = 0, interp = FALSE, ranges = TRUE,
         IQR = TRUE, quant = c(0.25,0.75))

#plots for TIME
plot(pro_S30$TIME_FULL_S30, 
     main = "Plot Graph for TIME FULLSUB
     Sample of 30",
     col = "magenta 4")

plot(pro_S30$TIME_PRE_S30, 
     main = "Plot Graph for TIME PRESUB
     Sample of 29",
     col = "orange red 4")

#histograms for TIME
hist(pro_S30$TIME_FULL_S30, 
     main = "Histogram for TIME FULLSUB
     Sample of 30",
     col = "aquamarine")

hist(pro_S30$TIME_PRE_S30, 
     col = "forest green",
     main = "Histogram for TIME PRESUB
     Sample of 29")

#boxplots for TIME
boxplot(pro_S30$TIME_FULL_S30,pro_S30$TIME_PRE_S30,
        col = "slate grey",
        main = "Boxplots for TIME FULLSUB and TIME PRESUB 
        Randomized Samples")

#T.test
t.test(pro_S30$TIME_FULL_S30,pro_S30$TIME_PRE_S30)

qt(0.975,908.2)
qt(0.025,908.2)

pt(q = -5.9411, df = 50.869) * 2

