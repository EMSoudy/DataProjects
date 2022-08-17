library(readr)
library(readxl)
library(dplyr)
library(outliers)
library(psych)



PROTHROM<-
read_csv("BHIS575/LDS_C07_PROTHROM.csv")  
pro<-PROTHROM


pro_S30<-sample_n(pro,30)
View(pro_S30)

ps<-sample_n(pro, 30)
View(ps)

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
     Sample of 30",
     col = "orange red 4")

#histograms for TIME
hist(pro_S30$TIME_FULL_S30, 
     main = "Histogram for TIME FULLSUB
     Sample of 30",
     col = "aquamarine")

hist(pro_S30$TIME_PRE_S30, 
     col = "forest green",
     main = "Histogram for TIME PRESUB
     Sample of 30")

#boxplots for TIME
boxplot(pro_S30$TIME_FULL_S30,pro_S30$TIME_PRE_S30,
        col = "slate grey",
        main = "Boxplots for TIME FULLSUB and TIME PRESUB 
        Sample of 30")

#Grubbs Test
grubbs.test(pro_S30$TIME_FULL_S30)

grubbs.test(pro_S30$TIME_PRE_S30)


#T.test
t.test(pro_S30$TIME_FULL_S30,pro_S30$TIME_PRE_S30)

qt(0.975,908.2)
qt(0.025,908.2)

pt(q = -5.9411, df = 50.869) * 2

#HEMOGLOB

HEMOGLOB<-
  read_csv("BHIS575/LDS_C07_HEMOGLOB.csv")  
h_S30<-HEMOGLOB


h_S30<-sample_n(h_S30,30)
View(h_S30)

#Summaries
summary(h_S30$Hb_A_S30)
summary(h_S30$Hb_B_S30)

#reformat for readability
describe(h_S30$Hb_A_S30, trim = 0, interp = FALSE, ranges = TRUE,
         IQR = TRUE, quant = c(0.25,0.75))

describe(h_S30$Hb_B_S30, trim = 0, interp = FALSE, ranges = TRUE,
         IQR = TRUE, quant = c(0.25,0.75))

#plots for Hb A&B sample of 30
plot(h_S30$Hb_A_S30, 
     main = "Plot Graph for Hb A
     Sample of 30",
     col = "navy")

plot(h_S30$Hb_B_S30, 
     main = "Plot Graph for Hb B
     Sample of 30",
     col = "dark red")

#histograms for Hb A&B sample of 30
hist(h_S30$Hb_A_S30, 
     main = "Histogram for Hb A
     Sample of 30",
     col = "turquoise")

hist(h_S30$Hb_B_S30, 
     col = "magenta 4",
     main = "Histogram for Hb B 
     Sample of 30")

#boxplots for Hb A&B sample of 30
boxplot(h_S30$Hb_A_S30,h_S30$Hb_B_S30,
        col = "lavender",
        main = "Boxplots for TIME FULLSUB and TIME PRESUB 
        in Samples of 30")

#Grubbs Test
grubbs.test(h_S30$Hb_A_S30)

grubbs.test(h_S30$Hb_B_S30)

#T.test
t.test(h_S30$Hb_A_S30,h_S30$Hb_B_S30)

outlier(h_S30$Hb_A_S30)


#TOTAL POPULATION (PROTHROMBIN)
describe(pro$TIME_FULL, trim = 0, interp = FALSE, ranges = TRUE,
  IQR = TRUE, quant = c(0.25,0.75))

describe(pro$TIME_PRE, trim = 0, interp = FALSE, ranges = TRUE,
  IQR = TRUE, quant = c(0.25,0.75))

t.test(pro$TIME_FULL, pro$TIME_PRE)

boxplot(pro$TIME_FULL, pro$TIME_PRE, 
        col = "tan", 
        main = " Boxplots for TIME FULLSUB and TIME PRESUB ")

hist(pro$TIME_FULL, 
     col = "yellow", 
     main = "Histogram for TIME FULLSUB")
hist(pro$TIME_PRE,
col = "light green", 
main = "Histogram for TIME PRESUB")


#TOTAL POPULATION (HEMOGLOB)
describe(HEMOGLOB$,trim = 0, interp = FALSE, ranges = TRUE,
         IQR = TRUE, quant = c(0.25,0.75))

describe(HEMOGLOB$Hb_B)

t.test(HEMOGLOB$Hb_A, HEMOGLOB$Hb_B)

boxplot(HEMOGLOB$Hb_A, HEMOGLOB$Hb_B, 
        col = "magenta", 
        main = " Boxplots for Hb_A and Hb_B ")

hist(HEMOGLOB$Hb_A, 
     col = "orange", 
     main = "Histogram for Hb_A")
hist(HEMOGLOB$Hb_B,
     col = "light blue", 
     main = "Histogram for Hb_B")

