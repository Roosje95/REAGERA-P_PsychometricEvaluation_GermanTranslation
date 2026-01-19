############r_additional_file_6#############
## R Script for the analysis of Santi et al. Translation and validation Reagrea scale swiss-german
## In case of question contact: sonja.santi@icloud.com 

## Set-up: if necessary install and always load required R-packages
#install.packages("AICcmodavg", "broom", "data.table", "dplyr", EnvStats", "ggplot2", "ggpubr", "lavaan", psych",  "reshape2", "readxl", "semPlot", "tidyverse")

library(AICcmodavg)
library(broom)
library(data.table)
library(dplyr)
library(EnvStats)
library(ggplot2)
library(ggpubr)
library(lavaan)
library(psych)
library(reshape2)
library(readxl)
library(semPlot)
library(tidyverse)

### TO ADAPT ###
REAGERA_P_raw <- read_excel("H:/Lehre/Supervision/CHG - Sonja/Submission/Additional file 7.xlsx")

##### descriptive statistics #####
#sex
table(REAGERA_P_raw$geschlecht)
round(prop.table(table(REAGERA_P_raw$geschlecht))*100,2)

#age
table(REAGERA_P_raw$alter)
round(prop.table(table(REAGERA_P_raw$alter))*100,2)

#profession
table(REAGERA_P_raw$beruf)
round(prop.table(table(REAGERA_P_raw$beruf))*100,2)

#profession 3.1
table(REAGERA_P_raw$beruf_d_1)
round(prop.table(table(REAGERA_P_raw$beruf_d_1))*100,2)

#profession 3.2
table(REAGERA_P_raw$beruf_d_2)
round(prop.table(table(REAGERA_P_raw$beruf_d_2))*100,2)

#current profession
table(REAGERA_P_raw$berufjahre)
round(prop.table(table(REAGERA_P_raw$berufjahre))*100,2)

#current workplace
table(REAGERA_P_raw$arbeitgeberjahre)
round(prop.table(table(REAGERA_P_raw$arbeitgeberjahre))*100,2)

#inpatientcare/outpatientcare
table(REAGERA_P_raw$statamb)
round(prop.table(table(REAGERA_P_raw$statamb))*100,2)

#education AOT
table(REAGERA_P_raw$ausbgia)
round(prop.table(table(REAGERA_P_raw$ausbgia))*100,2)

#further education AOT
table(REAGERA_P_raw$wbgia)
round(prop.table(table(REAGERA_P_raw$wbgia))*100,2)

#regional guidelines
table(REAGERA_P_raw$ktri)
round(prop.table(table(REAGERA_P_raw$ktri))*100,2)

#written guidelines work
table(REAGERA_P_raw$betriebri)
round(prop.table(table(REAGERA_P_raw$betriebri))*100,2)

#Question AOT work
table(REAGERA_P_raw$vorgesetzte)
round(prop.table(table(REAGERA_P_raw$vorgesetzte))*100,2)

#help from eachother work
table(REAGERA_P_raw$arbeithilf)
round(prop.table(table(REAGERA_P_raw$arbeithilf))*100,2)

####### statistical Tests: Convergent Validity ######
# drop rows with NA value
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$perserf_spont),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sw_1),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sw_2),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sw_3),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sw_4),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sw_5),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sw_6),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sw_7),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sw_8),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sorg_nachsorge),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sorg_negativfrage),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sorg_negativbeeinfluss),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$Bereit_Gesell),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$Bereit_Arbei),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$Bereit_Dok),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$Bereit_Kennt),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$hinder_wend),]

# decrease by 1 to have values between 0 and 10
REAGERA_P_raw$sw_1 <- REAGERA_P_raw$sw_1 - 1
REAGERA_P_raw$sw_2 <- REAGERA_P_raw$sw_2 - 1
REAGERA_P_raw$sw_3 <- REAGERA_P_raw$sw_3 - 1
REAGERA_P_raw$sw_4 <- REAGERA_P_raw$sw_4 - 1
REAGERA_P_raw$sw_5 <- REAGERA_P_raw$sw_5 - 1
REAGERA_P_raw$sw_6 <- REAGERA_P_raw$sw_6 - 1
REAGERA_P_raw$sw_7 <- REAGERA_P_raw$sw_7 - 1
REAGERA_P_raw$sw_8 <- REAGERA_P_raw$sw_8 - 1

# decrease by 1 to have values between 0 and 3
REAGERA_P_raw$sorg_nachsorge <- REAGERA_P_raw$sorg_nachsorge - 1
REAGERA_P_raw$sorg_negativbeeinfluss <- REAGERA_P_raw$sorg_negativbeeinfluss - 1
REAGERA_P_raw$sorg_negativfrage <- REAGERA_P_raw$sorg_negativfrage - 1

# decrease by 1 to have values between 0 and 4
REAGERA_P_raw$Bereit_Dok <- REAGERA_P_raw$Bereit_Dok - 1
REAGERA_P_raw$Bereit_Kennt <- REAGERA_P_raw$Bereit_Kennt - 1

# decrease by 1 to have values between 0 and 1
REAGERA_P_raw$hinder_wend <- REAGERA_P_raw$hinder_wend - 1

# decrease by 1 to have values between 0 and 5
REAGERA_P_raw$hinder_1 <- REAGERA_P_raw$hinder_1 - 1

##### I: Those with a positive experience of talking to patients about abuse have higher self-efficacy. ######

# add all the questions for self_efficacy
REAGERA_P_raw$sw_1_to_8 <- REAGERA_P_raw$sw_1 + REAGERA_P_raw$sw_2 + REAGERA_P_raw$sw_3 + REAGERA_P_raw$sw_4 + REAGERA_P_raw$sw_5 + REAGERA_P_raw$sw_6 + REAGERA_P_raw$sw_7 + REAGERA_P_raw$sw_8

REAGERA_P_raw$sw_1_to_3 <- REAGERA_P_raw$sw_1 + REAGERA_P_raw$sw_2 + REAGERA_P_raw$sw_3

REAGERA_P_raw$sw_4_to_8 <- REAGERA_P_raw$sw_4 + REAGERA_P_raw$sw_5 + REAGERA_P_raw$sw_6 + REAGERA_P_raw$sw_7 + REAGERA_P_raw$sw_8

#grouping No Experience(0)/Yes Experience (1)

REAGERA_P_raw[REAGERA_P_raw$perserf_spont > 1, "perserf_group"] <- 1 
REAGERA_P_raw[REAGERA_P_raw$perserf_spont == 1, "perserf_group"] <- 0

# 94 persons
REAGERA_P_raw_expyes <- REAGERA_P_raw[REAGERA_P_raw$perserf_group == 1,]

# 140 persons
REAGERA_P_raw_expno <- REAGERA_P_raw[REAGERA_P_raw$perserf_group == 0,]

paste("mean of self-efficacy of people with  experience: ", mean(REAGERA_P_raw_expyes$sw_1_to_8))

paste("mean of self-efficacy of people with no experience: ", mean(REAGERA_P_raw_expno$sw_1_to_8))


#overall SE
t.test(REAGERA_P_raw$sw_1_to_8 ~ REAGERA_P_raw$perserf_group)

#asking question SE
t.test(REAGERA_P_raw$sw_1_to_3 ~ REAGERA_P_raw$perserf_group)

#managing response SE
t.test(REAGERA_P_raw$sw_4_to_8 ~ REAGERA_P_raw$perserf_group)

#Graphs Test I
#figure 2
boxplot(REAGERA_P_raw$sw_1_to_8~REAGERA_P_raw$perserf_group,
        xlab = "Experience", ylab = "SE overall")
#figure 3
boxplot(REAGERA_P_raw$sw_1_to_3~REAGERA_P_raw$perserf_group,
        xlab = "Experience", ylab = "SE asking questions")
#figure 4
boxplot(REAGERA_P_raw$sw_4_to_8~REAGERA_P_raw$perserf_group,
        xlab = "Experience", ylab = "SE asking questions")

##### Test II: Those with a positive experience of talking to patients about abuse have less concern than those reporting negative or no experience at all.#####

#grouping No Experience(0)/Yes Experience (1)
REAGERA_P_raw[REAGERA_P_raw$perserf_spont > 1, "perserf_group"] <- 1 
REAGERA_P_raw[REAGERA_P_raw$perserf_spont == 1, "perserf_group"] <- 0

# follow-up, Item 18 (Table 1)
x1 <- REAGERA_P_raw$perserf_group
y1 <- as.factor(REAGERA_P_raw$sorg_nachsorge)

table_chi_square <- table(x1, y1)

chisq.test(table_chi_square)

#Table 5, Item 18

xtabs(~REAGERA_P_raw$sorg_nachsorge+REAGERA_P_raw$perserf_group)
colPerc(xtabs(~REAGERA_P_raw$sorg_nachsorge+REAGERA_P_raw$perserf_group))


# negative reactionm Item 19 (Table 1)
x2 <- REAGERA_P_raw$perserf_group
y2 <- as.factor(REAGERA_P_raw$sorg_negativfrage)

table_chi_square2 <- table(x2, y2)

chisq.test(table_chi_square2)

#Table 5, Item 19

xtabs(~REAGERA_P_raw$sorg_negativfrage+REAGERA_P_raw$perserf_group)
colPerc(xtabs(~REAGERA_P_raw$sorg_negativfrage+REAGERA_P_raw$perserf_group))

# negative impact Item 20 (Table 1)
x3 <- REAGERA_P_raw$perserf_group
y3 <- as.factor(REAGERA_P_raw$sorg_negativbeeinfluss)

table_chi_square3 <- table(x3, y3)

chisq.test(table_chi_square3)

#Table 5, Item 20
xtabs(~REAGERA_P_raw$sorg_negativbeeinfluss+REAGERA_P_raw$perserf_group)
colPerc(xtabs(~REAGERA_P_raw$sorg_negativbeeinfluss+REAGERA_P_raw$perserf_group))


##### Test III: SE is negatively correlates with concern, i.e., repondents with strong SE report less concern #####

# add all the questions for self_efficacy in managing abuse
REAGERA_P_raw$sw_1_to_8 <- REAGERA_P_raw$sw_1 + REAGERA_P_raw$sw_2 + REAGERA_P_raw$sw_3 + REAGERA_P_raw$sw_4 + REAGERA_P_raw$sw_5 + REAGERA_P_raw$sw_6 + REAGERA_P_raw$sw_7 + REAGERA_P_raw$sw_8

# Spearman-Correlation: concern not good follow-up
cor(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$sorg_nachsorge, method = "spearman")
cor.test(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$sorg_nachsorge, method = "spearman")

#Plot: Spearman-Correlation: concern not good follow-up
plot(REAGERA_P_raw$sorg_nachsorge, REAGERA_P_raw$sw_1_to_8, method = "spearman", main="Test III - Follow-up",
     xlab="Self-Efficacy", ylab="Follow-Up", pch=1) 

# Spearman-Correlation:concern negative reaction
cor(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$sorg_negativfrage, method = 'spearman')
cor.test(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$sorg_negativfrage, method = 'spearman')

#Plot: Spearman-Correlation: negative reaction
plot(REAGERA_P_raw$sorg_negativfrage, REAGERA_P_raw$sw_1_to_8, method = "spearman", main="Test III - Negative Reaction",
     xlab="Self-Efficacy", ylab="Negative Reaction", pch=1) 

# concern negative impact
cor(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$sorg_negativbeeinfluss, method = 'spearman')
cor.test(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$sorg_negativbeeinfluss, method = 'spearman')

#Plot: Spearman-Correlation: negative impact
plot(REAGERA_P_raw$sorg_negativbeeinfluss, REAGERA_P_raw$sw_1_to_8, method = "spearman", main="Test III - Negative Impact",
     xlab="Self-Efficacy", ylab="Negative Impact", pch=1) 

##### Test IV & V: personal experience vs. awareness of preparedness society/clinic #####

REAGERA_P_raw[REAGERA_P_raw$perserf_spont > 1, "perserf_group"] <- 1 
REAGERA_P_raw[REAGERA_P_raw$perserf_spont == 1, "perserf_group"] <- 0

# Test IV: chi2, preparedness of society
x1 <- REAGERA_P_raw$perserf_group
y1 <- as.factor(REAGERA_P_raw$Bereit_Gesell)

table_chi_square <- table(x1, y1)

chisq.test(table_chi_square)

#Table: Test IV, Table 5

xtabs(~REAGERA_P_raw$Bereit_Gesell+REAGERA_P_raw$perserf_group)
colPerc(xtabs(~REAGERA_P_raw$Bereit_Gesell+REAGERA_P_raw$perserf_group))

##### Test V: chi2 preparedness of workplace #####

REAGERA_P_raw[REAGERA_P_raw$perserf_spont > 1, "perserf_group"] <- 1 
REAGERA_P_raw[REAGERA_P_raw$perserf_spont == 1, "perserf_group"] <- 0

x1 <- REAGERA_P_raw$perserf_group
y1 <- as.factor(REAGERA_P_raw$Bereit_Arbei)

table_chi_square <- table(x1, y1)

chisq.test(table_chi_square)

#Table: Test V, Table 5

xtabs(~REAGERA_P_raw$Bereit_Arbei+REAGERA_P_raw$perserf_group)
colPerc(xtabs(~REAGERA_P_raw$Bereit_Arbei+REAGERA_P_raw$perserf_group))

###### Test VI,VII,VIII: preparation #####

# add all the questions for self_efficacy
REAGERA_P_raw$sw_1_to_8 <- REAGERA_P_raw$sw_1 + REAGERA_P_raw$sw_2 + REAGERA_P_raw$sw_3 + REAGERA_P_raw$sw_4 + REAGERA_P_raw$sw_5 + REAGERA_P_raw$sw_6 + REAGERA_P_raw$sw_7 + REAGERA_P_raw$sw_8

##### Test VI: A positive correlation is expected between self-efficacy and perceived capability of documenting in a correct way.#####
cor(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$Bereit_Dok, method = "spearman")
cor.test(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$Bereit_Dok, method = "spearman")

#####Plot: Test VI, Spearman-Correlation: SE - Capability of documenting correctly ######
plot(REAGERA_P_raw$Bereit_Dok, REAGERA_P_raw$sw_1_to_8, method = "spearman", main="Test VI - Documentation",
     xlab="Self-Efficacy", ylab="Capability of documenting correctly", pch=1)

#####Test VII: A positive correlation is expected between self-efficacy and perceived knowledge about legislation.#####
cor(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$Bereit_Kennt, method = "spearman")
cor.test(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$Bereit_Kennt, method = "spearman")

#Plot: Test VII, Spearman-Correlation: SE -Perceived knowledge about legislation
plot(REAGERA_P_raw$Bereit_Kennt, REAGERA_P_raw$sw_1_to_8, method = "spearman", main="Test VII - Legislation Knowledge",
     xlab="Self-Efficacy", ylab="Perceived Knowldge about Legislation", pch=1)

##### Test VIII, A positive correlation is expected between self-efficacy and perceived collegial support.######

t.test(REAGERA_P_raw$sw_1_to_8 ~ REAGERA_P_raw$hinder_wend)

#Plot: Test VIII, Figure 10, Boxplot Collegial Support
boxplot(REAGERA_P_raw$sw_1_to_8~REAGERA_P_raw$hinder_wend,
        xlab = "Perceived Collegial Support", ylab = "Self Efficacy")

##### Test IX: Responders who state to have sufficient time to ask questions have higher self-efficacy in asking questions ######

# add all the questions for self_efficacy
REAGERA_P_raw$sw_1_to_8 <- REAGERA_P_raw$sw_1 + REAGERA_P_raw$sw_2 + REAGERA_P_raw$sw_3 + REAGERA_P_raw$sw_4 + REAGERA_P_raw$sw_5 + REAGERA_P_raw$sw_6 + REAGERA_P_raw$sw_7 + REAGERA_P_raw$sw_8

# lack of time
correlation_self_efficacy__lack_of_time <- cor.test(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$hinder_1, method = 'spearman')
paste("correlation coefficient: ", correlation_self_efficacy__lack_of_time$estimate)
paste("p-value: ", correlation_self_efficacy__lack_of_time$p.value)

#Test XI: Spearman Correlation lack of time - SE
cor(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$hinder_1, method = "spearman")
cor.test(REAGERA_P_raw$sw_1_to_8, REAGERA_P_raw$hinder_1, method = "spearman")

#Plot: Test XI, Spearman Correlation lack of time - SE
plot(REAGERA_P_raw$hinder_1, REAGERA_P_raw$sw_1_to_8, method = "spearman", main="Test XI - Lack of Time",
     xlab="Self-Efficacy", ylab="Lack of time", pch=1)

####### Confirmatory Factor Analysis SE-Scale #####-------------

REAGERA_sw <- data.frame(REAGERA_P_raw$sw_1, REAGERA_P_raw$sw_2, REAGERA_P_raw$sw_3, REAGERA_P_raw$sw_4, REAGERA_P_raw$sw_5, REAGERA_P_raw$sw_6, REAGERA_P_raw$sw_7, REAGERA_P_raw$sw_8)

#Checking Suitability of FA
describe(REAGERA_sw)
pairs.panels(REAGERA_sw)

R <- cor(na.omit(REAGERA_sw))
KMO(R)


# Specifying the model for confirmatory factor analysis - e.g. results simmons et al. 2021
SW.model <- ' factor1  =~ REAGERA_P_raw.sw_1 + REAGERA_P_raw.sw_2 + REAGERA_P_raw.sw_3      
              factor2 =~ REAGERA_P_raw.sw_4 + REAGERA_P_raw.sw_5 + REAGERA_P_raw.sw_6 + REAGERA_P_raw.sw_7 + REAGERA_P_raw.sw_8'

# fit the model
fit_cfa <- cfa(SW.model, data = REAGERA_sw)
fit_efa <- efa(data = REAGERA_sw, nfactors = 1:4)

summary(fit_cfa,fit.measures=T)

summary(fit_efa,fit.measures=T)

#Cronbach's alpha - Self-efficacy

alpha(subset(X24_11_15_Erhebung_REAGERA_P_29_August_2024_nummerisch, select = c(sw_1, sw_2, sw_3, sw_4, sw_5, sw_6, sw_7, sw_8)), check.keys =TRUE)

#Cronbach's Alpha - Case Vignette

alpha(subset(X24_11_15_Erhebung_REAGERA_P_29_August_2024_nummerisch, select = c(meiereinsatz, meierschmerz, meiermedi, meiersohn, meierhämat)), check.keys =TRUE)

###### Confirmatory Factor Analysis - Self-efficacy ######
#https://datatab.de/tutorial/faktorenanalyse--> helpful
#Kaiser's Kriterium non is more than one? so can we even make a FA?

# drop rows with NA value
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$perserf_spont),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sorg_nachsorge),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sorg_negativfrage),]
REAGERA_P_raw <- REAGERA_P_raw[!is.na(REAGERA_P_raw$sorg_negativbeeinfluss),]


mydata_FA_sw <- data.frame(REAGERA_P_raw$sw_1, REAGERA_P_raw$sw_2, REAGERA_P_raw$sw_3, REAGERA_P_raw$sw_4, REAGERA_P_raw$sw_5, REAGERA_P_raw$sw_6, REAGERA_P_raw$sw_7, REAGERA_P_raw$sw_8)

# Check if data is suitable for factor analysis
describe(mydata_FA_sw) #check min/max average is all ok --> okay
pairs.panels(mydata_FA_sw) #check histogramms (normally distributed?) and correlations(not too high? <0,6) --> should be okay. not ev'rything ist normally distributed, but okay

R <- cor(na.omit(mydata_FA_sw))
KMO(R) # should be 8 or above - https://www.statisticshowto.com/kaiser-meyer-olkin/ --> two ar 0.76, but is okay like that


# specify the model for confirmatory factor analysis - e.g. results simmons et al. 2021
SW.model <- ' factor1  =~ REAGERA_P_raw.sw_1 + REAGERA_P_raw.sw_2 + REAGERA_P_raw.sw_3      
              factor2 =~ REAGERA_P_raw.sw_4 + REAGERA_P_raw.sw_5 + REAGERA_P_raw.sw_6 + REAGERA_P_raw.sw_7 + REAGERA_P_raw.sw_8'

# Example: Two-factor model
model <- '
  # Latent variables and their indicators
  F1 =~ REAGERA_P_raw.sw_1 + REAGERA_P_raw.sw_2 + REAGERA_P_raw.sw_3
  F2 =~ REAGERA_P_raw.sw_4 + REAGERA_P_raw.sw_5 + REAGERA_P_raw.sw_6 + REAGERA_P_raw.sw_7 + REAGERA_P_raw.sw_8

  # Factor covariance (optional, if factors are correlated)
  F1 ~~ F2
'


# fit the model
fit_cfa <- cfa(model, data = mydata_FA_sw)
fit_efa <- efa(data = mydata_FA_sw, nfactors = 1:4)

summary(fit_cfa,fit.measures=T) #here you can see a high covariation between the two factors
# just as thump rules: CFI >0,95, RMSEA<0,06, SRMR<0,08 would indicate a well fitted model - this is with the 2-factor model not the case
summary(fit_efa,fit.measures=T) #here you can see that with 1 or 2 factors the model reaches significance


#here you can see the eigenvalues with the "knick" in the graph at 2, only the first factor has an eigenvalue of above 1
fa.parallel(mydata_FA_sw, fa='fa')

semPlot::semPaths(fit_cfa, what = "std", layout = "tree",
                  edge.label.cex = 0.8, # Adjust label size
                  node.label.cex = 1.0) # Adjust node size

#as you can see here your approach leads to same results and as fitting efa with 2 factors
FA2_SW<-fa(mydata_FA_sw, nfactors=2, rotate = "oblimin", fm="ml")
summary(FA2_SW,fit.measures=T)
fa.diagram(FA2_SW)

FA3_SW<-fa(REAGERA_sw, nfactors=3, rotate = "oblimin", fm="ml")
summary(FA3_SW,fit.measures=T)
fa.diagram(FA3_SW)
# here you see that the 3 factor option have TLI>0,95 and RMSEA<0,06 - considering model fit criteria this one as shown also above woudl be best



##### Cronbach's alpha - Self-efficacy #####
#overall SE
mydata_FA_sw <- data.frame(REAGERA_P_raw$sw_1, REAGERA_P_raw$sw_2, REAGERA_P_raw$sw_3, REAGERA_P_raw$sw_4, REAGERA_P_raw$sw_5, REAGERA_P_raw$sw_6, REAGERA_P_raw$sw_7, REAGERA_P_raw$sw_8)
alpha(mydata_FA_sw, check.keys =TRUE)

#Asking Questions
mydata_FA_AQ<- data.frame(REAGERA_P_raw$sw_1, REAGERA_P_raw$sw_2, REAGERA_P_raw$sw_3)
alpha(mydata_FA_AQ, check.keys =TRUE)

#Managing Response
mydata_FA_MR<- data.frame(REAGERA_P_raw$sw_4, REAGERA_P_raw$sw_5, REAGERA_P_raw$sw_6, REAGERA_P_raw$sw_7, REAGERA_P_raw$sw_8)
alpha(mydata_FA_MR, check.keys =TRUE)
