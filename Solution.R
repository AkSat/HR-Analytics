library(dplyr)
library(caret)
library(ggplot2)
library(rpart)
library(rattle)
library(RColorBrewer)
library(ROSE)
library(irr)
library(gains)
library(plyr)



setwd("F:/Data Contests/AnalyticsVidya/Completed/HR Analytics/")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

test$is_promoted <- NA

data <- rbind(train,test)

str(data)

summary(data)



# ================== imputing for NA in previous_year_rating column ===========

summary(test)

t_na <- table(data$previous_year_rating,data$is_promoted)

prop.table(t_na,1)

get_na <- data[is.na(data$previous_year_rating),c("previous_year_rating","is_promoted")]

t_na1 <- get_na %>% group_by(is_promoted) %>% dplyr::summarise(total = n())

t_na1$rate <- t_na1$total/sum(t_na1$total)

data[is.na(data$previous_year_rating),"previous_year_rating"] <- 3



# ===================== imputing missing values for education ==================

levels(data$education)
ind1 <- grepl("^\\s*$",data$education)

data$education1 <- as.numeric(data$education)
str(data$education1)
unique(data$education1)

data$education1[data$education1==1 | data$education1==2] <- "Bachelor's"
data$education1[data$education1==3] <- "Below Secondary"
data$education1[data$education1==4] <- "Master's & above"

data$education1 <- as.factor(data$education1)

levels(data$education1)

ind1 <- which(grepl("^\\s*$",data$education1) == TRUE)

prop.table(table(data$education1,data$is_promoted),1)


names(data)
data <- data[,-4]




# renaming columns "KPIs_met..80." and "awards_won."

names(data)
names(data)[10] <- "KPIGReaterThan80perc"
names(data)[11] <- "awards_won"

str(data)

data$KPIGReaterThan80perc <- as.numeric(data$KPIGReaterThan80perc)

unique(data$awards_won)
data$awards_won <- as.numeric(data$awards_won)



summary(data)
names(data)
str(data)
unique(data$recruitment_channel)

data$is_promoted <- as.factor(data$is_promoted)




# =========================  bucketing age column ==========================
summary(data$age)
data$empStatus <- ifelse(data$age >= 20 & data$age < 29,"Fresher",
                         ifelse(data$age >= 29 & data$age < 33,"Lead",
                                ifelse(data$age >= 33 & data$age < 39,"Manager","Senior")))

data$empStatus <- as.factor(data$empStatus)


pcaData <- data






# For Principal Component Analysis =========================================
str(pcaData)

names(pcaData)
pcaData[,5] <- as.numeric(pcaData[,5])
pcaData[,6] <- as.numeric(pcaData[,6])
pcaData[,7] <- as.numeric(pcaData[,7])
pcaData[,9] <- as.numeric(pcaData[,9])
pcaData[,12] <- as.numeric(pcaData[,12])

str(pcaData)


ind1 <- which(is.na(pcaData$is_promoted))

pcaTrain <- pcaData[-ind1,]
pcaTest <- pcaData[ind1,]


# Factor Analysis / Principal Component Analysis

names(pcaTrain)
summary(pcaTrain)

fact1 <- factanal(scale(pcaTrain[,c(5:12)]),rotation = "varimax",factors = 4)
fact1

# Loadings : anything close to 1 or -1 indicates
# that the factor strongly influences the variable
# Here,
# Age and Length of service can be clubbed to denote joining age

# previous_year_rating and KPIGReaterThan80perc can be clubbed 
# to get total_performance

# avg_training_score column is required

# recruitment_channel is not significant




# Principal Component Analysis =============================
scaledData <- scale(pcaTrain[,c(5:12)],scale = TRUE)
pr <-  prcomp(scaledData)
summary(pr)

# considering only PC1, PC2 and PC3 as they have variance 
# greater than 1

pr$rotation

# in PC1 : age,length_service
# in PC2 : previous_year_rating, KPIGreaterThan80perc
# in PC3 : no_of_trainings,avg_training_score

pr$sdev

# plot(pr,type = "l")
# 
# biplot(pr,pc.biplot = TRUE)

# From the biplot we see that the following variables are linear:
# -> age, length_of_service
# -> average_training_score,awards_won,
#    KPIGReaterThan80perc, previous_year_rating

names(pcaTrain)
pcaTrain[25658,c(8,10)]
pcaTrain[21553,c(8,10)]

# Relative to PC2 =======================================================

# since previous_year_rating, KPIGReaterThan80perc are relative to PC2 axis,
# In positive axis, if we take an observation like 25658,21553 then, we see 
# Obs.No.       previous_year_rating    KPIGReaterThan80perc
# 25658                    4                    1
# 21553                    5                    1

pcaTrain[15632,c(8,10)]
# Obs.No.       previous_year_rating    KPIGReaterThan80perc
# 15632                    1                    0

pcaTrain[29373,c(8,10)]
# Obs.No.       previous_year_rating    KPIGReaterThan80perc
# 29373                    2                    0


# Relative to PC1 =======================================================
# age and length_of_service is on the positive side of the graph

pcaTrain[19881,c(7,9)]
#       age       length_of_service
# 1988  60                 30


pcaTrain[29903,c(7,9)]   
#        age length_of_service
# 29903  58                29


pcaTrain[39592,c(7,9)]
#        age length_of_service
# 29737  51                19



# since this is to the left of PC1, both decrease in magnitude .............

pcaTrain[32781,c(7,9)]    
#       age       length_of_service
# 32781  32                 6



# finding correlations ======================================
names(pcaTrain)

cor(pcaTrain[,c(5:12)])

cor(pcaTrain[,c(5:12)],as.numeric(pcaTrain[,13]))

pcaTrain[1989,c("avg_training_score","no_of_trainings")]
pcaTrain[19961,c("avg_training_score","no_of_trainings")]
pcaTrain[29373,c("avg_training_score","no_of_trainings")]
pcaTrain[21553,c("avg_training_score","no_of_trainings")]
pcaTrain[32871,c("avg_training_score","no_of_trainings")]



# Featured Engineering ==================================


# Derive the joining age .....................

data$joiningAge <- data$age - data$length_of_service

str(data)



# Derive total KPI ..................

data$totalKPI <- data$KPIGReaterThan80perc + data$previous_year_rating




# separating out the datasets ==================================

names(data)

ind1 <- which(is.na(data$is_promoted))

train1 <- data[-ind1,]
test1 <- data[ind1,]



# Visualization on descriptive analysis ========================


t1 <- train1 %>% filter(is_promoted == "1") %>% 
  select(previous_year_rating,is_promoted) %>%
  group_by(previous_year_rating) %>%
  dplyr::summarize(`Total Promoted` = n())

t2 <- train1 %>% group_by(previous_year_rating) %>%
  dplyr::summarize(Total_employees = n())


t2$`Total Promoted` <- t1$`Total Promoted`

t2$Perc_promoted <- t2$`Total Promoted` / t2$Total_employees * 100

t2

# it is observed from above descriptive summary, that employees who have had
# previous year ratings above 3 are given morepreference for promotion
# Those with ratings 5 are highly recommended

train1 %>% group_by(is_promoted) %>% dplyr::summarize(n())

with(train1,boxplot(is_promoted,avg_training_score))


summary(train1)

# ================ plotting graph

ggplot(data=train1,aes(avg_training_score, y = no_of_trainings,
                       color= as.numeric(no_of_trainings))) + 
  #scale_y_continuous(breaks = seq(1,10,1)) + geom_point() +
  geom_point()  +   
  facet_grid(is_promoted~.)

# ggplot(data=data[1:nrow(train),],aes(x=avg_mean_training_score,fill=is_promoted)) +
#   geom_histogram()













# =========================== creating balanced sample ========================

prop.table(table(train1$is_promoted))
set.seed(100)
# trainB <- ROSE(data = train1,formula = is_promoted~.,N=54808,
#                 seed = 1)$data

trainB <- ovun.sample(is_promoted ~ .,
                      data = train1, method = "under",N = 9000,seed=1)$data
prop.table(table(trainB$is_promoted))

str(trainB)

training <- trainB

names(training)

library(mltools)
library(data.table)

# one-hot encoding and scaling on training data ====================================

onehotData  <- training[,c(2:4,14,15)]
onehotRes <- one_hot(as.data.table(onehotData))
training <- cbind(training[,-c(2:5,14,15)],onehotRes)

names(training)



# one-hot encoding and scaling on test data ====================================

names(test1)
onehotData  <- test1[,c(2:4,14,15)]
onehotRes <- one_hot(as.data.table(onehotData))
test1 <- cbind(test1[,-c(2:5,14,15)],onehotRes)
names(test1)


# glm modelling alogorithm =============================

mod1 <- glm(formula = is_promoted~.,data = training[,-c(1,3:6)], family = "binomial")
summary(mod1)

mod2 <- step(mod1,direction = "both")
summary(mod2)




# partitioning the dataset into 80:20 split ==========================

set.seed(100)
sample_ind <- sample(1:nrow(training),0.8 * nrow(training),replace = FALSE)

trainSample <- training[sample_ind,]
testSample <- training[-sample_ind,]

prop.table(table(trainSample$is_promoted))




# testing the glm model on test data "test1" and checking accuracy on training
names(trainSample)

str(trainSample)

d1 <- trainSample[,-c(1,9)]
names(d1)
scaledData <- scale(d1,center = TRUE,scale = TRUE)


scaledData$is_promoted <- trainSample$is_promoted

names(scaledData)



mod1 <- glm(formula = is_promoted~.,data = trainSample[,-1], family = "binomial")
summary(mod1)

mod2_scale <- step(mod1,direction = "both") 
summary(mod2_scale) 

mod3_scale <- glm(formula = is_promoted ~ no_of_trainings + age + previous_year_rating + 
                    length_of_service + KPIGReaterThan80perc + awards_won + avg_training_score + 
                    department_Analytics + department_Finance + department_HR + 
                    department_Legal + department_Operations + department_Procurement + 
                    `department_R&D` + `department_Sales & Marketing` +  
                    region_region_17 + region_region_2 + region_region_22 + 
                    region_region_23 + region_region_25 + region_region_28 + 
                    region_region_4 + region_region_7 + `education1_Bachelor's` + 
                    `education1_Below Secondary` + region_region_32 + 
                    region_region_11 + region_region_8 , family = "binomial", 
                  data = trainSample[,-1])

summary(mod3_scale)


spredict <- predict(mod3_scale, type = 'response')

prop.table(table(trainSample$is_promoted))
prop.table(table(trainB$is_promoted))


spredict_new <- ifelse(spredict >= 0.5186667,1,0)

kappa2(data.frame(spredict_new,trainSample$is_promoted))

confusionMatrix(table(spredict_new,trainSample$is_promoted),positive = "1")


#Check model performance - Ideally you need to do it on a cross validation test dataset dereived from train dataset
library(ROCR)
ROCRpred <- prediction(spredict, trainSample$is_promoted)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf)
abline(0,1)

ROCRauc <- performance(ROCRpred, 'auc')
ROCRauc@y.values


library(car)
vif(mod5)


predicted <- predict(mod3_scale,type = "response", newdata = test1)
test1$is_promoted <- ifelse(predicted >= 0.5186667,1,0)
solution_glm <- data.frame(employee_id=test1$employee_id,is_promoted=test1$is_promoted)
write.csv(solution_glm,"submission_glm_solFinal_14_scale_1.csv",row.names = FALSE)


