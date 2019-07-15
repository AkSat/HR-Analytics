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



setwd("F:/Data Contests/AnalyticsVidya/Working on currently/HR Analytics/")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

test$is_promoted <- NA

data <- rbind(train,test)

str(data)

summary(data)


chkpr <- data[,7:13]
names(chkpr)





unique(data$no_of_trainings)

summary(data$previous_year_rating)






names(data)
names(data)[11] <- "KPIGReaterThan80perc"
names(data)[12] <- "awards_won"

data$KPIGReaterThan80perc1 <- ifelse(data$KPIGReaterThan80perc == 1,"Yes","No")
data$KPIGReaterThan80perc1 <- as.factor(data$KPIGReaterThan80perc1)

unique(data$awards_won)
data$awards_won1 <- ifelse(data$awards_won == 1,"Yes","No")
data$awards_won1 <- as.factor(data$awards_won1)

names(data)
data <- data[,-c(11,12)]



# ===================== avg_mean_score by department and region

getDeptRegTotal <- aggregate(data = data,avg_training_score ~ department + region,mean)

getTotal <- sapply(paste(data$department,data$region,sep="@"), 
                   function(x){
                     y <- unlist(strsplit(as.character(x),split = "@"))
                     dept = y[1]
                     reg = y[2]
                     result <- getDeptRegTotal$avg_training_score[getDeptRegTotal$department == dept & getDeptRegTotal$region == reg]
                     return(result)
                   })

data$deptReg_Total <- getTotal
data$avg_mean_score_dept_region <- data$avg_training_score / data$deptReg_Total


names(data)
data <- data[,-c(11,15)]



# =========================  bucketing age column ==========================
summary(train$age)
data$empStatus <- ifelse(data$age >= 20 & data$age < 29,"Fresher",
                  ifelse(data$age >= 29 & data$age < 33,"Lead",
                         ifelse(data$age >= 33 & data$age < 39,"Manager","Senior")))

data$empStatus <- as.factor(data$empStatus)




# ===================== start_age

data$start_age <- data$age - data$length_of_service

#data$work_fraction <- data$length_of_service/data$age

names(data)

data <- data[,-c(8,10)]



# ============== converting is_promoted to factor

data$is_promoted <- as.factor(data$is_promoted)




# ================== imputing for NA in previous_year_rating column ===========

summary(test)

# imputing in test

t_na <- table(data$previous_year_rating,data$is_promoted)

prop.table(t_na,1)

get_na <- data[is.na(data$previous_year_rating),c("previous_year_rating","is_promoted")]

t_na1 <- get_na %>% group_by(is_promoted) %>% dplyr::summarise(total = n())

t_na1$rate <- t_na1$total/sum(t_na1$total)

data[is.na(data$previous_year_rating),"previous_year_rating"] <- 3


summary(data)





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




# ======================== scaled avg_mean_score_dept_region

summary(data$avg_mean_score_dept_region)
score_mean <- mean(data$avg_mean_score_dept_region)
score_sd <- sd(data$avg_mean_score_dept_region)

score_max <- max(data$avg_mean_score_dept_region)
score_min <- min(data$avg_mean_score_dept_region)

newMax <- 1
newMin <- 0

norm1 <- (data$avg_mean_score_dept_region - score_min)/ (score_max - score_min)

data$scaledScoreDeptReg <- norm1

names(data)
data <- data[,-11]



# ========================== separating out the datasets =======================


ind1 <- which(is.na(data$is_promoted))

train1 <- data[-ind1,]
test1 <- data[ind1,]





# =========================== creating balanced sample ========================

prop.table(table(train1$is_promoted))
set.seed(100)
# trainB <- ROSE(data = train1,formula = is_promoted~.,N=54808,seed = 1)$data

trainB <- ovun.sample(is_promoted ~ .,
                                  data = train1, method = "under",N = 9000,seed=1)$data
prop.table(table(trainB$is_promoted))

str(trainB)


str(data)




# ========================== modelling alogorithm =============================

names(trainB)

mod1 <- glm(formula = is_promoted~.,data = trainB[,-1], family = "binomial")
summary(mod1)




# ============================ creating dummies in train dataset =================

training <- trainB

names(training)

# ===  dummies for columns 2,3,5,9,10
str(training)


dummies_df <- training[,c(2,3,13)]

for (j in 1:length(names(dummies_df)))
{
  dept_l <-levels(dummies_df[,j])
  length(dept_l)
  for(i in 1:length(dept_l))
  {
    getName <- dept_l[i]   
    newName <- paste(names(dummies_df)[j],getName,"dummy",sep = "_")
    dummies_df$tmp <- ifelse(dummies_df[,j] == getName,1,0)
    getColLast <- ncol(dummies_df)
    names(dummies_df)[getColLast] <- newName
  }
}

names(dummies_df)

training <- cbind(training,dummies_df[,-c(1:3)])

names(training)

training <- training[,-c(2,3,13)]








# ========================== creating dummies in test data

testing <-  test1
testing$is_promoted <- as.factor(testing$is_promoted)

names(testing)
str(testing)

dummies_df <- testing[,c(2,3,13)]

for (j in 1:length(names(dummies_df)))
{
  dept_l <-levels(dummies_df[,j])
  length(dept_l)
  for(i in 1:length(dept_l))
  {
    getName <- dept_l[i]   
    newName <- paste(names(dummies_df)[j],getName,"dummy",sep = "_")
    dummies_df$tmp <- ifelse(dummies_df[,j] == getName,1,0)
    getColLast <- ncol(dummies_df)
    names(dummies_df)[getColLast] <- newName
  }
}

names(dummies_df)

testing <- cbind(testing,dummies_df[,-c(1:3)])

names(testing)

testing <- testing[,-c(2,3,13)]





# ========================== partitioning the training dataset

set.seed(100)
sample_ind <- sample(1:nrow(training),0.8 * nrow(training),replace = FALSE)

trainSample <- training[sample_ind,]
testSample <- training[-sample_ind,]

names(trainSample)

str(trainSample)

summary(mod1)

mod2 <- glm(formula = is_promoted ~ department_Finance_dummy +
              department_HR_dummy +
              department_Legal_dummy + department_Operations_dummy + 
              department_Procurement_dummy + `department_R&D_dummy` + 
              `department_Sales & Marketing_dummy` +
              KPIGReaterThan80perc1 + awards_won1 + no_of_trainings + 
              previous_year_rating + start_age +
              `education1_Master's & above_dummy` +
              scaledScoreDeptReg + 
              region_region_11_dummy +
            region_region_13_dummy +
            region_region_15_dummy +
            region_region_16_dummy+ 
            region_region_19_dummy+
            region_region_2_dummy+
            region_region_20_dummy+
            region_region_21_dummy+
            region_region_24_dummy+
            region_region_26_dummy+
            region_region_27_dummy+
            region_region_29_dummy+
            region_region_30_dummy+
            region_region_31_dummy+
            region_region_32_dummy+
              region_region_33_dummy +
            region_region_34_dummy+
            region_region_5_dummy+
            region_region_6_dummy+
            region_region_8_dummy+
            region_region_9_dummy
            
            ,data = trainSample[,-1], family = "binomial")
summary(mod2)



mod3 <- glm(formula = is_promoted ~ department_Finance_dummy +
              department_HR_dummy +
              department_Legal_dummy + department_Operations_dummy + 
              department_Procurement_dummy + `department_R&D_dummy` + 
              `department_Sales & Marketing_dummy` +
              KPIGReaterThan80perc1 + awards_won1 + no_of_trainings + 
              previous_year_rating + start_age +
              `education1_Master's & above_dummy` +
              scaledScoreDeptReg + 
              region_region_11_dummy +
              region_region_13_dummy +
              region_region_15_dummy +
              region_region_16_dummy+ 
              region_region_19_dummy+
              region_region_2_dummy+
              region_region_20_dummy+
              region_region_21_dummy+
              region_region_26_dummy+
              region_region_27_dummy+
              region_region_29_dummy+
              region_region_30_dummy+
              region_region_31_dummy+
              region_region_32_dummy+
              region_region_5_dummy+
              region_region_6_dummy+
              region_region_8_dummy+
              region_region_9_dummy
            
            ,data = trainSample[,-1], family = "binomial")
summary(mod3)




# validating the results .............................................................................

predict <- predict(mod3, type = 'response')

prop.table(table(train1$is_promoted))
prop.table(table(trainB$is_promoted))
prop.table(table(trainSample$is_promoted))

predict_new <- ifelse(predict >= 0.5186667,1,0)


nrow(trainSample)

kappa2(data.frame(predict_new,trainSample$is_promoted))

confusionMatrix(table(predict_new,trainSample$is_promoted),positive = "1")


#Check model performance - Ideally you need to do it on a cross validation test dataset dereived from train dataset
library(ROCR)
ROCRpred <- prediction(predict, trainSample$is_promoted)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf)
abline(0,1)

ROCRauc <- performance(ROCRpred, 'auc')
ROCRauc@y.values

library(car)
vif(mod3)






predicted <- predict(mod3,type = "response", newdata = testing)
testing$is_promoted <- ifelse(predicted >= 0.5186667,1,0)
solution_glm <- data.frame(employee_id=testing$employee_id,is_promoted=testing$is_promoted)
write.csv(solution_glm,"submission_glm_solFinal_11.csv",row.names = FALSE)

