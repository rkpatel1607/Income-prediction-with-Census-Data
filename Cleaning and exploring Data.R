library("ggplot2")
library("corrplot")
library("sqldf")
library("reshape")
# Read the Data
train <- read.csv("./census_income_learn.csv")
test <- read.csv("./census_income_test.csv")
# Cleaning the data
# Add attribute names for comprehension 
colnames(train) <-c("age", "classOfWorker", "industryCode", "occupationCode", "education", "wagePerHour",
                    "enrollEduInstLastWk", "maritalStatus", "majorIndustryCode", "majorOccupationCode",
                    "race", "hispanicOrigin", "sex",
                    "laborUnion", "reasonUnemployment", "FTPTEmployement", "capitalGain", "capitalLoss", "divdendsFromStocks",
                    "taxFiler", "regionPreviousResidence",
                    "statePreviousResidence", "detailedHousehold", "householder", "instanceWeight",
                    "MSAMigrationCodeChange", "REGMigrationCodeChange",
                    "REGMigrationCodeChangeWithin", "House1YearAgo", "sunbelt", "numPersonsWorkedForEmployer",
                    "parentsIfUnder18", "fatherBirthCountry",
                    "motherBirthCountry", "selfBirthCountry", "citizenship", "ownBusisnessSE", "questionnaireVet",
                    "benefVet", "weeksWorkedInYear", "year", "income")

to_remove <- c("industryCode", "occupationCode",
               "enrollEduInstLastWk", "majorIndustryCode", "majorOccupationCode",
               "hispanicOrigin",
               "laborUnion", "reasonUnemployment", "FTPTEmployement", "regionPreviousResidence",
               "statePreviousResidence", "detailedHousehold", "householder", "instanceWeight",
               "MSAMigrationCodeChange", "REGMigrationCodeChange",
               "REGMigrationCodeChangeWithin", "fatherBirthCountry",
               "motherBirthCountry")
for (column in to_remove){
  train[[column]] <- NULL
}

# Change every value that is a " ?" to NA, and remove elements with missing values
train[train == " ?"] <- NA
train <- train[complete.cases(train),]
#Changing income to 0, 1
train$income <- as.numeric(train$income)-1
#Correlation plot
corrplot(cor(train[,c(1,4,8,9,21,23)]))
#Re-factoring income
train$income <- factor(train$income, labels=c("<=50k", ">50k"))
#We do the same steps to the data test
# Add attribute names for comprehension 
colnames(test) <-c("age", "classOfWorker", "industryCode", "occupationCode", "education", "wagePerHour",
                   "enrollEduInstLastWk", "maritalStatus", "majorIndustryCode", "majorOccupationCode",
                   "race", "hispanicOrigin", "sex",
                   "laborUnion", "reasonUnemployment", "FTPTEmployement", "capitalGain", "capitalLoss", "divdendsFromStocks",
                   "taxFiler", "regionPreviousResidence",
                   "statePreviousResidence", "detailedHousehold", "householder", "instanceWeight",
                   "MSAMigrationCodeChange", "REGMigrationCodeChange",
                   "REGMigrationCodeChangeWithin", "House1YearAgo", "sunbelt", "numPersonsWorkedForEmployer",
                   "parentsIfUnder18", "fatherBirthCountry",
                   "motherBirthCountry", "selfBirthCountry", "citizenship", "ownBusisnessSE", "questionnaireVet",
                   "benefVet", "weeksWorkedInYear", "year", "income")

for (column in to_remove){
  test[[column]] <- NULL
}
# Change every value that is a " ?" to NA, and remove elements with missing values
test[test == " ?"] <- NA
test <- test[complete.cases(test),]
test$income <- factor(test$income, labels=c("<=50k", ">50k"))

# Explore Categorical Data
#Work Class
ggplot(train, aes(x = classOfWorker, fill = income)) + geom_bar(position="fill") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Proportions of incomes within different Workclass")
table(train$classOfWorker, train$income)
#Age
summary(train$age)
str(train$age)
sum(is.na(train$age))
# Split data by class into two subsets
more_50k <- train[train$income == "<=50k",]
less_50k <- train[train$income == ">50k",]
# Class repartition depending on age category
more_50k_young <- 0
more_50k_adult <- 0
for (i in 1:nrow(more_50k)){
  age <- more_50k[i,"age"]
  if (age <= 20){
    more_50k_young <- more_50k_young + 1
  } else {
    more_50k_adult <- more_50k_adult + 1
  }
}
less_50k_young <- 0
less_50k_adult <- 0
for (i in 1:nrow(less_50k)){
  age <- less_50k[i,"age"]
  if (age <= 20){
    less_50k_young <- less_50k_young + 1
  } else {
    less_50k_adult <- less_50k_adult + 1
  }
}
age_group <- data.frame(Young = numeric(0), Adult = numeric(0))
age_less_50k <- data.frame(Young = less_50k_young, Adult = less_50k_adult)
age_group <- rbind(age_group, age_less_50k)
age_more_50k <- data.frame(Young = more_50k_young, Adult = more_50k_adult)
age_group <- rbind(age_group, age_more_50k)
age_group <- as.matrix(age_group)
barplot(age_group, main = "Proportions of incomes with different Ages", legend = c(">50k", "<=50k"), col = c("green", "blue"))
# Class repartition depending on sex
temp <- table(train$income, train$sex)
barplot(temp, beside=TRUE, main = "Proportion of incomes with different sex", legend.text=rownames(temp), ylab="frequency", 
        col = c("blue", "green"))

#Education 
ggplot(train, aes(x = education, fill = income)) + geom_bar(position="fill") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Proportion of incomes with different Education")
table(train$education, train$income)

#Race
#Changing income to 0, 1
train$income <- as.numeric(train$income)-1
race<-0
race<-sqldf('SELECT race, count(race) as Count 
                  ,sum(income) as Above_50k from train group by race')
race$Below_50k<-race$Count-race$Above_50k
table<-data.frame(race=race$race, Proportion=race$Above_50k/race$Count)
race<-race[,c(1,3,4)]
rac<-melt(race,id.vars = 'race')
ggplot(rac,aes(x=race,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('red','green'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of incomes within different races')

#WeekWorked
hist(train$weeksWorkedInYear, col="blue", main="Week worked in year")
train$WeekJ<-ifelse(train$weeksWorkedInYear<=45,'NormalWork','HugeWork')
wl<-sqldf('SELECT WeekJ as WorkLoad, count(WeekJ) as Count, sum(income) as Above_50k from train group by WeekJ')
wl$Below_50k<-wl$Count-wl$Above_50k
Percentage<-wl$Above/wl$Count
wl<-wl[,c(1,3,4)]
wlt<-melt(wl,id.vars = 'WorkLoad')
wl<-cbind(wl,Percentage)
gg<-ggplot(wlt,aes(x=WorkLoad,y=value,fill=variable))+geom_bar(stat = 'identity',position = 'stack')+theme_bw()+scale_fill_manual(values = c('blue','green'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of incomes with different Weeks worked')
#Re-factoring income
train$income <- factor(train$income, labels=c("<=50k", ">50k"))
