## read raw datas into R
X_test <- read.table("X_test.txt")
Y_test <- read.table("Y_test.txt")
subject_test <- read.table("subject_test.txt")

##set variable names
names(Y_test)<- "activity"
names(subject_test)<- "id"

##built the test data
##add a variable "subject" to indicate these datas come from test
test <- cbind(subject_test, Y_test,X_test,"subject"="test")

##built train data
X_train <- read.table("X_train.txt")
Y_train <- read.table("Y_train.txt")
names(Y_train)<- "activity"
subject_train <- read.table("subject_train.txt")
names(subject_train)<- "id"

train <- cbind(subject_train, Y_train,X_train,"subject"="train")

##combine test and train data
data <- rbind(test,train)

##read "features" file (in order to extract variable names)
features <- read.table("features.txt")
variablenames <- features[,2]

##extract variables with names including "mean" and "std"
##grep will return the numbers where these variables are located
mean_variable <- grep("mean",variablenames)
std_variable <- grep("std",variablenames)

##use the numbers obtained in previous step to generate a list of variable names
index <- paste0("V",c(mean_variable,std_variable))

library(dplyr)

##select all the columns involved "mean" and "std"
data1 <- select(data,"id", "activity", index)

##built a dataframe of activity_label
##(in order to substitute numers in "activity" by descriptive activity names)
activity_label <- data.frame(1:6,c("WALKING", "WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
names(activity_label) <- c("activity", "description")

##merge activity_label and data1 by="activity"
##(returns a dataframe with first two colume are activity_label in numbers and descriptive names respectively)
data2 <- merge(activity_label,data1,by="activity")

data3 <- select(data2, id, activity=description, starts_with("V"))

##extract variablenames that containing "mean" and "std"
##(returning the variable name)
mean_variable1 <- grep("mean",variablenames,value=TRUE)
std_variable1 <- grep("std",variablenames,value=TRUE)

##remove "()" and "-"
variablenames <- c(mean_variable1,std_variable1)
variablenames1 <- sub("[(]","",variablenames)
variablenames2 <- sub("[)]","",variablenames1)
variablenames3 <- gsub("-","",variablenames2)

##set variable names
variablenames4 <- c("id","activity",variablenames3)
colnames(data3) <- variablenames4

##group up data3 by "id" and "activity" (into 180 subgroups)
##calculate the average of each variable for each subgroup
data4 <- aggregate(data3[, 3:81], list(data3$id,data3$activity), mean)
