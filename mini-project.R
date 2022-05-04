# https://www.kaggle.com/datasets/jordizar/climb-dataset

library("corrplot")
library(MASS) 
library(ISLR)
library(glmnet) 


data <- read.csv("climber_df.csv")
summary(data)


library(sampling)
set.seed('555')

freq <- table(data$sex)
freq


st.sizes <- 501 * freq / sum(freq)
st.sizes

st.2 <- sampling::strata(data, stratanames = c("sex"),
                         size = st.sizes, method = "srswor",
                         description = TRUE)

st.2

st.sample2 <- getdata(data, st.2)

st.sample2

summary(st.sample2)

drop <- c("country", "year_first", "year_last", "date_first", "date_last","ID_unit","Prob", "Stratum",
          "grades_max", "grades_last", "grades_count")

data1 = st.sample2[,!(names(st.sample2) %in% drop)]

tail(data1)

if(any(is.na(data1))) { print("TRUE") } else{print("FALSE") }

my_data <- data1[, c(2,3,4,5,6,7,8)]
my_dataCorr <- cor(my_data)
my_dataCorr
corrplot(my_dataCorr, method="number")

#Simple Linear Regression
plot(data$grades_first, data$grades_mean,
     main="",
     xlab="grades_first",
     ylab="grades_mean", col="black")

model1<-lm(grades_mean~grades_first, data=data1)
anova(model1)
summary(model1)
abline(model1,col='red')

plot(model1, which = 1, pch = 16)
qf(0.05, df1 = 1, df2 = 498, lower.tail = FALSE)
qt(0.975, 498)


#Multiple Linear Regression
model2<-lm(grades_mean~grades_first+years_cl+sex+age+height+weight, data=data1)
anova(model2)
summary(model2)
plot(model2, which = 1, pch = 16)
qf(0.05, df1 = 6, df2 = 493, lower.tail = FALSE)
qt(0.975, df = 493)


model3<-lm(grades_mean~grades_first+years_cl+height, data=data1)
anova(model3)
summary(model3)
plot(model3, which = 1, pch = 16)
qf(0.05, df1 = 3, df2 = 496, lower.tail = FALSE)
qt(0.975, df = 496)

model4<-lm(grades_mean~grades_first+years_cl, data=data1)
anova(model4)
summary(model4)
plot(model4, which = 1, pch = 16)
qf(0.05, df1 = 2, df2 = 497, lower.tail = FALSE)
qt(0.975, df = 497) 

model5<-lm(grades_mean~grades_first+years_cl+age+height+weight, data=data1)
anova(model5)
summary(model5)
plot(model4, which = 1, pch = 16)
qf(0.05, df1 = 5, df2 = 494, lower.tail = FALSE)
qt(0.975, df = 494)

model6<-lm(grades_mean~grades_first+years_cl+sex+age+weight, data=data1)
anova(model6)
summary(model6)
plot(model6, which = 1, pch = 16)
qf(0.05, df1 = 5, df2 = 494, lower.tail = FALSE)
qt(0.975, df = 494)


newModel = step(lm(grades_mean ~. -(user_id), data = data1),direction = 'forward')
summary(newModel)


newModel = step(lm(grades_mean ~. -(user_id), data = data1),direction = 'backward')
summary(newModel)
