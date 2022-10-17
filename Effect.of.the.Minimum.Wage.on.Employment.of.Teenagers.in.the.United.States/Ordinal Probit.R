#### 캡스톤 프로젝트 
## 작성자 : 강진 
## 순서형 프로빗 모형
## 다른 값이 일정할 때, 5를 선택할 확율 구하기 
setwd("/Users/jin/Documents/Rstudy/capstone")

library(data.table); library(ggplot2)
library(dplyr)
library(UsingR)
library(MASS)

# 데이터 로딩 
data <- fread("capstone_data_v3.csv")
# 데이터 확인 
head(data)
#colnames(data) <- c('별점','v_neg','v_pos','가격','개인적만족','신체적','심리적','자기계발','사교적','스포츠','야외활동','예술활동','price','평점_cat_1unit','star_cat')
# 데이터 subset
data <- subset(data, select = -c(V1))
# 데이터 factor화 
data$star_cat <- factor(data$star)
data$star_cat <- factor(data$평점_cat_1unit)
#data$star_cat <- factor(data$star_1unit, levels=c(1,2,3,4,5))
data$star_cat
# ordered probit model
#model.1 <- polr(star_cat ~ v_neg + v_pos + ind_satis + physi + psych + selfdev + social +
#                  sports + outdoor + art + price, data =data, method = 'probit')  # logistic

model.1 <- polr(star_cat ~ v_neg + v_pos + 개인적만족 + 신체적 + 심리적 + 자기계발 + 사교적 +
                  스포츠 + 야외활동 + 예술활동 + 가격_만원, data =data, method = 'probit')  # logistic



summary(model.1)
logLik(model.1)

# 결정계수 확인 mcfadden R^2
library(pscl)
pR2(model.1)

# coefficient confidence interval check
confint(model.1, levle=0.95)

m1.coef <- data.frame(coef(summary(model.1)))
m1.coef$pval = round((pnorm(abs(m1.coef$t.value), lower.tail = FALSE)*2),2)
m1.coef

# coefficient visualize 
library(stargazer)
stargazer(model.1, type='html',out='model1.htm')

# marginal effect
library(erer)
x <- ocME(model.1)
x$out

# predicted
model1.pred <-predict(model.1, type='probs')
summary(model1.pred)

setup1 <- data.frame(v_neg = c(mean(data$v_neg),mean(data$v_neg)),
                     v_pos = c(mean(data$v_pos),mean(data$v_pos)),
                     개인적만족 = c(0,0),
                     신체적 = c(0,0),
                     심리적 = c(0,0),
                     자기계발 = c(0,1),
                     사교적 = c(0,0),
                     스포츠 = c(0,0),
                     야외활동 = c(0,1),
                     예술활동 = c(0,0),
                     가격_만원 = c(4.5,4.5))

predict(model.1, newdata = setup1, type='probs')
mean(data$v_neg)
mean(data$v_pos)
setup2 <- data.frame(v_neg = rep(mean(data$v_neg),5),
                     v_pos = rep(mean(data$v_pos),5),
                     개인적만족 = c(0,1,0,0,0),
                     신체적 = c(0,0,1,0,0),
                     심리적 = c(0,0,0,1,0),
                     자기계발 = c(0,0,0,0,1),
                     사교적 = c(0,0,0,0,0),
                     스포츠 = c(0,0,0,0,0),
                     야외활동 = c(0,0,0,0,0),
                     예술활동 = c(0,0,0,0,0),
                     가격_만원 = rep(mean(data$가격_만원),5))

# 다른 값이 일정할때, 5를 선택할 확율 
predict(model.1, newdata = setup2, type='probs')
