install.packages('plotly')
library(plotly)
install.packages("sqldf")
install.packages("tidyr")
library(sqldf)
library(ggplot2)
library(tidyr)


#Importing entire data

Jan_2018 <- read.csv('page_level_data-1-2018.csv')
Feb_2018 <- read.csv('page_level_data-2-2018.csv')
March_2018 <- read.csv('page_level_data-3-2018.csv')
April_2018 <- read.csv('page_level_data-4-2018.csv')
May_2018 <- read.csv('page_level_data-5-2018.csv')
June_2018 <- read.csv('page_level_data-6-2018.csv')
July_2018 <- read.csv('page_level_data-7-2018.csv')
Aug_2018 <- read.csv('page_level_data-8-2018.csv')
Sept_2018 <- read.csv('page_level_data-9-2018.csv')
Oct_2018 <- read.csv('page_level_data-10-2018.csv')
Nov_2018 <- read.csv('page_level_data-11-2018.csv')
Dec_2018 <- read.csv('page_level_data-12-2018.csv')
Jan_2019 <- read.csv('page_level_data-1-2019.csv')
cpm <- read.csv('cpm_estimates-25Jan19.csv')

#union all months data
Data<- rbind(Jan_2018,Feb_2018, March_2018, April_2018, May_2018, June_2018, July_2018, Aug_2018, Sept_2018, Oct_2018, Nov_2018, Dec_2018, Jan_2019)

attach(Data)
nrow(Data)



##############################################################################

data_1 <- sqldf('SELECT 
                     name,case when name="page_impressions_by_age_gender_unique" then metric else ".-" end as metric1, value, hID, date,case when name="page_impressions_by_age_gender_unique" then value else 0 end as metricx, 
case when name="page_impressions" then value else 0 end as Page_impression,
case when name="page_impressions_organic" then value else 0 end as P_im_organic,
case when name="page_impressions_paid" then value else 0 end as P_im_paid,
case when name="page_cta_clicks_logged_in_total" then value else 0 end as P_clicks_total,
case when name="page_post_engagements" then value else 0 end as P_post_eng
FROM Data ')


age_data = sqldf('select a.hID,a.metric1,sum(a.metricx) as mp,a.date,sum(a.Page_impression),sum(a.P_im_organic),sum(a.P_im_paid),sum(a.P_clicks_total),sum(a.P_post_eng)
from data_1 a 
                 group by a.date,a.hID,a.metric1 having (a.metric1 not null) and sum(a.Page_impression)>0 or sum(a.P_im_organic)>0 or sum(a.P_im_paid)>0 or sum(a.P_clicks_total)>0 or sum(a.P_post_eng)>0 or sum(a.metricx)>0   order by a.date' )


metric <- data.frame(do.call('rbind', strsplit(as.character(age_data$metric1), '.', fixed = T)))
strsplit(as.character(age_data$metric1), '.', fixed = T)
metric
names(metric) <- c("Gender", "X")
metric2 <- data.frame(do.call('rbind', strsplit(as.character(metric$X), '-', fixed = T)))
names(metric2) <- c("age_min", "age_max")

age_data1 <- age_data[,-2]

Final_data <- cbind(age_data1, metric$Gender, metric2 )
head(Final_data)
nrow(Final_data)

#############################
cpm_data = sqldf('SELECT *,cASE 
WHEN male=1 and female=1 then "U"
when male=1 and female=0 then "M"
when male=0 and female=1 then "F" end  as Gender  
from cpm ' )
head(cpm_data)
nrow(cpm_data)
                  
Fin_data = sqldf('select Case when F.hID IS NOT NULL then F.hID else c.hID end as hID,
Case when F.date IS NOT NULL then F.date else c.date end as date,
Case when c.age_min IS NOT NULL then c.age_min 
when F.age_min=F.age_max and c.age_min IS NULL then "18" else F.age_min end age_min ,
Case when c.age_max IS NOT NULL then c.age_max else F.age_max end as age_max,
Case when c.Gender IS NOT NULL then c.Gender else F.metric$Gender end as Gender,
F.mp as value,c.cpm
                
from cpm_data c left join Final_data F on F.date=c.date where F.hID=c.hID and F.age_max=c.age_max  ')
head(Fin_data)

nrow(Fin_data)
head(Fin_data)
attach(Fin_data)


######################### Modeling ################################

model = lm(cpm ~ age_min +age_max + Gender + value  ,data=Fin_data)
summary(model)


#RSE
sigma(model)/mean(Fin_data$cpm)

# 1. Add predictions 
pred.int <- predict(model, interval = "prediction")
mydata <- cbind(Fin_data, pred.int)
mydata

# 2. Regression line + confidence intervals

p <- ggplot(mydata, aes(Fin_data$value, Fin_data$cpm)) +
  geom_point() +
  stat_smooth(method = lm)

# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

# value predictions
predicted_set = sqldf('select hID,CAST(age_min AS int) as age_min,date,age_max, F.metric$Gender as Gender, mp as value from Final_data F where mp is
not null and age_max in ("65+","18","24","34","44","49","54") and F.metric$Gender in("M","F","U") and mp>0')
nrow(predicted_set)
Prediction_set = cbind(predicted_set,predict(model, newdata = predicted_set, interval = "confidence"))
head(Prediction_set)
max(Prediction_set$fit)

clusters = kmeans(Prediction_set[ ,1:6],5)
Prediction = as.factor(clusters$cluster)

str(clusters)

