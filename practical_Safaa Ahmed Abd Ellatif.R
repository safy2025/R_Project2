
# 1-Set your R work directory.
setwd("C:/Users/HP/Desktop/labs")
getwd()

# 2-Read your dataset.
df <- read.csv("G4_howell.csv")

# 3- View dataset on R environment.
View(df)

# 4- Filter the dataset according to specific criteria.
filter1 <- df[df$sex == "M",]
filter1
filter2 <- df[df$age > 40.00,]
filter2
filter3 <- df[df$age > 40.00,c(1,3)]
filter3
filter4 <- df[df$sex == "M" & df$age > 50.00 ,-c(5)]
filter4

# 5- Re-coding means use different values for a variable.

df$newsex[df$sex == "M"] = "1"
df$newsex[df$sex == "F"] = "2"
df
# To Simplify the analysis convert it to factor
df$newsex<-as.factor(df$sex)
str(df)

df$newage[df$age <= 20.00] = "young"
df$newage[df$age >20.00 & df$age < 50.00] = "adult"
df$newage[df$age >= 50.00] = "old"
df

#6- Re-code of code.
df$newage2[df$newage == "young"] = "1"
df$newage2[df$newage == "adult"] = "2"
df$newage2[df$newage == "old"] = "3"
df

# 7- Sorting the dataset according to specific criteria.
sortAge1 <- df[order(df$age),]
sortAge1

sortAge2 <- df[order(-df$age),]
sortAge2

sortAge3 <- df[order(df$age,df$height),]
sortAge3

#8-Deal with NA and empty data.
#what inside c() will be converted to NA

df2 <- read.csv("G4_howell.csv",na.strings = c(""))
df2

#9- To get first 25 rows, we can see NA and <NA>
head(df2, 25)
tail(df2, 10)

#10- To get all locations of NA

complete.cases(df2)

# 11- Get all rows contain missing data

df2[! complete.cases(df2),]

#12- Deal with incorrect data types for variables.

str(df2)


#13- Display some statics about the data

summary(df2)

#14-Change weight variable from char to num because it will not used in analysis/visualization &Remove text in numeric values.

df2$weight<- gsub(" kg"," ",df2$weight)
df2$weight<- as.numeric(df2$weight)
str(df2)
df2

#15- get row with missing data
df2[ ! complete.cases(df2) , ]
is.na(df2)


#16-Get all row with missing data for specific variable



#deleting missing value
df2[is.na(df2$overweight), ]
df2[!is.na(df2$overweight),]
df2<- df2[!is.na(df2$overweight),]
df2
rownames(df2)<-NULL

#______________________________________________ now the data is ready to visualization

library(tidyverse)

#Show the distribution of age using histogram,name the figure and rename the x,y
d<-ggplot(df2,aes(x=age))
d+ geom_histogram(binwidth = 8)
d+geom_histogram(fill = "blue",alpha=0.5)+labs(x="Age",y="Freguancy")+ggtitle("Histogram/Age")

#Show the distribution of Height using histogram ,name the figure
d<-ggplot(df2,aes(x=height))
d+ geom_histogram(binwidth = 8)
d+geom_histogram(fill = "gray",alpha=0.5)+labs(x="Height",y="Freguancy")+ggtitle("Histogram/Height")

#summarize the height to age and sex groups using Bar chart

d<-ggplot(df2 , aes(x=height  ,fill= age))
d +geom_bar()+labs(y=" Height count" ,title="Height category rate")
d +geom_bar() +theme_light()+facet_wrap(~sex)

#display the effect of height on foot_length colored by the groups of age range using scatter plot
d<-ggplot(df2 , aes(weight , height))
d + geom_point(aes(color=age)) +stat_smooth(se=FALSE)  







