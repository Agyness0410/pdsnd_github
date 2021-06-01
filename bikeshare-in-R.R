#amend date:1st Jun


library(ggplot2)
library(dplyr)

setwd("/Users/chiarachen/DocfromMac/OneDrive/DocfromMac/R_in_Udacity/bikeshare")

# Load datasets and redefine
chicago_df <- read.csv("chicago.csv",check.names = F,row.names = 1)
chicago_df <- na.omit(chicago_df)
newyork_df <- read.csv("new-york-city.csv",check.names = F,row.names = 1)
newyork_df <- na.omit(newyork_df)
washington_df <- read.csv("washington.csv",check.names = F,row.names = 1)
washington_df <- na.omit(washington_df)




#1---- Total travel time for different cities
total_time <- data.frame(chicago = sum(chicago_df$`Trip Duration`),newyork=sum(newyork_df$`Trip Duration`),washington=sum(washington_df$`Trip Duration`))
total_time <- as.data.frame(t(total_time))
colnames(total_time) <- c("total_duration")
total_time$city <- rownames(total_time)
ggplot(total_time,aes(x=city,y=total_duration),fill=city) +
  geom_bar(stat="identity") +
  geom_text(label=total_time$total_duration,vjust=00)
#----------summary 1：the total travel time of chicago,newyork,washington are 167128457s,217901836s,371183985s
#----------and washington has the longest travel time。

#2---- Average travel time for different cities
mean_time <- data.frame(chicago = mean(chicago_df$`Trip Duration`),newyork = mean(newyork_df$`Trip Duration`),washington=mean(washington_df$`Trip Duration`))
mean_time <- as.data.frame(t(mean_time))
colnames(mean_time) <- c("mean_duration")
mean_time$city <- rownames(mean_time)
ggplot(mean_time,aes(x=city,y=mean_duration),fill=city) +
  geom_bar(stat="identity") +
  geom_text(label=mean_time$mean_duration,vjust=00)
#----------summary 2： the average travel time of chicago,newyork,washington are 699,34s,801.76s,1237.28s
#----------and washington has the longest travel time。

#3---- The counts of each user type
user_type1 <- chicago_df %>%
  group_by(`User Type`)  %>%
  summarise(counts = n())
user_type2 <- newyork_df %>%
  group_by(`User Type`)  %>%
  summarise(counts = n())
user_type3 <- washington_df %>%
  group_by(`User Type`)  %>%
  summarise(counts = n())
user_type2 <- user_type2[-1,]
user_type <- data.frame(user_type=c("Customer","Subscriber","Dependent"),
                        counts=c((user_type1$counts[1] + user_type2$counts[1] + user_type3$counts[1]),(user_type1$counts[2] + user_type2$counts[2] + user_type3$counts[2]),1))

ggplot(user_type, aes(x="", y=counts, fill=user_type))+
  geom_bar(width = 1, stat = "identity")+
  geom_text(label=user_type$counts,vjust=00)
#----------summary 3：the counts of chicago,newyork,washington in the type of Dependent,Subscriber and Customer are 1,83721,487459 and 1
