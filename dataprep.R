library(magrittr)
library(dplyr)
library(chron)
library(anytime)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)
library(stringr)
library(data.table)
library(memoise)

table <- read.csv("D:/table.csv", na = "empty", header = "true")
names(table) <- sub("[.]","_",(sub("[.]","_",names(table))))

#url split
urlsplit <- strsplit(table$request_url,'/')
urlsplit <- sapply(urlsplit, "[[", 3)
table$request_url <- urlsplit
rm(urlsplit)

#date split
table$date <- anydate(as.POSIXct(as.numeric(as.character(table$server_request_request_unixtime)),origin="1970-01-01",tz="GMT"))
table$day <- weekdays(as.Date(table$date))
table$time <- strftime(as.POSIXct(as.numeric(as.character(table$server_request_request_unixtime)),origin="1970-01-01",tz="GMT"),format="%H:%M:%S")
table$hour <- format(as.POSIXct(table$time,format="%H:%M:%S"),"%H")


#group by ip and url with min and max time to calculate time spent
a_col <- list('request_ip','request_url','date')
con_table <- table %>% group_by_(.dots = a_col) %>% summarise(max_time = max(time),min_time = min(time))
con_table$time_spent <- difftime(strptime(con_table$max_time,"%H:%M:%S"),strptime(con_table$min_time,"%H:%M:%S"))
aggregate_time_spent <- aggregate(con_table$time_spent, by=list(Category=con_table$request_ip), FUN=sum)

#get unique domain counts
url_counts <- table %>% group_by(request_url) %>% dplyr::summarise(n= n()) %>% ungroup %>% arrange(request_url, desc(n))
url_counts <- url_counts[order(url_counts$n,decreasing = T),]
top_10 <- head(url_counts,n =20)
top_10 <- top_10[order(top_10$n,decreasing = T),]
write.csv(top_10,'D:/shinyapp/shiny7park/top_10.csv')

#get unique ip counts
unique_ip_date <- setDT(table)[, .(count = uniqueN(request_ip)), by = date]
unique_ip_day <- setDT(table)[, .(count = uniqueN(request_ip)), by = day]
unique_ip_hour <- setDT(table)[, .(count = uniqueN(request_ip)), by = hour] 
ip_counts <- Table %>% group_by(request_ip) %>% dplyr::summarise(n= n()) %>% ungroup %>% arrange(request_ip, desc(n))

#get counts by day
day_counts <- table %>% group_by(day) %>% dplyr::summarise(n= n()) %>% ungroup %>% arrange(day, desc(n))
day_counts$n[5] <- day_counts$n[5]/5
day_counts$n[1] <- day_counts$n[1]/5
day_counts$n[2] <- day_counts$n[2]/4
day_counts$n[3] <- day_counts$n[3]/4
day_counts$n[4] <- day_counts$n[4]/4
day_counts$n[6] <- day_counts$n[6]/4
day_counts$n[7] <- day_counts$n[7]/4
day_counts <- day_counts[c(2,6,7,5,1,3,4),]

write.csv(day_counts, 'D:/shinyapp/shiny7park/day_wise.csv')

#get counts by hour
hour_counts <- table %>% group_by(hour) %>% dplyr::summarise(n= n()) %>% ungroup %>% arrange(hour, desc(n))
hour_counts$n <- hour_counts$n/29
write.csv(hour_counts, 'D:/shinyapp/shiny7park/hour_wise.csv')

p<-ggplot(hour_counts, aes(x=hour, y=n, group=1)) +
  geom_line(stat="identity")+geom_point()+theme_minimal()
p



#domain Comparison

domain_facebook <- table %>% filter(str_detect(request_url,pattern = "facebook"))
facebook_ip_date <- setDT(domain_facebook)[, .(count = uniqueN(request_ip)), by = date]

domain_google <- table %>% filter(str_detect(request_url,pattern = "www.google.com"))
google_ip_date <- setDT(domain_google)[, .(count = uniqueN(request_ip)), by = date]

domain_amazon <- table %>% filter(str_detect(request_url,pattern = "amazon"))
amazon_ip_date <- setDT(domain_amazon)[, .(count = uniqueN(request_ip)), by = date]

domain_netflix <- table %>% filter(str_detect(request_url,pattern = "netflix"))
netflix_ip_date <- setDT(domain_netflix)[, .(count = uniqueN(request_ip)), by = date]

domain_priceline <- table %>% filter(str_detect(request_url,pattern = "priceline"))
priceline_ip_date <- setDT(domain_priceline)[, .(count = uniqueN(request_ip)), by = date]

domain_yahoo <- table %>% filter(str_detect(request_url,pattern = "yimg"))
yahoo_ip_date <- setDT(domain_yahoo)[, .(count = uniqueN(request_ip)), by = date]

domain_twitter <- table %>% filter(str_detect(request_url,pattern = "twitter"))
twitter_ip_date <- setDT(domain_twitter)[, .(count = uniqueN(request_ip)), by = date]

domain_day_count <- merge.data.frame(google_ip_date,facebook_ip_date,by.x = "date",by.y = "date",suffixes = c("1","2"))

domain_day_count <- merge.data.frame(domain_day_count,amazon_ip_date,by.x = "date",by.y = "date",suffixes = c("3","4"))

domain_day_count <- merge.data.frame(domain_day_count,netflix_ip_date,by.x = "date",by.y = "date",suffixes = c("5","6"))

domain_day_count <- merge.data.frame(domain_day_count,priceline_ip_date,by.x = "date",by.y = "date",suffixes = c("7","8"))

domain_day_count <- merge.data.frame(domain_day_count,yahoo_ip_date,by.x = "date",by.y = "date",suffixes = c("9","10"))

domain_day_count <- merge.data.frame(domain_day_count,twitter_ip_date,by.x = "date",by.y = "date",suffixes = c("11","12"))

names(domain_day_count)<-c("date","google","facebook","amazon","netflix","priceline","yahoo","twitter")


#dayof the week wise
facebook_ip_day <- setDT(domain_facebook)[, .(count = uniqueN(request_ip)), by = day]
google_ip_day <- setDT(domain_google)[, .(count = uniqueN(request_ip)), by = day]
amazon_ip_day <- setDT(domain_amazon)[, .(count = uniqueN(request_ip)), by = day]
netflix_ip_day <- setDT(domain_netflix)[, .(count = uniqueN(request_ip)), by = day]
priceline_ip_day <- setDT(domain_priceline)[, .(count = uniqueN(request_ip)), by = day]
yahoo_ip_day <- setDT(domain_yahoo)[, .(count = uniqueN(request_ip)), by = day]
twitter_ip_day <- setDT(domain_twitter)[, .(count = uniqueN(request_ip)), by = day]




#hourly average
facebook_ip_hour <- setDT(domain_facebook)[, .(count = uniqueN(request_ip)), by = hour]
google_ip_hour <- setDT(domain_google)[, .(count = uniqueN(request_ip)), by = hour]
amazon_ip_hour <- setDT(domain_amazon)[, .(count = uniqueN(request_ip)), by = hour]
netflix_ip_hour <- setDT(domain_netflix)[, .(count = uniqueN(request_ip)), by = hour]
priceline_ip_hour <- setDT(domain_priceline)[, .(count = uniqueN(request_ip)), by = hour]
yahoo_ip_hour <- setDT(domain_yahoo)[, .(count = uniqueN(request_ip)), by = hour]
twitter_ip_hour <- setDT(domain_twitter)[, .(count = uniqueN(request_ip)), by = hour]

facebook_ip_hour$count<-facebook_ip_hour$count/30
google_ip_hour$count<-google_ip_hour$count/30
amazon_ip_hour$count<-amazon_ip_hour$count/30
netflix_ip_hour$count<-netflix_ip_hour$count/30
priceline_ip_hour$count<-priceline_ip_hour$count/30
yahoo_ip_hour$count<-yahoo_ip_hour$count/30
twitter_ip_hour$count<-twitter_ip_hour$count/30



clicks_user <- setDT(con_table)[, .(count = uniqueN(date)), by = request_ip]

write.csv(top_10,'D:/shinyapp/deploy/top_10.csv')
write.csv(day_counts,'D:/shinyapp/deploy/day_counts.csv')
write.csv(hour_counts,'D:/shinyapp/deploy/hour_counts.csv')

write.csv(domain_day_count,'D:/shinyapp/deploy/domain_day_count.csv')
write.csv(domain_daywise_count,'D:/shinyapp/deploy/domain_daywise_count.csv')
write.csv(domain_hour_count,'D:/shinyapp/deploy/domain_hour_count.csv')


#wrodcloud functions
wordcloud_amazon <- wordcloud[(str_detect(wordcloud$url,"amazon")),]
wordcloud_amazon <-na.omit(wordcloud_amazon)


jeopCorpus <- Corpus(VectorSource(wordcloud_amazon$`table$request_windowTitle`))
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))

jeopCorpus <- tm_map(jeopCorpus, stemDocument)
wordcloud(jeopCorpus, max.words = 100, random.order = FALSE,colors=brewer.pal(8, "Dark2"))

wordcloud_netflix <- wordcloud[(str_detect(wordcloud$url,"netflix")),]
wordcloud_netflix <-na.omit(wordcloud_netflix)

jeopCorpus <- Corpus(VectorSource(wordcloud_netflix$`table$request_windowTitle`))
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))

jeopCorpus <- tm_map(jeopCorpus, stemDocument)
wordcloud(jeopCorpus, max.words = 100, random.order = FALSE,colors=brewer.pal(8, "Dark2"))

wordcloud_facebook <- wordcloud[(str_detect(wordcloud$url,"facebook")),]
wordcloud_facebook <-na.omit(wordcloud_facebook)


jeopCorpus <- Corpus(VectorSource(wordcloud_facebook$`table$request_windowTitle`))
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))

jeopCorpus <- tm_map(jeopCorpus, stemDocument)
wordcloud(jeopCorpus, max.words = 100, random.order = FALSE,colors=brewer.pal(8, "Dark2"))

