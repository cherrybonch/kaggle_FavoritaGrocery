library(dplyr)
library(zoo)
library(dummies)

#######
# Enrich oil file: add missing dates, add moving averages
oil_full <- right_join(oil,
                       data.frame(date=as.character(seq.Date(as.Date("2013-01-01"),
                                      as.Date("2017-08-31"),by="day"))),by="date")
price <- NA
for (i in seq(nrow(oil_full))){
    if (is.na(oil_full$dcoilwtico[i])){
        oil_full$dcoilwtico[i] <- price
    }
    else{
        price <- oil_full$dcoilwtico[i]
    }
}
oil_full$avg3 <- rollapply(oil_full$dcoilwtico, 3, mean, 
                           na.rm=TRUE, fill=NA, align='right')
oil_full$avg7 <- rollapply(oil_full$dcoilwtico, 7, mean, 
                           na.rm=TRUE, fill=NA, align='right')
oil_full$avg15 <- rollapply(oil_full$dcoilwtico, 15, mean, 
                           na.rm=TRUE, fill=NA, align='right')
oil_full$avg30 <- rollapply(oil_full$dcoilwtico, 30, mean, 
                            na.rm=TRUE, fill=NA, align='right')

#####
# Find days affected by holidays
holidays_events_filt <- holidays_events %>%
    filter(!transferred & type!="Work Day" & !grepl("Terremoto Manabi",description))
special_dates_around <- apply(holidays_events_filt,1,function(x){
    return(data.frame(date=as.character(c(as.Date(x[1])-2,as.Date(x[1])-1,as.Date(x[1])+1)),
                      type=rep(x[2],3),
                      locale=rep(x[3],3),
                      locale_name=rep(x[4],3),
                      description=rep(x[5],3)))
})
special_dates <- rbind(holidays_events[!holidays_events$transferred & 
                                           holidays_events$type!="Work Day",-6],
                       do.call("rbind",special_dates_around))
special_dates$description <- sapply(special_dates$description,function(x){
    res <- gsub("\\+([[:digit:]]+)","",x)
    res <- gsub("-([[:digit:]]+)","",res)
    res <- gsub("Traslado ","",res)
    res <- gsub("Puente ","",res)
    return(res)
})
special_dates <- special_dates[,-c(2,3)]
special_dates <- special_dates[!duplicated(special_dates),]
special_dates <- cbind(special_dates[,-3],
                       dummy(special_dates$description,sep = "_"))
colnames(special_dates) <- sapply(colnames(special_dates),gsub,pattern="special_dates",replacement="holiday")


####    Modifying train 

# Add info about date, stores and items
train_big <- train %>%
    mutate(day_of_week = weekdays(as.Date(date))) %>%
    mutate(month = months(as.Date(date))) %>%
    mutate(year = substr(date,1,4)) %>%
    inner_join(stores,by="store_nbr") %>%
    inner_join(items,by="item_nbr")

# Add oil data to train
train_oil <- train_big %>%
    left_join(oil_full,by="date") %>%
    mutate(datelag1 = as.character(as.Date(date)+1)) %>%
    left_join(oil_full,by=c("datelag1"="date")) %>%
    mutate(datelag2 = as.character(as.Date(date)+2)) %>%
    left_join(oil_full,by=c("datelag2"="date")) %>%
    mutate(datelag3 = as.character(as.Date(date)+3)) %>%
    left_join(oil_full,by=c("datelag3"="date")) %>%
    mutate(datelag7 = as.character(as.Date(date)+7)) %>%
    left_join(oil_full,by=c("datelag7"="date")) %>%
    rename(oil = dcoilwtico.x, avg3=avg3.x,avg7=avg7.x,avg15=avg15.x,avg30=avg30.x,
           oil_lag1 = dcoilwtico.y,avg3_lag1=avg3.y,avg7_lag1=avg7.y,avg15_lag1=avg15.y,avg30_lag1=avg30.y,
           oil_lag2 = dcoilwtico.x.x, avg3_lag2=avg3.x.x,avg7_lag2=avg7.x.x,avg15_lag2=avg15.x.x,avg30_lag2=avg30.x.x,
           oil_lag3 = dcoilwtico.y.y,avg3_lag3=avg3.y.y,avg7_lag3=avg7.y.y,avg15_lag3=avg15.y.y,avg30_lag3=avg30.y.y,
           oil_lag7 = dcoilwtico,avg3_lag7=avg3,avg7_lag7=avg7,avg15_lag7=avg15,avg30_lag7=avg30)

# Change day of week to holiday or workday
holidays <- holidays_events$date[!holidays_events$transferred]
CorrectDate <- function(x){
    # date 2, day_of_week 7, city 10, state 11
    ret <- x[7]
    if(is.element(x[2],holidays)){
        day_type <- filter(holidays_events,(locale_name==x[10] | locale_name==x[11] |
                                                locale_name=="Ecuador") & date==x[2])$type
        if(length(day_type)>0){
            if(day_type!="Work Day" & day_type!="Event" & 
               x[7]!="Saturday" & x[7]!="Sunday"){
                ret <- "Holiday"
            }
            if((x[7]=="Saturday"|x[7]=="Sunday")&day_type=="Work Day"){
                ret <- "Work day"
            }
        }
    }
    return(ret)
}
train_oil$day_of_week <- apply(train_oil,1,CorrectDate)

# Add holiday data to train
train_holidays <- train_oil %>%
    left_join(special_dates,by="date") %>%
    filter(city==locale_name | state==locale_name |
               locale_name == "Ecuador" | is.na(locale_name))






# LATER!!!
# Do same with test
test_big <- test %>%
    mutate(day_of_week = weekdays(as.Date(date))) %>%
    mutate(month = months(as.Date(date))) %>%
    mutate(year = substr(date,1,4)) %>%
    inner_join(stores,by="store_nbr") %>%
    inner_join(items,by="item_nbr") %>%
    left_join(oil_full,by="date") %>%
    mutate(datelag1 = as.character(as.Date(date)+1)) %>%
    left_join(oil_full,by=c("datelag1"="date")) %>%
    mutate(datelag2 = as.character(as.Date(date)+2)) %>%
    left_join(oil_full,by=c("datelag2"="date")) %>%
    mutate(datelag3 = as.character(as.Date(date)+3)) %>%
    left_join(oil_full,by=c("datelag3"="date")) %>%
    mutate(datelag7 = as.character(as.Date(date)+7)) %>%
    left_join(oil_full,by=c("datelag7"="date")) %>%
    rename(oil = dcoilwtico.x, avg3=avg3.x,avg7=avg7.x,avg15=avg15.x,avg30=avg30.x,
           oil_lag1 = dcoilwtico.y,avg3_lag1=avg3.y,avg7_lag1=avg7.y,avg15_lag1=avg15.y,avg30_lag1=avg30.y,
           oil_lag2 = dcoilwtico.x.x, avg3_lag2=avg3.x.x,avg7_lag2=avg7.x.x,avg15_lag2=avg15.x.x,avg30_lag2=avg30.x.x,
           oil_lag3 = dcoilwtico.y.y,avg3_lag3=avg3.y.y,avg7_lag3=avg7.y.y,avg15_lag3=avg15.y.y,avg30_lag3=avg30.y.y,
           oil_lag7 = dcoilwtico,avg3_lag7=avg3,avg7_lag7=avg7,avg15_lag7=avg15,avg30_lag7=avg30)
test_big$day_of_week <- apply(test_big,1,CorrectDate)
test_big <- test_big %>%
    left_join(special_dates,by="date") %>%
    filter(city==locale_name | state==locale_name |
               locale_name == "Ecuador" | is.na(locale_name))

