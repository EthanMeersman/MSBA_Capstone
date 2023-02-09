#Quantity#

library(fpp3)
library(stargazer)

btc  <- read.csv('cleaned_btc.csv')

btc2 <- mutate(btc, Date=as.Date(Timestamp, "%m/%d/%y"))
btc3 <- mutate(btc2, day=day(Date))
btc3[1102, 3]=23.2457
btc3[1103, 3]=24.8668
btc3[1104, 3]=19.9825
btc3a <- btc3%>%rename(Volume = Volume_.BTC.)%>%
  mutate(Month=month(Date), Year=year(Date))
btc.agg <- btc3a%>%group_by(Year, Month)%>%
  summarize(total.vol = sum(Volume), date=first(Date))%>%
  ungroup()

btc.agg <- mutate(btc.agg, monthyear = yearmonth(date))%>%
  as_tsibble(index=monthyear)%>%
  select(monthyear, total.vol)
is_tsibble(btc.agg)

autoplot(btc.agg, total.vol)

gg_season(btc.agg, total.vol)

gg_subseries(btc.agg, total.vol)

features(btc.agg, total.vol, unitroot_nsdiffs)
features(btc.agg, total.vol, unitroot_ndiffs)

btc.train <- filter_index(btc.agg, "2012 Jan" ~ "2019 Dec")
btc.test <- filter_index(btc.agg, "2020 Jan" ~.)

## ETS Model
ets.model <- btc.train%>%model(ETS(total.vol))
report(ets.model)

ets.forecast <- ets.model%>%forecast(h=15)
autoplot(ets.forecast)

autoplot(ets.forecast, level=80)+
  autolayer(btc.agg, total.vol)

## ARIMA model
arima.model <- btc.train%>%model(ARIMA(total.vol))
arima.forecast <- arima.model%>%forecast(h=15)

report(arima.model)

##arima.model2 <- btc.train%>%model(ARIMA(total.vol)~PDQ(0,0,0))
##report(arima.model2)

features(btc.train, total.vol, unitroot_nsdiffs)

autoplot(arima.forecast, level=80)+
  autolayer(btc.agg, total.vol)

## Random Walk model
rw.model <- btc.train%>%model(RW(total.vol))#~drift())
report(rw.model)

rw.forecast <- rw.model%>%forecast(h=15)

##Accuracy 
accuracy(ets.forecast, btc.agg)
accuracy(arima.forecast, btc.agg)
accuracy(rw.forecast, btc.agg)
