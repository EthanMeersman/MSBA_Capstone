library(fpp3)
library(stargazer)

btc  <- read.csv('cleaned_btc.csv')

btc2 <- mutate(btc, Date=as.Date(Timestamp, "%m/%d/%y"))
btc3 <- mutate(btc2, day=day(Date))
btc3[1102, 2]=286.19
btc3[1103, 2]=294.34
btc3[1104, 2]=283.35
btc4 <- filter(btc3, day==1)
btc5 <- mutate(btc4, Month=yearmonth(Date))%>%select(-Date, -day)
btc6 <- as_tsibble(btc5, index=Month)

autoplot(btc6, log(Close))+
  labs(x="Date by month", y="log Close", title=
         "The log of Bitcoin closing price over time")

gg_season(btc6, log(Close))

gg_subseries(btc6, log(Close))

features(btc6, log(Close), unitroot_nsdiffs)
features(btc6, log(Close), unitroot_ndiffs)

btc.train <- filter_index(btc6, "2019 Jan" ~ "2020 Dec")
btc.test <- filter_index(btc6, "2021 Jan" ~.)

ets.model <- btc.train%>%model(ETS(log(Close)))
report(ets.model)

ets.forecast <- ets.model%>%forecast(h=15)
autoplot(ets.forecast)

autoplot(ets.forecast, level=80)+
  autolayer(btc6, Close)

# Zoomed in
autoplot(ets.forecast, level=80)+
  autolayer(btc.test, Close)

gg_tsresiduals(ets.model)

arima.model <- btc.train%>%model(ARIMA(log(Close)))
arima.forecast <- arima.model%>%forecast(h=15)

report(arima.model)

autoplot(arima.forecast, level=80)+
  autolayer(btc6, Close)

# Zoomed in
autoplot(arima.forecast, level=80)+
  autolayer(btc.test, Close)

#####btc.test <- rename(btc.test, Month=Timestamp)
table <- full_join(btc.test, ets.forecast, by = "Month")%>%
  select(Month, Close.x, .mean)%>%rename(Close=Close.x,fore.ets=.mean)

table <- full_join(table, arima.forecast, by = "Month")%>%
  select(Month, Close.x, fore.ets, .mean.x)%>%
  rename(Close=Close.x, fore.arima=.mean.x)

accuracy(ets.forecast, btc6)
accuracy(arima.forecast, btc6)

autoplot(ets.forecast, level=NULL, color="red")+
  autolayer(arima.forecast, level = NULL)+
  autolayer(btc.test)

rw.model <- btc.train%>%model(RW(log(Close)~drift()))
report(rw.model)

rw.forecast <- rw.model%>%forecast(h=15)

autoplot(ets.forecast, level=NULL, color="red")+
  autolayer(arima.forecast, level = NULL)+
  autolayer(btc.test)+autolayer(rw.forecast, level=NULL)

accuracy(ets.forecast, btc6)
accuracy(arima.forecast, btc6) ## BEST ONE
accuracy(rw.forecast, btc6)













