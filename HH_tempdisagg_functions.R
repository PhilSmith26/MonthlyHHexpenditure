# Functions for use in temporal disaggregation of HH final consumption
# September 22, 2022

#------------------------------------------------------------------------------
# Function to plot a quarterly series VALUE (in a data frame df1) and
# a time-disaggregated monthly series VALUE (in a data frame df2). The
# two series must both be named VALUE and the two data frames must both
# contain Date vectors beginning in 2018-01-01 or earlier
PlotTempDisagg <- function(df1,df2,nam) {
  chrt <- ggplot()+
    geom_line(data=filter(df1,Date>=as.Date("2018-01-01")),
      aes(Date,VALUE),colour="red",size=1.3)+
    geom_point(data=filter(df1,Date>=as.Date("2018-01-01")),
      aes(Date,VALUE),colour="red")+
    geom_line(data=filter(df2,Date>=as.Date("2018-01-01")),
      aes(Date,VALUE),colour="blue",size=0.9)+
    geom_point(data=filter(df2,Date>=as.Date("2018-01-01")),
      aes(Date,VALUE),colour="blue")+
    labs(title=paste0("Chart for ",nam," = ",HHCE[[nam]]$betterName),
      subtitle=paste0("Red = original quarterly ",
      "series, Blue = new monthly series\nThe quarterly series is ",
      "plotted at the first month of the quarter"),x="",y="")+
    scale_y_continuous(position="right")+
    theme(plot.title = element_text(size=16,face="bold")) +
    theme(plot.subtitle = element_text(size=12)) +
    theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
    theme(panel.border = element_rect(fill=NA,colour="black"))+
    theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
    theme(axis.text.x = element_text(angle=0,hjust=0,size=10)) +
    theme(axis.text.y = element_text(size=10))  
  print(chrt)
  readline(prompt="Press [enter] to continue")
}
#------------------------------------------------------------------------------
# Function to convert a quarterly series to monthly with no driver series,
# using the Denton-Cholette procedure. The case of CANN (cannabis) is an
# exception: from firstqtr to the end of 2018 the Denton-Cholette procedure
# is used but from 2019 forward one driver series is used.
# Inputs:     x - any vector with range from firstqtr to lastqtr
#           nam - name of the variable being processed
#      rounding - rounding number of digits after the decimal
#         Pause - T or F to pause after chart
#------------------------------------------------------------------------------
MakeMonthlyNoDriver <- function(x,nam,rounding,Pause) {
  if (nam=="CANN") { # Exception for cannabis - indicator starts late
    df1 <- data.frame(Date=Qdata$Date,VALUE=Qdata[["CANN"]])
    df2 <- data.frame(Date=CLdata$Date,VALUE=CLdata[["CANN"]]) 
    df1a <- filter(df1,Date<as.Date("2018-10-01"))
    df1b <- filter(df1,Date>=as.Date("2018-10-01"))
    df2a <- filter(df2,Date<as.Date("2018-10-01"))
    df2b <- filter(df2,Date>=as.Date("2018-10-01"))
    result1 <- predict(td(df1a~1,conversion="average",to="monthly",
      method="denton-cholette",criterion = "proportional")) 
    result1 <- rename(result1,"Date"="time","VALUE"="value")
    result2 <- predict(td(df1b~1+df2b,conversion="average",to="monthly",
      method="chow-lin-maxlog")) 
    result2 <- rename(result2,"Date"="time","VALUE"="value")
    result <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"),
      VALUE=c(result1$VALUE,result2$VALUE))    
  } else {
    df1 <- data.frame(Date=seq.Date(firstqtr,lastqtr,"quarter"),VALUE=x)
    result <- predict(td(df1~1,conversion="average",to="monthly",
      method="denton-cholette",criterion = "proportional")) 
    result <- rename(result,"Date"="time","VALUE"="value")
    result$VALUE <- round(result$VALUE,rounding)
  }
  if (Pause) {
    PlotTempDisagg(df1,result,nam)
  }
  return(result$VALUE)
}
#------------------------------------------------------------------------------
# Function for tempdisagg with one driver series using Chow-Lin
# For HFCE at current prices
# Inputs: var   - name of a column in the quarterly Qdata and 
#                 monthly CLdata data frames
#      rounding - rounding number of digits after the decimal
#         Pause - T or F to pause after chart
#------------------------------------------------------------------------------
MakeMonthly1Driver <- function(MODELS_ChowLin,nam,rounding,Pause) {
  if (nam=="CANN") { # Exception for cannabis - indicator starts late
    df1 <- data.frame(Date=Qdata$Date,VALUE=Qdata[["CANN"]])
    df2 <- data.frame(Date=CLdata$Date,VALUE=CLdata[["CANN"]]) 
    df1a <- filter(df1,Date<as.Date("2018-10-01"))
    df1b <- filter(df1,Date>=as.Date("2018-10-01"))
    df2a <- filter(df2,Date<as.Date("2018-10-01"))
    df2b <- filter(df2,Date>=as.Date("2018-10-01"))
    result1 <- predict(td(df1a~1,conversion="average",to="monthly",
      method="denton-cholette",criterion = "proportional")) 
    result1 <- rename(result1,"Date"="time","VALUE"="value")
    result2 <- predict(td(df1b~1+df2b,conversion="average",to="monthly",
      method="chow-lin-maxlog")) 
    result2 <- rename(result2,"Date"="time","VALUE"="value")
    result <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"),
      VALUE=c(result1$VALUE,result2$VALUE))
  } else {
    df1 <- data.frame(Date=Qdata$Date,VALUE=Qdata[[nam]])
    df2 <- data.frame(Date=CLdata$Date,VALUE=CLdata[[nam]]) # getdf2(var)
    model <- td(df1~1+df2,conversion="average",to="monthly",
      method="chow-lin-maxlog")
    result <- predict(model) 
    MODELS_ChowLin[[length(MODELS_ChowLin)+1]] <- summary(model)
    names(MODELS_ChowLin)[length(MODELS_ChowLin)] <- nam
    result <- rename(result,"Date"="time","VALUE"="value")
    result$VALUE <- round(result$VALUE,rounding)
  }
  if (Pause) {
    PlotTempDisagg(df1,result,nam)
  }
  return(NewList <- list(MODELS_ChowLin,result$VALUE))
}
#------------------------------------------------------------------------------
# Function for tempdisagg with two driver series using Chow-Lin
# For HFCE at current prices
# Inputs: var1  - name of a column in the quarterly Qdata data frame
#         var2  - name of a driver column in the monthly CLE1data data frame
#         var3  - name of a 2nd driver column in the monthly CLE2data data frame
#     rounding  - rounding number of digits after the decimal
#        Pause  - T or F to pause after chart
#------------------------------------------------------------------------------
MakeMonthly2Driver <- function(MODELS_ChowLin,var,rounding,Pause) {
  df1 <- data.frame(Date=Qdata$Date,VALUE=Qdata[[nam]])
  df2 <- data.frame(Date=CLE1data$Date,VALUE=CLE1data[[nam]])
  df3 <- data.frame(Date=CLE2data$Date,VALUE=CLE2data[[nam]])
  model <- td(df1~1+df2+df3,conversion="average",to="monthly",
    method="chow-lin-maxlog")
  result <- predict(model) 
  MODELS_ChowLin[[length(MODELS_ChowLin)+1]] <- summary(model)
  names(MODELS_ChowLin)[length(MODELS_ChowLin)] <- var
  result <- rename(result,"Date"="time","VALUE"="value")
  result$VALUE <- round(result$VALUE,rounding)
  if (Pause) {
    PlotTempDisagg(df1,result,nam)
  }
  Val <- result$VALUE
  return(NewList <- list(MODELS_ChowLin,Val))
}
#------------------------------------------------------------------------------
# Function for tempdisagg with one driver series using Chow-Lin
# For HFCE at current prices
# Inputs: 
# var   - name of a column in the quarterly Qdata and 
#         monthly CLdata data frames
# Pause - T or F to pause after chart
MakeMonthlyP1Driver <- function(MODELSP_ChowLin,nam,Pause) {
  if (nam=="CANN") {
    df1 <- data.frame(Date=Pdata$Date,VALUE=Pdata$CANN)
    df2 <- data.frame(Date=Defdata$Date,VALUE=Defdata$CANN) 
    df1a <- filter(df1,Date<=as.Date("2019-01-01"))
    df1b <- filter(df1,Date>=as.Date("2019-01-01"))
    df2a <- filter(df2,Date<=as.Date("2019-01-01"))
    df2b <- filter(df2,Date>=as.Date("2019-01-01"))
    result1 <- predict(td(df1a~1,conversion="average",to="monthly",
      method="denton-cholette",criterion = "proportional")) 
    result1 <- rename(result1,"Date"="time","VALUE"="value")
    part1 <- round(result1$VALUE,3)
    model <- td(df1b~1+df2b,conversion="average",to="monthly",
      method="chow-lin-maxlog")
    result2 <- predict(model)
    MODELSP_ChowLin[[length(MODELSP_ChowLin)+1]] <- summary(model)
    names(MODELSP_ChowLin)[length(MODELSP_ChowLin)] <- nam
    result2 <- rename(result2,"Date"="time","VALUE"="value")
    part2 <- round(result2$VALUE,3)
    part1 <- part1*143.6/144.0 # linking to CPI that starts in Jan 2019
    part1 <- part1[1:108]
    result <- data.frame(Date=seq.Date(firstmon,lastmonF,"month"),
      VALUE=c(part1,part2)) 
  } else {
    df1 <- data.frame(Date=Pdata$Date,VALUE=Pdata[[nam]])
    df2 <- data.frame(Date=Defdata$Date,VALUE=Defdata[[nam]])
    model <- td(df1~1+df2,conversion="average",to="monthly",
      method="chow-lin-maxlog")
    result <- predict(model) 
    MODELSP_ChowLin[[length(MODELSP_ChowLin)+1]] <- summary(model)
    names(MODELSP_ChowLin)[length(MODELSP_ChowLin)] <- nam
    result <- rename(result,"Date"="time","VALUE"="value")
    result$VALUE <- round(result$VALUE,3)
  }
  if (Pause) {
    PlotTempDisagg(df1,result,nam)
  }
  return(NewList <- list(MODELSP_ChowLin,round(result$VALUE,3)))
}
#------------------------------------------------------------------------------
# Function to convert a data frame from monthly to quarterly by averaging.
# The data frame must have a Date column called 'Date' as its first column.
M_to_Q <- function(df) {
  df <- df %>%
    group_by(quarter = paste(quarters(Date), lubridate::year(Date))) %>%
    summarise(across(2:ncol(df),function(x) mean(x)))
  df <- mutate(df,qtr=case_when(
    substr(quarter,2,2)==1~"01",
    substr(quarter,2,2)==2~"04",
    substr(quarter,2,2)==3~"07",
    substr(quarter,2,2)==4~"10"
  ))
  df <- mutate(df,Date=paste0(substr(quarter,4,7),"-",qtr,"-01"))
  df <- arrange(df,Date)
  df <- select(df,-quarter,-qtr)
  df <- select(df,Date,everything())
  df <- mutate(df,across(2:ncol(df),function(x) round(x,0)))
}
#------------------------------------------------------------------------------
# Function to compute a month difference (eg. May 2020 minus Nov 2019 = 6)
mondf <- function(d1, d2) {
  lt1 <- as.POSIXlt(as.Date(d1, origin="1900-01-01"))
  dt1 <- lt1$year*12 + lt1$mon
  lt2 <- as.POSIXlt(as.Date(d2, origin="1900-01-01"))
  dt2 <- lt2$year*12 + lt2$mon
  return(dt2-dt1)
}
#------------------------------------------------------------------------------
# Function to make forecasts with auto.arima
# Inputs: x  - a monthly time series to be projected forward
#         Nf - the number of months to be projected forward
ArimaForecast <- function(x,Nf) {
  tmp1 <- forecast(auto.arima(x),Nf)
  tmp2 <- as.numeric(tmp1$mean)
  y <- c(x,tmp2) # return the original series with the forecast appended
}
#------------------------------------------------------------------------------
# Backcast a driver time series with auto.arima
# Inputs: x      - a monthly time series to be backcasted
#         num    - the number of months to backcast
#         stDate - the first date
Backcast <- function(x,num,stDate) {
  revx <- ts(rev(x),frequency=12) # use rev() function to reverse series
  revx <- as.numeric(revx[1:length(revx)]) # convert back to numeric from ts
  fc <- forecast(auto.arima(revx),num)
  revx1 <- c(revx,fc$mean) # extend with forecasts (in fc$mean)
  m <- month(as.POSIXlt(stDate,format="%Y-%m-%d"))
  y <- year(as.POSIXlt(stDate,format="%Y-%m-%d"))
  x <- as.numeric(ts(rev(revx1),start=c(y,m),frequency=12))
  return(x)
}
#------------------------------------------------------------------------------
# Seasonal adjustment with a trycatch escape for cases,
# such as many CPIs, that fail
DoSeasAdj <- function(x,start_year) {
  out <- tryCatch(
    { 
      x1 <- ts(data=x,start=start_year,frequency=12)
      suppressWarnings(x1_seas <- seas(x1))
      x1_sa <- final(x1_seas)
      y <- as.numeric(x1_sa)
      return(y) # will be returned if no errors
    },
    error=function(cond) {
        message(paste("Seasonal adjustment failed for ",x))
        # message("Here's the original error message:")
        # message(cond)
        # Choose a return value in case of error
        return(10)
    },
    warning=function(cond) {
        message(paste("Seasonal adjustment has warnings for ",x))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(20)
    },
    finally={
    # Put anything that should be executed at the end here,
    # regardless of success or error.
    }
  )    
  return(out)
}
#------------------------------------------------------------------------------
# Pad a series at the back ("b") or front ("f") with num zeros 
PADzero <- function(x,num,w) {
  if(w=="b") {
    tmp <- rep(0,num)
    x <- c(tmp,x)
  } else {
    tmp <- rep(0,num)
    x <- c(x,tmp)
  }
}

