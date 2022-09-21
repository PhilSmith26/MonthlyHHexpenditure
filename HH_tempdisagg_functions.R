# Functions for use in tempdisagg for HH final 
# consumption expenditure
# August 22, 2022

#------------------------------------------------------------------------------
# Function for tempdisagg with no driver series, using Denton-Cholette
# Inputs:     x - any vector with range from firstqtr to lastqtr
#           nam - name of the variable being processed
#      rounding - rounding number of digits after the decimal
#         Pause - T or F to pause after chart
#------------------------------------------------------------------------------
MakeMonthly00 <- function(x,nam,rounding,Pause) {
  df1 <- data.frame(Date=seq.Date(firstqtr,lastqtr,"quarter"),VALUE=x)
  result <- predict(td(df1~1,conversion="average",to="monthly",
    method="denton-cholette",criterion = "proportional")) 
  result <- rename(result,"Date"="time","VALUE"="value")
  result$VALUE <- round(result$VALUE,rounding)
  if (Pause) {
    chrt <- ggplot()+
      geom_line(data=filter(result,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="blue")+
      geom_point(data=filter(result,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="blue")+
      geom_line(data=filter(df1,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="red",size=1.3)+
      geom_point(data=filter(df1,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="red")+
      labs(title=paste0("Chart for ",nam,"\nRed = original quarterly ",
        "series, blue = new monthly series\nThe quarterly series is ",
        "plotted at the first month of the quarter"))
    print(chrt)
    readline(prompt="Press [enter] to continue")
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
MakeMonthly1 <- function(MODELS_ChowLin,var,rounding,Pause) {
  df1 <- data.frame(Date=Qdata$Date,VALUE=Qdata[[var]])
  df2 <- data.frame(Date=CLdata$Date,VALUE=CLdata[[var]]) # getdf2(var)
  model <- td(df1~1+df2,conversion="average",to="monthly",
    method="chow-lin-maxlog")
  result <- predict(model) 
  MODELS_ChowLin[[length(MODELS_ChowLin)+1]] <- summary(model)
  names(MODELS_ChowLin)[length(MODELS_ChowLin)] <- var
  result <- rename(result,"Date"="time","VALUE"="value")
  result$VALUE <- round(result$VALUE,rounding)
  if (Pause) {
    chrt <- ggplot()+
      geom_line(data=filter(result,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="blue")+
      geom_point(data=filter(result,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="blue")+
      geom_line(data=filter(df1,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="red",size=1.3)+
      geom_point(data=filter(df1,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="red")+
      labs(title=paste0("Chart for ",var,"\nRed = original quarterly ",
        "series, blue = new monthly series\nThe quarterly series is ",
        "plotted at the first month of the quarter"))
    print(chrt)
    readline(prompt="Press [enter] to continue")
  }
  Val <- result$VALUE
  return(NewList <- list(MODELS_ChowLin,Val))
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
MakeMonthly2 <- function(MODELS_ChowLin,var,rounding,Pause) {
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
    chrt <- ggplot()+
      geom_line(data=filter(result,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="blue")+
      geom_point(data=filter(result,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="blue")+
      geom_line(data=filter(df1,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="red",size=1.3)+
      geom_point(data=filter(df1,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="red")+
      labs(title=paste0("Chart for ",nam,"\nRed = original quarterly ",
        "series, blue = new monthly series\nThe quarterly series is ",
        "plotted at the first month of the quarter"))
    print(chrt)
    readline(prompt="Press [enter] to continue")
  }
  Val <- result$VALUE
  return(NewList <- list(MODELS_ChowLin,Val))
}
#------------------------------------------------------------------------------
# Function for tempdisagg with no driver series using Denton-Cholette
# SPECIAL CASE: Cannabis, for which the driver is available from 2019-01-01
# For HFCE at current prices
MakeMonthly1CANN <- function(rounding,Pause) {
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
  finresult <- c(result1$VALUE,result2$VALUE)
  tmp <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"),
    finresult=finresult)
  if (Pause) {
    chrt <- ggplot()+
      geom_line(data=filter(tmp,Date>=as.Date("2018-01-01")),
        aes(Date,finresult),colour="blue")+
      geom_point(data=filter(tmp,Date>=as.Date("2018-01-01")),
        aes(Date,finresult),colour="blue")+
      geom_line(data=filter(df1,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="red",size=1.3)+
      geom_point(data=filter(df1,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="red")+
      labs(title=paste0("Chart for CANN\nRed = original quarterly ",
        "series, blue = new monthly series\nThe quarterly series is ",
        "plotted at the first month of the quarter"))
    print(chrt)
    readline(prompt="Press [enter] to continue")
  }
  return(round(finresult,rounding))
}
#------------------------------------------------------------------------------
# Function for tempdisagg with one driver series using Chow-Lin
# For HFCE at current prices
# Inputs: 
# var   - name of a column in the quarterly Qdata and 
#         monthly CLdata data frames
# Pause - T or F to pause after chart
MakeMonthlyP1 <- function(MODELSP_ChowLin,var,Pause) {
  df1 <- data.frame(Date=Pdata$Date,VALUE=Pdata[[var]])
  df2 <- data.frame(Date=Defdata$Date,VALUE=Defdata[[var]])
  model <- td(df1~1+df2,conversion="average",to="monthly",
    method="chow-lin-maxlog")
  result <- predict(model) 
  MODELSP_ChowLin[[length(MODELSP_ChowLin)+1]] <- summary(model)
  names(MODELSP_ChowLin)[length(MODELSP_ChowLin)] <- var
  result <- rename(result,"Date"="time","VALUE"="value")
  result$VALUE <- round(result$VALUE,3)
  if (Pause) {
    chrt <- ggplot()+
      geom_line(data=filter(result,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="blue")+
      geom_point(data=filter(result,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="blue")+
      geom_line(data=filter(df1,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="red",size=1.3)+
      geom_point(data=filter(df1,Date>=as.Date("2018-01-01")),
        aes(Date,VALUE),colour="red")+
      labs(title=paste0("Chart for ",var,"\nRed = original quarterly ",
        "series, blue = new monthly series\nThe quarterly series is ",
        "plotted at the first month of the quarter"))
    print(chrt)
    readline(prompt="Press [enter] to continue")
  }
  Val <- round(result$VALUE,3)
  return(NewList <- list(MODELSP_ChowLin,Val))
}
#------------------------------------------------------------------------------
# Function for tempdisagg with no driver series using Denton-Cholette
# SPECIAL CASE: Cannabis, for which the driver is available from 2019-01-01
# For HFCE price indexes
MakeMonthlyP1CANN <- function(Pause) { # Cannabis special case for Pdata
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
  result2 <- predict(td(df1b~1+df2b,conversion="average",to="monthly",
    method="chow-lin-maxlog")) 
  result2 <- rename(result2,"Date"="time","VALUE"="value")
  part2 <- round(result2$VALUE,3)
  part1 <- part1*143.6/144.0 # linking to CPI that starts in Jan 2019
  part1 <- part1[1:108]
  finresult <- c(part1,part2)
  if (Pause) {
    df <- data.frame(Date=seq.Date(firstmon,lastmonF,"month"),
      VALUE=finresult)
    chrt <- ggplot()+
      geom_line(data=df1,aes(Date,VALUE),colour="red",size=1.3)+
      geom_point(data=df1,aes(Date,VALUE),colour="red",size=1.3)+
      geom_line(data=df,aes(Date,VALUE),colour="blue")+
      geom_point(data=df,aes(Date,VALUE),colour="blue")+
      labs(title=paste0("Chart for CANN\nRed = original quarterly ",
        "series, blue = new monthly series\nThe quarterly series is ",
        "plotted at the first month of the quarter"))
    print(chrt)
    readline(prompt="Press [enter] to continue")
  }
  return(round(finresult,3))
}
#------------------------------------------------------------------------------
# Function to convert a df from monthly to quarterly by averaging.
# The df must have a Date column called 'Date' as its first column.
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
  #df <- select(df,-quarter,-qtr)
  #df <- select(df,date,everything())
}
#------------------------------------------------------------------------------
# Function to turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
                          lt$year*12 + lt$mon } 
#------------------------------------------------------------------------------
# Function to compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
#------------------------------------------------------------------------------
# Code to take mondf for a spin
mondf(as.Date("2008-01-01"), Sys.Date())
#------------------------------------------------------------------------------
# Make forecasts with auto.arima
# Inputs: 
# x  - a monthly time series to be projected forward
# Nf - the number of months to be projected forward
ArimaForecast <- function(x,Nf) {
  tmp1 <- forecast(auto.arima(x),Nf)
  tmp2 <- as.numeric(tmp1$mean)
  y <- c(x,tmp2) # return the original series with the forecast appended
}
#------------------------------------------------------------------------------
# Function to backcast a driver time series
# Inputs:
# x      - a monthly time series to be backcasted
# num    - the number of months to backcast
# stDate - the first date
backcast <- function(x,num,stDate) {
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
# Function to do seasonal adjustment with a trycatch escape for cases
# such as many CPIs that fail
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
        #message("Here's the original error message:")
        #message(cond)
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
    # NOTE:
    # Here goes everything that should be executed at the end,
    # regardless of success or error.
    # If you want more than one expression to be executed, then you 
    # need to wrap them in curly brackets ({...}); otherwise you could
    # just have written 'finally=<expression>' 
    #    message(paste("Processed URL:", url))
    #    message("Some other message at the end")
    }
  )    
  return(out)
}
#------------------------------------------------------------------------------
SEASADJ <- function(x,start_year) {
  x1 <- ts(data=x,start=start_year,frequency=12)
  x1_seas <- seas(x1)
  x1_sa <- final(x1_seas)
  y <- as.numeric(x1_sa)
  return(y)
}
#------------------------------------------------------------------------------
PAD <- function(x,num,w) { # Pad a series at the back ("b") or front ("f") with num NAs 
  if(w=="b") {
    tmp <- rep(NA,num)
    x <- c(tmp,x)
  } else {
    tmp <- rep(NA,num)
    x <- c(x,tmp)
  }
}
#------------------------------------------------------------------------------
PAD1 <- function(x,num,w) { # Pad a series at the back ("b") or front ("f") with num zeros 
  if(w=="b") {
    tmp <- rep(0,num)
    x <- c(tmp,x)
  } else {
    tmp <- rep(0,num)
    x <- c(x,tmp)
  }
}
