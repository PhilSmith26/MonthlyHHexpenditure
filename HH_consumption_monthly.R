# Temporal disaggregation of consumer expenditure from Q to M
# Value, volume and price
# September 13, 2022

setwd(paste0("/Users/philipsmith/Documents/R/Households/",
  "HH_consumption/Consumption_monthly"))

pkgs <- c("cansim","tidyverse","lubridate","anytime","ggthemes","ggpubr",
          "gridExtra","grid","gtable","reshape2","ggrepel","pracma",
          "seasonal","FactoMineR","RcppBDT","forecast","profvis",
          "RColorBrewer","WriteXLS","gdata","scales","ggpubr","ggtext",
          "tibble","simplecolors","stringr","tempdisagg","gt","canbank")
suppressPackageStartupMessages({
  inst <- lapply(pkgs,library,character.only=TRUE)
})
source(paste0("/Users/philipsmith/Documents/R/Households/HH_consumption/",
  "Consumption_monthly/HH_tempdisagg_functions.R"))

firstqtr <- as.Date("2010-01-01")
firstqtrC <- "2010-01-01"
lastqtr <- as.Date("2022-04-01")
firstmon <- as.Date("2010-01-01")
firstmonC <- "2010-01-01"
lastmon <- as.Date("2022-06-01")
lastmonF <- as.Date("2022-07-01") 
(Nobs <- mondf(firstmon,lastmonF)+1)

if (lastmonF<=lastmon) {
  print("lastmonF must be greater than lastmon")
  stop
}

# This big list contains all the meta data
HHCE <- list(
  HHFC=list(name="Household final consumption expenditure [C]",
    betterName="Household final consumption expenditure",
    vnumber="61989072",
    ChaVolnumber="v61989012",
    ConVolnumber="v62700682",
    AdjEntry="v62700809",
    deflator="NA",
    type="identity",
    equation="FB+ATC+CF+HWEG+FEOD+HEAL+TRAN+COMM+RC+EDU+FBAS+FIN+MISC+NEA",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  FB=list(name="Food and non-alcoholic beverages [C11]",
    betterName="Food and non-alcoholic beverages",
    vnumber="v61989018", # vnumber for quarterly at current prices
    ChaVolnumber="v61988958", # vnumber for quarterly at chained prices
    ConVolnumber="v62700683", # vnumber for quarterly at 2012 constant prices
    deflator="v41690975", # CPI used as a monthly deflation indicator
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v52367105",
    driver2="NA",
    d2_vnumber="NA"),
  ATC=list(name="Alcoholic beverages, tobacco and cannabis [C12]",
    betterName="Alcoholic beverages, tobacco and cannabis",
    vnumber="v61989021",
    ChaVolnumber="v61988961",
    ConVolnumber="v62700687",
    AdjEntry="v62700690",
    deflator="NA",
    type="identity",
    equation="AB+TOB+CANN",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  AB=list(name="Alcoholic beverages [C121]",
    betterName="Alcoholic beverages",
    vnumber="v61989022",
    ChaVolnumber="v61988962",
    ConVolnumber="v62700688",
    deflator="v41691212",
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v52367115",
    driver2="NA",
    d2_vnumber="NA"),
  TOB=list(name="Tobacco [C122]",
    betterName="Tobacco",
    vnumber="v61989023",
    ChaVolnumber="v61988963",
    ConVolnumber="v62700689",
    deflator="v41691216",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691216", # CPI tobacco products and smokers' supplies
    driver2="NA",
    d2_vnumber="NA"),
  CANN=list(name="Cannabis [C123]",
    betterName="Cannabis",
    vnumber="v1043024086",
    ChaVolnumber="v1043024085",
    ConVolnumber1="v1043024093",
    ConVolnumber2="v1043024094",
    deflator="v1043024072",
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v1035339099",
    driver2="NA",
    d2_vnumber="NA"),
  CF=list(name="Clothing and footwear [C13]",
    betterName="Clothing and footwear",
    vnumber="v61989024",
    ChaVolnumber="v61988964",
    ConVolnumber="v62700691",
    AdjEntry="v62700696",
    deflator="NA",
    type="identity",
    equation="CLO+FOOT",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  CLO=list(name="Clothing [C131]",
    betterName="Clothing",
    vnumber="v61989025",
    ChaVolnumber="v61988965",
    ConVolnumber1="v62700692",
    ConVolnumber2="v62700693",
    ConVolnumber3="v62700694",
    deflator="v41691109",
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v52367123",
    driver2="NA",
    d2_vnumber="NA"),
  FOOT=list(name="Footwear [C132]",
    betterName="Footwear",
    vnumber="v61989026",
    ChaVolnumber="v61988966",
    ConVolnumber="v62700695",
    deflator="v41691113",
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v52367123",
    driver2="NA",
    d2_vnumber="NA"),
  HWEG=list(name="Housing, water, electricity, gas and other fuels [C14]",
    betterName="Housing, water, electricity, gas and other fuels",
    vnumber="v61989027",
    ChaVolnumber="v61988967",
    ConVolnumber="v62700697",
    AdjEntry="v62700706",
    deflator="NA",
    type="identity",
    equation="RENT+IMPR+MAIN+WAT+EGF",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  RENT=list(name="Paid rental fees for housing [C141]",
    betterName="Paid rental fees for housing",
    vnumber="v61989028",
    ChaVolnumber="v61988968",
    ConVolnumber="v62700698",
    deflator="v41691052",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691052",
    driver2="NA",
    d2_vnumber="NA"),
  IMPR=list(name="Imputed rental fees for housing [C142]",
    betterName="Imputed rental fees for housing",
    vnumber="v61989029",
    ChaVolnumber="v61988969",
    ConVolnumber="v62700699",
    deflator="v41691057",
    type="Chow-Lin",
    equation="NA",
    driver="GDP",
    d_vnumber="v65201422",
    driver2="NA",
    d2_vnumber="NA"),
  MAIN=list(name="Maintenance and repair of the dwelling [C143]",
    betterName="Maintenance and repair of the dwelling",
    vnumber="v61989030",
    ChaVolnumber="v61988970",
    ConVolnumber1="v62700700",
    ConVolnumber2="v62700701",
    deflator="v41691060",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691060",
    driver2="RTS", # Building material and garden equipment and supplies
    d2_vnumber="v52367103"),
  WAT=list(name="Water supply and sanitation services [C144]",
    betterName="Water supply and sanitation services",
    vnumber="v61989031",
    ChaVolnumber="v61988971",
    ConVolnumber="v62700702",
    deflator="v41691064",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691064",
    driver2="GDP",
    d2_vnumber="v65201257"),
  EGF=list(name="Electricity, gas and other fuels [C145]",
    betterName="Electricity, gas and other fuels",
    vnumber="v61989032",
    ChaVolnumber="v61988972",
    ConVolnumber1="v62700703",
    ConVolnumber2="v62700704",
    ConVolnumber3="v62700705",
    deflator="v41691062",
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v52367121", # Gas stations
    driver2="CPI",
    d2_vnumber="v41691063"), # CPI electricity
  FEOD=list(name="Furnishings, household equipment and other goods and services related to the dwelling and property [C15]",
    betterName="Furnishings, household equipment and related goods and services",
    vnumber="v61989033",
    ChaVolnumber="v61988973",
    ConVolnumber="v62700707",
    AdjEntry="v62700720",
    deflator="NA",
    type="identity",
    equation="FFCF+HTEX+HAPP+TEHG+ODP",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  FFCF=list(name="Furniture, furnishings, carpets and other floor coverings [C151]",
    betterName="Furniture, furnishings, carpets and other floor coverings",
    vnumber="v61989034",
    ChaVolnumber="v61988974",
    ConVolnumber1="v62700708",
    ConVolnumber2="v62700709",
    deflator="v41691087",
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v52367153",
    driver2="NA",
    d2_vnumber="NA"),
  HTEX=list(name="Household textiles [C152]",
    betterName="Household textiles",
    vnumber="v61989035",
    ChaVolnumber="v61988975",
    ConVolnumber="v62700710",
    deflator="v41691093",
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v52367099",
    driver2="GDP", # Textile and textile product mills
    d2_vnumber="v65201280"),
  HAPP=list(name="Household appliances [C153]",
    betterName="Household appliances",
    vnumber="v61989036",
    ChaVolnumber="v61988976",
    ConVolnumber1="v62700711",
    ConVolnumber2="v62700712",
    deflator="v41691098",
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v52367101",
    driver2="NA",
    d2_vnumber="NA"),
  TEHG=list(name="Tools and equipment for house and garden [C154]",
    betterName="Tools and equipment for house and garden",
    vnumber="v61989037",
    ChaVolnumber="v61988977",
    ConVolnumber1="v62700713",
    ConVolnumber2="v62700714",
    deflator="v41691104",
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v52367103",
    driver2="GDP",
    d2_vnumber="v65201317"),
  ODP=list(name="Other goods and services related to the dwelling and property [C155]",
    betterName="Other goods and services related to the dwelling and property",
    vnumber="v61989038",
    ChaVolnumber="v61988978",
    ConVolnumber1="v62700715",
    ConVolnumber2="v62700716",
    ConVolnumber3="v62700717",
    ConVolnumber4="v62700718",
    ConVolnumber5="v62700719",
    deflator="v41691107",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691107", # CPI services related to HH furnishings and equipment
    driver2="NA",
    d2_vnumber="NA"),
  HEAL=list(name="Health [C16]",
    betterName="Health",
    vnumber="v61989039",
    ChaVolnumber="v61988979",
    ConVolnumber="v62700721", # Pharm is 50%, out-patient services is 35%
    deflator="v41691154",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691156", # Medicinal and pharmaceutical products
    driver2="GDP",
    d2_vnumber="v65201458"), # Ambulatory health care services
  TRAN=list(name="Transport [C17]",
    betterName="Transport",
    vnumber="v61989043",
    ChaVolnumber="v61988983",
    ConVolnumber="v62700727",
    AdjEntry="v62700745",
    deflator="NA",
    type="identity",
    equation="VEH+OTE+TS",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  VEH=list(name="Purchase of vehicles [C171]",
    betterName="Purchase of vehicles",
    vnumber="v61989044",
    ChaVolnumber="v61988984",
    ConVolnumber1="v62700728",
    ConVolnumber2="v62700729",
    ConVolnumber3="v62700730",
    ConVolnumber4="v62700731",
    deflator="v41691132",
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v52367143",
    driver2="NA",
    d2_vnumber="NA"),
  OTE=list(name="Operation of transport equipment [C172]",
    betterName="Operation of transport equipment",
    vnumber="v61989045",
    ChaVolnumber="v61988985",
    ConVolnumber1="v62700732", # vehicle spare parts and accessories (24%)
    ConVolnumber2="v62700733", # Fuels and lubricants (55%)
    ConVolnumber3="v62700734", # Vehicle maintenance and repair (14%)
    ConVolnumber4="v62700735", # Parking
    ConVolnumber5="v62700736", # Passenger vehicle renting
    ConVolnumber6="v62700737", # Other related services
    deflator="v41691135",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691135",
    driver2="GDP",
    d2_vnumber="v65201393"),
  TS=list(name="Transport services [C173]",
    betterName="Transport services",
    vnumber="v61989046",
    ChaVolnumber="v61988986",
    ConVolnumber1="v62700738",
    ConVolnumber2="v62700739",
    ConVolnumber3="v62700740",
    ConVolnumber4="v62700741",
    ConVolnumber5="v62700742",
    ConVolnumber6="v62700743",
    ConVolnumber7="v62700744",
    deflator="v41691146",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691146",
    driver2="GDP",
    d2_vnumber="v65201381"),
  COMM=list(name="Communications [C18]",
    betterName="Communications",
    vnumber="v61989047",
    ChaVolnumber="v61988987",
    ConVolnumber="v62700746",
    deflator="v41691069",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691069",
    driver2="NA",
    d2_vnumber="NA"),
  RC=list(name="Recreation and culture [C19]",
    betterName="Recreation and culture",
    vnumber="v61989049",
    ChaVolnumber="v61988989",
    ConVolnumber="v62700751",
    AdjEntry="v62700771",
    deflator="NA",
    type="identity",
    equation="AVPI+ODRC+OREG+RCS+NBS",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  AVPI=list(name="Audio-visual, photographic and information processing equipment [C191]",
    betterName="Audio-visual, photographic and information processing equipment",
    vnumber="v61989050",
    ChaVolnumber="v61988990",
    ConVolnumber1="v62700752",
    ConVolnumber2="v62700753",
    ConVolnumber3="v62700754",
    deflator="v41691184",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691184", # CPI home entertainment equipment
    driver2="WTS", # Home entertainment equipment and HH appliances
    d2_vnumber="v52367701"),
  ODRC=list(name="Other major durables for recreation and culture [C192]",
    betterName="Other major durables for recreation and culture",
    vnumber="v61989051",
    ChaVolnumber="v61988991",
    ConVolnumber1="v62700755", # Major dur for outdoor recr
    ConVolnumber2="v62700756", # Musical instr and major dur for indoor recr
    deflator="v41691179",
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v52367131", # Sporting gds, hobby, book and music stores
    driver2="CPI",
    d2_vnumber="v41691180"), # Purchase of recr vehicles and outboard motors
  OREG=list(name="Other recreational items and equipment, garden products and pets [C193]",
    betterName="Other recreational items and equipment, garden products and pets",
    vnumber="v61989052",
    ChaVolnumber="v61988992",
    ConVolnumber1="v62700757",
    ConVolnumber2="v62700758",
    ConVolnumber3="v62700759",
    ConVolnumber4="v62700760",
    ConVolnumber5="v62700761",
    deflator="v41691179",
    type="Chow-Lin",
    equation="NA",
    driver="RTS",
    d_vnumber="v52367131",
    driver2="NA",
    d2_vnumber="NA"),
  RCS=list(name="Recreational and cultural services [C194]",
    betterName="Recreational and cultural services",
    vnumber="v61989053",
    ChaVolnumber="v61988993",
    ConVolnumber1="v62700762",
    ConVolnumber2="v62700763",
    ConVolnumber3="v62700764",
    ConVolnumber4="v62700765",
    ConVolnumber5="v62700766",
    ConVolnumber6="v62700767",
    deflator="v41691193",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691193",
    driver2="GDP",
    d2_vnumber="v65201463"),
  NBS=list(name="Newspapers, books and stationery [C195]",
    betterName="Newspapers, books and stationery",
    vnumber="v61989054",
    ChaVolnumber="v61988994",
    ConVolnumber1="v62700768",
    ConVolnumber2="v62700769",
    ConVolnumber3="v62700770",
    deflator="v41691202",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691202",
    driver2="GDP",
    d2_vnumber="v65201399"),
  EDU=list(name="Education [C21]",
    betterName="Education",
    vnumber="v61989055",
    ChaVolnumber="v61988995",
    ConVolnumber="v62700772",
    deflator="v41691198",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691198",
    driver2="GDP",
    d2_vnumber="v65201452"),
  FBAS=list(name="Food, beverage and accommodation services [C22]",
    betterName="Food, beverage and accommodation services",
    vnumber="v61989057",
    ChaVolnumber="v61988997",
    ConVolnumber="v62700776",
    AdjEntry="v62700780",
    deflator="NA",
    type="identity",
    equation="FBS+AS",
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  FBS=list(name="Food and beverage services [C221]",
    betterName="Food and beverage services",
    vnumber="v61989058",
    ChaVolnumber="v61988998",
    ConVolnumber1="v62700777",
    ConVolnumber2="v62700778",
    deflator="v41691046",
    type="Chow-Lin",
    equation="NA",
    driver="FSDP",
    d_vnumber="v42170701",
    driver2="NA",
    d2_vnumber="NA"),
  AS=list(name="Accommodation services [C222]",
    betterName="Accommodation services",
    vnumber="v61989059",
    ChaVolnumber="v61988999",
    ConVolnumber="v62700779",
    deflator="v41691191",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="v41691191",
    driver2="GDP",
    d2_vnumber="v65201469"),
  FIN=list(name="Insurance and financial services [C23]",
    betterName="Insurance and financial services",
    vnumber="v61989060",
    ChaVolnumber="v61989000",
    ConVolnumber="v62700781",
    deflator="v41693229",
    type="Chow-Lin",
    equation="NA",
    driver="CPI",
    d_vnumber="41693229", # starts December 2002
    driver2="GDP",
    d2_vnumber="v65201407"), # GDP for finance and insurance
  MISC=list(name="Miscellaneous goods and services [C24]",
    betterName="Miscellaneous goods and services",
    vnumber="v61989064",
    ChaVolnumber="v61989004",
    ConVolnumber="v62700793",
    deflator="v41690973", # all-items CPI
    type="Chow-Lin",
    equation="NA",
    driver="RTS", # Health and personal care stores
    d_vnumber="v52367117",
    driver2="CPI",
    d2_vnumber="v41691163"), # CPI for personal care
  NEA=list(name="Net expenditure abroad [C25]",
    betterName="Net expenditure abroad",
    ChaVolnumber="v61989009",
    vnumber="v61989069",
    ConVolnumber="v62700805",
    AdjEntry="v62700808",
    deflator="NA",
    type="identity",
    equation="TECA+TENR", # TENR is negative
    driver="NA",
    d_vnumber="NA",
    driver2="NA",
    d2_vnumber="NA"),
  TECA=list(name="Expenditure by Canadians abroad [C251]",
    betterName="Expenditure by Canadians abroad",
    vnumber="v61989070",
    ChaVolnumber="v61989010",
    ConVolnumber="v62700806",
    deflator="v41690973", # all-items CPI
    type="Chow-Lin",
    equation="NA",
    driver="ITERC",  #24-10-0054-01
    d_vnumber="v1296948835",
    driver2="NA",
    d2_vnumber="NA"),
  TENR=list(name="Expenditure by non-residents in Canada [C252]",
    betterName="Expenditure by non-residents in Canada",
    vnumber="v61989071",
    ChaVolnumber="v61989011",
    ConVolnumber="v62700807",
    deflator="v41691191", # accommodations services CPI
    type="Chow-Lin",
    equation="NA",
    driver="ITERC",  #24-10-0054-01
    d_vnumber="v1296948811",
    driver2="NA",
    d2_vnumber="NA")
)
(N <- length(HHCE))
(HHCEnames <- names(HHCE))
#-------------------------------------------------------------------------------
# List of data frames to be built
# Qdata      - All the quarterly SAAR data at current prices for consumption
# Adata      - All of the C-series without indicators, that are not identities,
#              interpolated to monthly (using Denton-Cholette) for the same time 
#              range as for Qdata
# Fdata      - For ARIMA cases; same as Adata except that forecasts are added on end
# FQdata     - Fdata converted to quarterly
# CLdata     - All of the monthly indicators, associated with each Chow-Lin 
#              case, extended to lastmonF by ARIMA is necessary
# CQdata     - Cdata converted to quarterly
# CLE1data   - Data for first of two indicators (drivers)
# CLE1data   - Data for second of two indicators (drivers)
# CEdata     - Interpolated and extrapolated monthly data for two-driver cases
# Cdata      - All of the monthly series that were interpolated and extrapolated
#              using one or two related indicators and the Chow-Lin method
# Idata      - All of the identity cases calculated by applying the equations
#              to the data in Fdata and Cdata
# Edata      - The final monthly results at current prices for all series,  
#              interpolated and extrapolated
# EQdata     - Edata converted to quarterly
# Vdata      - The quarterly volume data (chain)
# Pdata      - The quarterly price data (100*Qdata/Vdata)
# Defdata    - The monthly driver data for the price indexes
# CPdata     - The monthly interpolated and extrapolated price indexes
# Kdata      - The monthly interpolated and extrapolated consumption data 
#              at constant 2012 prices
# IKdata     - The monthly identity consumption data at constant 2012 prices
# FinalKdata - The combination of Kdata and IKdata
# IPdata    - The full set of monthly price data 
#-------------------------------------------------------------------------------
# Retrieve all the quarterly SAAR data in a df called Qdata
# covering the time period firstqtr to lastqtr
# Qdata is quarterly, current prices, SAAR
# Qdata names are in the same order as the HHCE list
Qdata <- data.frame(Date=seq.Date(firstqtr,lastqtr,by="quarter"))
for (i in 2:(N+1)) {
  vec <- HHCE[[i-1]]$vnumber # vnumbers for the quarterly data
  tmp <- get_cansim_vector(vec,firstqtrC)
  tmp <- filter(tmp,Date<=lastqtr)
  Qdata[,i] <- tmp$VALUE
  names(Qdata)[i] <- names(HHCE)[i-1]
}
#-------------------------------------------------------------------------------
# Identify the ARIMA cases; then use Denton-Cholette to temporally disaggregate
# all of them; finally use ARIMA to forecast them
#MODELS_ARIMA <- list()
#Acases <- list()
#for (i in 1:N) {
#  if (HHCE[[i]]$driver=="ARIMA") Acases <- c(Acases,HHCE[i])
#}
#Adata <- data.frame(Date=seq.Date(firstmon,lastmon,by="month"))
#for (i in 2:(length(Acases)+1)) {
#  nam <- names(Acases)[i-1]
#  Adata[i] <- MakeMonthly00(Qdata[,which(colnames(Qdata)==nam)],nam,0,F)
#  names(Adata)[i] <- nam
#}
#Fdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
#for (i in 2:(length(Acases)+1)) {
#  nam <- names(Acases)[i-1]
#  model <- auto.arima(Adata[i])
#  fc <- forecast(model)
#  #fc <- forecast(auto.arima(Adata[i]),mondf(lastmon,lastmonF))
#  MODELS_ARIMA[[i-1]] <- summary(model)
#  names(MODELS_ARIMA)[i-1] <- nam
#  Fdata[,i] <- c(Adata[,i],fc$mean[1])
#  names(Fdata)[i] <- nam
#}
#-------------------------------------------------------------------------------
# Find all the Chow-Lin cases having only one driver; then collect the 
# Chow-Lin single driver vnumbers; then check availability dates;
# finally forecast the drivers to lastmonF with ARIMA if necessary
CLcases <- list()
for (i in 1:N) {
  if (HHCE[[i]]$type=="Chow-Lin" & HHCE[[i]]$d2_vnumber=="NA") {
    CLcases <- c(CLcases,HHCE[i])  # the Chow-Lin lists (from inside HHCE)
  }
}
CLdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 1:length(CLcases)) {
    nam <- names(CLcases)[i]
    vec <- HHCE[[nam]]$d_vnumber 
    driv <- HHCE[[nam]]$driver 
    tmp1 <- get_cansim_vector(vec,firstmonC)
    tmpval <- tmp1$VALUE
    date1 <- tmp1$Date[1]
    date2 <- tmp1$Date[nrow(tmp1)]
    if (driv=="CPI" & nam=="CLO") { # Special case: clothing CPI very seasonal
      tmp1$VALUE <- DoSeasAdj(tmp1$VALUE,firstmonC)
      tmpval <- tmp1$VALUE
    }
    if (date1>firstmon) { # backcast the series to firstmon via ARIMA
      if(HHCE[[nam]]$name=="Cannabis [C123]") { # Special case: cannabis
        tmpval <- c(rep(0,mondf(firstmon,date1)),tmp1$VALUE)
        tmp1 <- data.frame(Date=seq.Date(firstmon,date2,"month"),
          VALUE=tmpval)
      } else {
        tmpval <- backcast(tmp1$VALUE,mondf(firstmon,date1),firstmon)
      }
    }
    if (date2>lastmonF) {
      tmp1 <- filter(tmp1,Date<=lastmonF)
      tmpval <- tmp1$VALUE
    }
    if (date2<lastmonF) {
      print(paste0("The last available date for ",nam," is ",date2,
        " so the indicator series will be extended with ARIMA to ",lastmonF))
      tmpval <- ArimaForecast(tmp1$VALUE,mondf(date2,lastmonF))
    }
    CLdata[,i+1] <- tmpval
    names(CLdata)[i+1] <- nam
}
#-------------------------------------------------------------------------------
# Use the Chow-Lin procedure in each of the cases where it applies,  
# to interpolate and extrapolate the cases with only one driver
MODELS_ChowLin <- list()
Cdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 2:(length(CLcases)+1)) {
  nam <- names(CLcases)[i-1]
  if (nam=="CANN") {
    Cdata[i] <- MakeMonthly1CANN(0,F)
  } else {
    NewList <- MakeMonthly1(MODELS_ChowLin,nam,0,F)
    Cdata[i] <- NewList[[2]]
    MODELS_ChowLin <- NewList[[1]]
  }
  names(Cdata)[i] <- nam
}
#-------------------------------------------------------------------------------
# Find all the Chow-Lin cases with two drivers; collect the Chow-Lin two 
# driver d2_vnumbers; check availability dates; finally forecast 
# to lastmonF with ARIMA if necessary
CLEcases <- list()
for (i in 1:N) {
  if (HHCE[[i]]$type=="Chow-Lin" & HHCE[[i]]$driver2!="NA") {
    CLEcases <- c(CLEcases,HHCE[i])  # the Chow-Lin lists (from inside HHCE)
  }
}
CLE1data <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month")) # extra drivers
CLE2data <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month")) # extra drivers
for (i in 2:(length(CLEcases)+1)) {
  # first get indicator 1
  nam <- names(CLEcases)[i-1]
  print(paste0("Series is ",nam))
  vec <- HHCE[[nam]]$d_vnumber 
  driv <- HHCE[[nam]]$driver 
  tmp1 <- get_cansim_vector(vec,firstmonC)
  tmpval <- tmp1$VALUE
  tmp2 <- tmp1
  date1 <- tmp1$Date[1]
  date2 <- tmp1$Date[nrow(tmp1)]
  if (driv=="CPI" & nam=="CLO") { 
    tmp1$VALUE <- DoSeasAdj(tmp1$VALUE,firstmonC)
    tmpval <- tmp1$VALUE
    tmp1 <- data.frame(Date=seq.Date(firstmon,lastmonF,"month"),
      VALUE=tmpval)
  }
  if (date1>firstmon) { # backcast the series to firstmon via ARIMA
    tmpval <- backcast(tmp1$VALUE,mondf(firstmon,date1),firstmon) # doesn't work for CANN
  }
  if (date2>lastmonF) {
    tmp1 <- filter(tmp1,Date<=lastmonF)
    tmpval <- tmp1$VALUE
  }
  if (date2<lastmonF) {
    print(paste0("The last available date for the first indicator for ",
      nam," is ",date2," so the indicator series will be ",
      "extended with ARIMA to ",lastmonF))
    tmpval <- ArimaForecast(tmp1$VALUE,mondf(date2,lastmonF))
  }
  CLE1data[,i] <- tmpval
  colnames(CLE1data)[i] <- nam
  # now get indicator 2
  vec2 <- HHCE[[nam]]$d2_vnumber 
  driv2 <- HHCE[[nam]]$driver2
  tmp1 <- get_cansim_vector(vec2,firstmonC)
  tmpval <- tmp1$VALUE
  tmp2 <- tmp1
  date1 <- tmp1$Date[1]
  date2 <- tmp1$Date[nrow(tmp1)]
  if (driv2=="CPI" & nam=="CLO") { 
    tmp1$VALUE <- DoSeasAdj(tmp1$VALUE,firstmonC)
    tmpval <- tmp1$VALUE
    tmp1 <- data.frame(Date=seq.Date(firstmon,lastmonF,"month"),
      VALUE=tmpval)
  }
  if (date1>firstmon) { # backcast the series to firstmon via ARIMA
    tmpval <- backcast(tmp1$VALUE,mondf(firstmon,date1),firstmon) # doesn't work for CANN
  }
  if (date2>lastmonF) {
    tmp1 <- filter(tmp1,Date<=lastmonF)
    tmpval <- tmp1$VALUE
  }
  if (date2<lastmonF) {
    print(paste0("The last available date for the second indicator for ",
      nam," is ",date2," so the indicator series will be ",
      "extended with ARIMA to ",lastmonF))
    tmpval <- ArimaForecast(tmp1$VALUE,mondf(date2,lastmonF))
  }
  CLE2data[,i] <- tmpval
  colnames(CLE2data)[i] <- nam
}
#-------------------------------------------------------------------------------
# Use the Chow-Lin procedure in each of the cases with two indicators. 
# The indicators are in CLEdata (which all extend to lastmonF)
# CEdata is monthly, current prices, SAAR
CEdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 1:length(CLEcases)) {
  nam <- names(CLEcases)[i]
  #CEdata[i+1] <- MakeMonthly2(nam,0,F)
  NewList <- MakeMonthly2(MODELS_ChowLin,nam,0,F)
  CEdata[i+1] <- NewList[[2]]
  MODELS_ChowLin <- NewList[[1]]
  colnames(CEdata)[i+1] <- nam
}
# combine 1-indicator with 2-indicators case
Cdata <- cbind(Cdata,select(CEdata,-Date)) 
#-------------------------------------------------------------------------------
# Summarize regression results
NR <- length(MODELS_ChowLin)
REGRES <- data.frame(Name=character(NR),RSQ=numeric(NR),
  ARSQ=numeric(NR),SIGMA=numeric(NR),RHO=numeric(NR),
  RSS=numeric(NR),MEAN=numeric(NR), # RMS=numeric(NR),
  SEE=numeric(NR),NUMVAR=numeric(NR),DRIVER1=character(NR),
  DRIVER2=character(NR))
for (i in 1:length(MODELS_ChowLin)) {
  df <- as.data.frame(MODELS_ChowLin[[i]]$actual)
  MEAN <- mean(df$VALUE)
  REGRES$Name[i] <- names(MODELS_ChowLin)[i]
  REGRES$RSQ[i] <- round(MODELS_ChowLin[[i]]$r.squared,3)
  REGRES$ARSQ[i] <- round(MODELS_ChowLin[[i]]$adj.r.squared,3)
  REGRES$SIGMA[i] <- round((MODELS_ChowLin[[i]]$s_2)^0.5,3)
  REGRES$RHO[i] <- round(MODELS_ChowLin[[i]]$rho,3)
  REGRES$RSS[i] <- round(MODELS_ChowLin[[i]]$rss,0)
  #REGRES$RMS[i] <- round((MODELS_ChowLin[[i]]$rss/
  #    MODELS_ChowLin[[i]]$n_l)^0.5,0)
  REGRES$MEAN[i] <- MEAN
  REGRES$SEE[i] <- 100*((MODELS_ChowLin[[i]]$s_2)^0.5)/MEAN
  REGRES$NUMVAR[i] <- nrow(MODELS_ChowLin[[i]]$coefficients)
  if (REGRES$NUMVAR[i]==2) {
    REGRES$DRIVER1[i] <- HHCE[[REGRES$Name[i]]]$driver
    REGRES$DRIVER2[i] <- NA
  } else {
    REGRES$DRIVER1[i] <- HHCE[[REGRES$Name[i]]]$driver
    REGRES$DRIVER2[i] <- HHCE[[REGRES$Name[i]]]$driver2
  }
}
REGRESpub <- REGRES
for (i in 1:nrow(REGRES)) {
  REGRESpub$Name[i] <- HHCE[[REGRES$Name[i]]]$betterName  
}
REGRESpub <- select(REGRESpub,-RSQ,-RSS,-NUMVAR)
REGRESpub$MEAN <- round(REGRESpub$MEAN,0)
REGRESpub$SIGMA <- round(REGRESpub$SIGMA,0)
REGRESpub$SEE <- round(REGRESpub$SEE,3)
View(REGRESpub)
saveRDS(REGRES,"REGRES.rds")
saveRDS(REGRESpub,"REGRESpub.rds")
#-------------------------------------------------------------------------------
# Identify the identity cases; then calculate the identity cases using 
# the identity equations
IdentityCases <- list()
Idents <- vector()
j <- 0
for (i in 2:N) {
  if (HHCE[[i]]$type=="identity") {
    j <- j+1
    IdentityCases <- c(IdentityCases,HHCE[i])
    Idents[j] <- HHCE[[i]]$equation
  }
}
j <- j+1
# Do the big identity last because it depends on the others
IdentityCases <- c(IdentityCases,HHCE[1])
Idents[j] <- HHCE[[1]]$equation
# Edata will be monthly, current prices, SAAR
Edata <- Cdata  # cbind(Cdata[,2:ncol(Cdata)]) # Fdata removed
Idata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 2:(length(IdentityCases)+1)) {
  nam <- names(IdentityCases)[i-1]
  Idata[i] <- round(eval(parse(text=Idents[i-1]),Edata),0)
  Edata <- cbind(Edata,Idata[i])
  colnames(Edata)[ncol(Edata)] <- names(IdentityCases)[i-1]
}
Edata <- select(Edata,all_of(c("Date",HHCEnames))) 
# Save C$ results for use in HHI table
saveRDS(Edata,paste0("/Users/philipsmith/Documents/R/",
  "Households/HH_consumption/Consumption_monthly/",
  "HHCE_results.rds")) 
# Test by converting back to quarterly and comparing
EQdata <- M_to_Q(Edata)
Diffs <- data.frame(Date=seq.Date(firstqtr,lastqtr,"quarter"))
for (i in 1:nrow(Qdata)) {
  for (j in 2:ncol(Qdata)) {
    Diffs[i,j] <- Qdata[i,j]-EQdata[i,j]
  }
}
View(Diffs)
cat(paste0("\nThere are ",N," time series being temporally\n",
    "disaggregated from quarters to months.\n\nOf these, ",
    ncol(CLdata)-1," are ",
    "interpolated and extrapolated\nusing one related indicator ",
    "via the Chow-Lin method\nand ",ncol(CEdata)-1,
    " more are interpolated and extrapolated ",
    "using\ntwo related indicators. \n\nFinally, ",
    length(IdentityCases)," of the ",N," quarterly series ",
    "are interpolated\n",
    "and extrapolated by applying identity equations,\n",
    "for totals and sub-totals."),sep="\n")
#-------------------------------------------------------------------------------
printResultsCdollar <- FALSE
if (printResultsCdollar) {
#-------------------------------------------------------------------------------
# Make a table with the levels results
tbl0 <- filter(Edata,Date>=as.Date("2021-12-01"))
tbl1 <- as.data.frame(t(tbl0))
tbl1 <- tbl1[2:nrow(tbl1),]
nm <- character()
for (i in 1:nrow(tbl1)) {
  nm[i] <- HHCE[[HHCEnames[i]]]$betterName
}
tbl1$names <- nm
tbl1 <- select(tbl1,names,everything())
tbl1 <- mutate(tbl1,across(2:ncol(tbl1),as.numeric))

colls <- c("V1","V2","V3","V4","V5","V6","V7","V8") # Dec 2021 to July 2022
tabName <- "HHCE_table_July2022_Sep0522.png"
LastMonth <- "July"

gt_tbl1 <- gt(data=tbl1)
gt_tbl1 <- gt_tbl1 %>% 
  tab_options(table.font.size=10,container.width = 950) %>%
  tab_header(
    title=md(html(paste0("**Household final consumption expenditure<br>December 2021 to ",
      LastMonth," 2022<br>Millions of dollars**")))
    #subtitle=md(html("1-month percentage change"))
  ) %>% 
  tab_source_note(
    source_note=md(html("@PhilSmith26"))
  ) %>% 
  cols_align(
    align=c("left"),
    columns=c(`names`)
  ) %>%
  fmt_number(
    columns=all_of(colls),
    decimals=0,
    use_seps=TRUE
  ) %>%
  cols_label(
    `names`="",
    `V1`=md("**Dec<br>2021**"),
    `V2`=md("**Jan<br>2022**"),
    `V3`=md("**Feb<br>2022**"),
    `V4`=md("**Mar<br>2022**"),
    `V5`=md("**Apr<br>2022**"),
    `V6`=md("**May<br>2022**"),
    `V7`=md("**Jun<br>2022**"),
    `V8`=md("**Jul<br>2022**")
  ) %>%
  data_color(
    columns=all_of(colls),
    colors=scales::col_numeric(
      palette=c(
        "#E3F2FD"),
      domain=c(-10000000.0,1000000000.0),
    )
  ) %>%
  tab_style(style = cell_text(indent=pct(3.5)),
    locations = cells_body(
      columns = 1,
      rows = c(2,3,7,10,16,22,23,27,28,34,35,38,39,40))
  ) %>%
  tab_style(style = cell_text(indent=pct(6.5)),
    locations = cells_body(
      columns = 1,
      rows = c(4,5,6,8,9,11,12,13,14,15,17,18,19,20,
        21,24,25,26,29,30,31,32,33,36,37,41,42))
  ) %>%
  tab_style( # column label style
    style = list(
      cell_fill(color = "#E3F2FD"), # "bisque3" or gray66" or "darkslategray2"
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=c(names,all_of(colls)))
  ) %>%
  tab_footnote(
    footnote = paste0("Estimates derived using time disaggregation methods ",
       "credited to F.T. Denton, P.A. Cholette, G.C. Chow and A.-L. Lin, ",
       "autoregressive moving-average models and data from Statistics ",
       "Canada tables 36-10-0107-01, 20-10-0008-01, 18-10-0004-01, ",
       "36-10-0434-01, 21-10-0019-01 and 24-10-0054-01. ",Sys.time()),
    locations = cells_title()
  )
gt_tbl1
setwd(paste0("/Users/philipsmith/Documents/R/Households/HH_consumption/Consumption_monthly"))
gtsave(gt_tbl1,tabName)
   
#-------------------------------------------------------------------------------
# Make a table with the percentage change results
TransposeTbl <- function(df) {
  df1 <- as.data.frame(t(df))
  df1 <- df1[2:nrow(df1),]
  nm <- character()
  for (i in 1:nrow(df1)) { nm[i] <- HHCE[[HHCEnames[i]]]$betterName }
  df1$names <- nm
  df1 <- select(df1,names,everything())
  df1 <- mutate(df1,across(2:ncol(df1),as.numeric))
}   
tblQl <- TransposeTbl(EQdata)
tblQp <- mutate(EQdata,across(2:ncol(Edata),function (x) round(100*(x/lag(x)-1),1)))
tblQp <- TransposeTbl(tblQp)
tbl0 <- mutate(Edata,across(2:ncol(Edata),function (x) round(100*(x/lag(x)-1),1)))
tbl0 <- filter(tbl0,Date>=as.Date("2021-12-01"))
tbl1 <- TransposeTbl(tbl0)
tbl1$Q2l <- tblQl[,ncol(tblQl)]
tbl1$Q1p <- tblQp[,(ncol(tblQp)-1)]
tbl1$Q2p <- tblQp[,ncol(tblQp)]
tbl1 <- select(tbl1,names,Q2l,Q1p,Q2p,everything())

colls <- c("V1","V2","V3","V4","V5","V6","V7","V8") # Dec 2021 to July 2022
tabName <- "HHCE_table_July2022_Sep0522.png"
LastMonth <- "July"

gt_tbl1 <- gt(data=tbl1)
gt_tbl1 <- gt_tbl1 %>% 
  tab_options(table.font.size=12,container.width = 950) %>%
  tab_header(
    title=md(html(paste0("**Household final consumption expenditure at current prices<br>December 2021 to ",
      LastMonth," 2022<br>Monthly percentage change**")))
    #subtitle=md(html("1-month percentage change"))
  ) %>% 
  tab_source_note(
    source_note=md(html("@PhilSmith26"))
  ) %>% 
  cols_align(
    align=c("left"),
    columns=c(`names`)
  ) %>%
  fmt_number(
    columns=Q2l,
    decimals=0,
    use_seps=TRUE
  ) %>%
  fmt_number(
    columns=c(Q1p,Q2p,all_of(colls)),
    decimals=1,
    use_seps=TRUE
  ) %>%
  cols_label(
    `names`="",
    `Q2l`=md("**2022 Q2<br>($ million)**"),
    `Q1p`=md("**2022 Q1<br>(% change)**"),
    `Q2p`=md("**2022 Q2<br>(% change)**"),
    `V1`=md("**Dec<br>2021**"),
    `V2`=md("**Jan<br>2022**"),
    `V3`=md("**Feb<br>2022**"),
    `V4`=md("**Mar<br>2022**"),
    `V5`=md("**Apr<br>2022**"),
    `V6`=md("**May<br>2022**"),
    `V7`=md("**Jun<br>2022**"),
    `V8`=md("**Jul<br>2022**")
  ) %>%
  data_color(
    columns=c(Q2l,Q1p,Q2p,all_of(colls)),
    colors=scales::col_numeric(
      palette=c(
        "#E3F2FD"),
      domain=c(-10000000.0,1000000000.0),
    )
  ) %>%
  tab_style(style = cell_text(indent=pct(3.5)),
    locations = cells_body(
      columns = 1,
      rows = c(3,4,5,7,8,10,11,12,13,14,16,17,18,19,
    20,23,24,25,28,29,30,31,32,35,36,40,41))
  ) %>%
  tab_style( # column label style
    style = list(
      cell_fill(color = "#E3F2FD"), # "bisque3" or gray66" or "darkslategray2"
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=c(names,Q2l,Q1p,Q2p,all_of(colls)))
  ) %>%
  tab_footnote(
    footnote = paste0("Estimates derived using time disaggregation methods ",
       "credited to F.T. Denton, P.A. Cholette, G.C. Chow and A.-L. Lin, ",
       "plus a few autoregressive integrated moving-average (ARIMA) models. ",
       "Done with extensive use of the 'tempdisagg' and 'forecast' R ",
       "packages. The monthly estimates are produced by interpolating and ",
       "extrapolating the quarterly national accounts estimates up to 2022 ",
       "Q2, using related series from the retail trade survey, the ",
       "consumer price index survey, the monthly GDP by industry ",
       "estimates, the food services and drinking places survey ",
       "and the international travellers survey of ",
       "Statistics Canada. The data are all ",
       "seasonally adjusted. See tables 36-10-0107-01, 20-10-0008-01, ",
       "18-10-0004-01, 36-10-0434-01, 21-10-0019-01 and 24-10-0054-01. ",Sys.time()),
    locations = cells_title()
  )
gt_tbl1
setwd(paste0("/Users/philipsmith/Documents/R/Households/HH_consumption/Consumption_monthly/"))
gtsave(gt_tbl1,"HHCE_table_July2022_PC01.png")
      
HHCEP01 <- mutate(Edata,across(2:ncol(Edata),function (x) round(100*(x/lag(x)-1),1)))
# Make monthly/quarterly series
qtrser <- vector()
for (i in 1:nrow(Edata)) {
  j <- floor((i-1)/3)+1
  qtrser[i] <- EQdata[j,ncol(EQdata)]
}
qtrser <- unlist(qtrser)
df <- data.frame(Date=as.Date(Edata$Date),HHFC=Edata$HHFC,HHFCPC01=HHCEP01$HHFC,
  qtrser=qtrser)
sub <- 1650000
div <- 60000
c1 <- ggplot(filter(df,Date>=as.Date("2020-09-01")))+
  geom_line(aes(x=Date,y=(HHFC-sub)/div),colour="blue",size=1.5)+
  geom_point(aes(x=Date,y=(HHFC-sub)/div),colour="blue",size=2.5)+
  #geom_line(aes(x=Date,y=(qtrser-sub)/div),colour="forestgreen",
  #  size=2.5,linetype="dashed")+
  geom_col(aes(x=Date,y=HHFCPC01),fill="red")+
  #geom_line(aes(x=Date,y=MA),colour="black",size=1.5)+
  labs(title=paste0("Household final consumption expenditure, September 2020 to ",LastMonth," 2022"),
    subtitle=paste0("The red bars are monthly percentage changes (right axis)\n",
      #"The black line is a 13-month centred moving average (right axis)\n",
      "The blue line is the household final consumption expenditure level in millions of dollars (left axis)"),
    caption=paste0("Own calculations using Statistics Canada data. ",Sys.time()," @PhilSmith26"),x="") +
  scale_x_continuous(breaks=c(seq.Date(as.Date("2020-01-01"),
    as.Date("2022-07-01"),by="3 months")),labels=
    c("Jan\n2020","Apr\n2020","Jul\n2020","Oct\n2020",
    "Jan\n2021","Apr\n2021","Jul\n2021","Oct\n2021","Jan\n2022","Apr\n2022",
    "Jul\n2022"))+
    scale_y_continuous(
      position="right",breaks=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5),
      name="Monthly % change\n",
      sec.axis=sec_axis(~ . * div + sub,name="Millions of dollars",
        breaks=c(1200000,1250000,1300000,1350000,1400000,1450000)))+
  theme(plot.title = element_text(size=16,face="bold")) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  theme(panel.border = element_rect(fill=NA,colour="black"))+
  theme(panel.grid.minor=element_line(colour="lightgrey",size=0.5)) +
  theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
  theme(axis.text.x = element_text(angle=0,hjust=1,size=9)) +
  theme(axis.text.y = element_text(size=12))
c1
ggsave("MHHCE_dual_chart_volSep2022.png",c1,height=8.5,width=11,dpi=300)

#-------------------------------------------------------------------------------
} # end of printResultsCdollar
#-------------------------------------------------------------------------------

#===============================================================================
# Now calculate the quarterly implicit price indexes and 
# interpolate and extrapolate them (in several steps)
# NOTE: The chain volume numbers will not "add up" correctly since
# they are chain indexes, so the estimates at 2012 constant prices are
# the target instead.
#-------------------------------------------------------------------------------
# Get the actual quarterly deflators by first getting the volume data
# at constant 2012 prices and comparing it to the value data in 2012
# to make sure the volume categories line up with the value ones
# NOTE: the input C$ quarterly data are SAAR but the input K$ data are SAQR
Vdata <- data.frame(Date=seq.Date(firstqtr,lastqtr,by="quarter"))
(Vnames <- names(HHCE)) # 42 consumption time series from 36-10-0107-01
for (i in 1:N) {
  if (exists("ConVolnumber",HHCE[[i]])) { # Just one vnumber
    vec <- HHCE[[i]]$ConVolnumber # get that one vnumber
    dfTmp <- get_cansim_vector(vec,firstqtrC)
    dfTmp <- filter(dfTmp,Date<=lastqtr)
    Vdata[,i+1] <- dfTmp$VALUE*4 # convert to annual rates
    names(Vdata)[i+1] <- names(HHCE)[i]
  } else { # If this is an aggregate of 2+ vnumbers
    # Count the number of components (max 10) and add them together
    tmp <- data.frame(Date=seq.Date(firstqtr,lastqtr,by="quarter"))
    for (j in 1:10) {
      if (exists(paste0("ConVolnumber",j),where=HHCE[[i]])) { 
        if (j==1) {
          vec <- HHCE[[i]]$ConVolnumber1
        } else if (j==2) {
          vec <- HHCE[[i]]$ConVolnumber2
        } else if (j==3) {
          vec <- HHCE[[i]]$ConVolnumber3
        } else if (j==4) {
          vec <- HHCE[[i]]$ConVolnumber4
        } else if (j==5) {
          vec <- HHCE[[i]]$ConVolnumber5
        } else if (j==6) {
          vec <- HHCE[[i]]$ConVolnumber6
        } else if (j==7) {
          vec <- HHCE[[i]]$ConVolnumber7
        } else if (j==8) {
          vec <- HHCE[[i]]$ConVolnumber8
        } else if (j==9) {
          vec <- HHCE[[i]]$ConVolnumber9
        } else if (j==10) {
          vec <- HHCE[[i]]$ConVolnumber10
        }
        dfTmp <- get_cansim_vector(vec,firstqtrC)
        dfTmp <- filter(dfTmp,Date<=lastqtr)
        tmp[,j+1] <- dfTmp$VALUE
      }
    }
    tmp[is.na(tmp)] <- 0
    tmp <- select(tmp,-Date)
    Vdata[,i+1] <- rowSums(tmp)*4 # convert to annual rates
    names(Vdata)[i+1] <- names(HHCE)[i]
  }
}
# Make sure the columns are in the correct order
Vdata <- select(Vdata,"Date","HHFC","FB","ATC","AB","TOB","CANN","CF","CLO",
  "FOOT","HWEG","RENT","IMPR","MAIN","WAT","EGF","FEOD","FFCF","HTEX",
  "HAPP","TEHG","ODP","HEAL","TRAN","VEH","OTE","TS","COMM","RC",
  "AVPI","ODRC","OREG","RCS","NBS","EDU","FBAS","FBS","AS","FIN",
  "MISC","NEA","TECA","TENR")
Vnames <- names(Vdata)
# Calculate the sum of the four quarters in 2012 - should be same in C$ and K$ - both are SAAR
tmp <- filter(Vdata,Date>as.Date("2011-10-01") & Date<as.Date("2013-01-01"))
tmp <- tmp[2:ncol(tmp)] # remove Date column
SumVQ2012 <- colSums(tmp)
tmp <- filter(Qdata,Date>as.Date("2011-10-01") & Date<as.Date("2013-01-01"))
tmp <- tmp[2:ncol(tmp)] # remove Date column
SumCQ2012 <- colSums(tmp)
(Diff <- SumVQ2012-SumCQ2012)
# Calculate the quarterly price indexes
Pdata <- data.frame(Date=seq.Date(firstqtr,lastqtr,by="quarter"))
for (i in 1:nrow(Qdata)) {
  for (j in 2:ncol(Qdata)) {
    Pdata[i,j] <- 100*Qdata[i,j]/Vdata[i,j]
  }
}
(colnames(Pdata) <- Vnames)
Pnames <- names(HHCE) # 42 consumption time series from 36-10-0107-01
#-------------------------------------------------------------------------------
# Find all the deflation cases, which are the cases where 
# interpolation and extrapolation are done.
Defcases <- list()
Defnums <- vector()
for (i in 1:N) {
  if (HHCE[[i]]$deflator!="NA") {
    Defcases <- c(Defcases,HHCE[i])
    Defnums <- c(Defnums,i) 
  }
}
#-------------------------------------------------------------------------------
# Collect the consumption deflator indicators, check availability  
# dates and forecast to lastmonF with ARIMA if necessary
# The Defdata data frame is entirely CPIs
Defdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 1:length(Defcases)) {
    vec <- HHCE[[Defnums[i]]]$deflator 
    tmp1 <- get_cansim_vector(vec,firstmonC)
    if (names(Defcases)[i]=="CLO") { # Clothing CPI is very seasonal
      tmpval <- SEASADJ(tmp1$VALUE,year(firstmon))
      tmp1$VALUE <- tmpval
    } else {
      tmpval <- tmp1$VALUE
    }
    date1 <- tmp1$Date[1]
    date2 <- tmp1$Date[nrow(tmp1)]
    # Treat the cannabis case with a separate function
    # because it starts too late to backcast the gap
    if(HHCE[[Defnums[i]]]$name=="Cannabis [C123]") {
      tmpval <- tmp1$VALUE
      tmpval <- PAD1(tmpval,mondf(firstmon, date1),"b")
      tmp1 <- data.frame(Date=seq.Date(firstmon,date2,by="month"),
        VALUE=tmpval)
      date1 <- tmp1$Date[1]
      date2 <- tmp1$Date[nrow(tmp1)]
    }
    if (date1>firstmon) { # backcast the series to firstmon via ARIMA
      tmpval <- backcast(tmp1$VALUE,mondf(firstmon,date1),firstmon)
    }
    if (date2>lastmonF) {
      tmp1 <- filter(tmp1,Date<=lastmonF)
      tmpval <- tmp1$VALUE
    }
    if (date2<lastmonF) {
      print(paste0("The last available date for ",names(Defcases)[i]," is ",date2,
        " so the indicator series will be extended with ARIMA to ",lastmonF))
      tmpval <- ArimaForecast(tmp1$VALUE,mondf(date2,lastmonF))
    }
    Defdata[,i+1] <- tmpval
}
(colnames(Defdata) <- c("Date",names(Defcases)))
#-------------------------------------------------------------------------------
# Use the Chow-Lin procedure in each of the cases where it applies, with 
# the indicators in Defdata (which all extend to lastmonF)
# All price indexes have one driver only
CPdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 2:(length(Defcases)+1)) {
  nam <- names(Defcases)[i-1]
  if (nam=="CANN") { # Special case
    CPdata[,i] <- MakeMonthlyP1CANN(F)
  } else {
    CPdata[,i] <- MakeMonthlyP1(nam,F)
  }
}
(colnames(CPdata) <- c("Date",names(Defcases)))
(CPnames <- names(CPdata))
#-------------------------------------------------------------------------------
# Calculate the volume data using the interpolated and extrapolated
# results in CPdata combined with the nominal data stored in Edata
Kdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
tmp <- select(Edata,all_of(c("Date",CPnames)),everything())
for (i in 1:nrow(Kdata)) {
  for (j in 2:(length(Defcases)+1)) {
    Kdata[i,j] <- round(100*tmp[i,j]/CPdata[i,j],0)
  }
}
names(Kdata) <- CPnames
#-------------------------------------------------------------------------------
# Calculate the identity cases using the identity equations
IKdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 2:(length(IdentityCases)+1)) {
  nam <- names(IdentityCases)[i-1]
  IKdata[,i] <- round(eval(parse(text=Idents[i-1]),Kdata),0)
  if (exists("AdjEntry",HHCE[[nam]])) {
    vec <- HHCE[[nam]]$AdjEntry # get that vnumber
    dfTmp <- get_cansim_vector(vec,firstqtrC)
    dfTmp <- filter(dfTmp,Date<=lastqtr)
    MadjEntry <- MakeMonthly00(dfTmp$VALUE,nam,0,F)
    if(length(MadjEntry)<Nobs) {
      AddNobs <- mondf(lastmon,lastmonF)
      MadjEntry <- PAD1(MadjEntry,AddNobs,"f")
    }
    IKdata[,i] <- IKdata[,i]+MadjEntry
  }
  Kdata <- cbind(Kdata,IKdata[i])
  colnames(Kdata)[ncol(Kdata)] <- nam
  #readline(prompt="Press [enter] to continue")
}
(colnames(IKdata) <- c("Date",names(IdentityCases)))
Kdata <- select(Kdata,"Date","HHFC","FB","ATC","AB","TOB","CANN","CF","CLO",
  "FOOT","HWEG","RENT","IMPR","MAIN","WAT","EGF","FEOD","FFCF","HTEX",
  "HAPP","TEHG","ODP","HEAL","TRAN","VEH","OTE","TS","COMM","RC",
  "AVPI","ODRC","OREG","RCS","NBS","EDU","FBAS","FBS","AS","FIN",
  "MISC","NEA","TECA","TENR")

# Check back to STC quarterly table at quarterly rates
KdataQtrlyRates <- mutate(Kdata,across(2:ncol(Kdata),
  function(x) round(x/4,0)))
KdataQtrlyRates <- M_to_Q(KdataQtrlyRates)
# Now make complete set of monthly implicit price indexes
IPdata <- data.frame(Date=seq.Date(firstmon,lastmonF,by="month"))
for (i in 1:nrow(Edata)) {
  for (j in 2:ncol(Edata)) {
    IPdata[i,j] <- round(100*(Edata[i,j]/Kdata[i,j]),1)
  }
}
names(IPdata) <- names(Edata)
View(IPdata) # Monthly implicit price indexes
# In summary, the monthly interpolated and extrapolated consumption data
# are in these data frames:
# At current prices:       Edata
# At constant 2012 prices: Kdata
# Implicit prices:         IPdata
saveRDS(Edata,"HFCE_C_monthly_2010_stop22M7.rds")
saveRDS(Kdata,"HFCE_K_monthly_2010_stop22M7.rds")
saveRDS(IPdata,"HFCE_P_monthly_2010_stop22M7.rds")

#-------------------------------------------------------------------------------
# Make a table with the levels results: value, price, volume
tbl01 <- filter(Edata,Date>=as.Date("2022-04-01"))
tbl02 <- filter(IPdata,Date>=as.Date("2022-04-01"))
tbl03 <- filter(Kdata,Date>=as.Date("2022-04-01"))
tbl11 <- as.data.frame(t(tbl01))
tbl12 <- as.data.frame(t(tbl02))
tbl13 <- as.data.frame(t(tbl03))
df <- cbind(tbl11,tbl12,tbl13)
df <- df[2:nrow(df),]
nms <- rownames(df)
nm <- character()
for (i in 1:nrow(df)) {
  nm[i] <- HHCE[[nms[i]]]$betterName
}
df$nms <- nm
colnames(df) <- c("V2022M4","V2022M5","V2022M6","V2022M7",
  "P2022M4","P2022M5","P2022M6","P2022M7",
  "K2022M4","K2022M5","K2022M6","K2022M7","Category")
df <- select(df,Category,everything())
df <- mutate(df,across(2:ncol(df),as.numeric))

tabName <- "HHCE_table_July2022_Sep0822.png"
LastMonth <- "July"

gt_tbl1 <- gt(data=df)
gt_tbl1 <- tab_options(gt_tbl1,table.font.size=12,container.width = 1350)
gt_tbl1 <- tab_header(gt_tbl1,
  title=md(html(paste0("**Household final consumption expenditure<br>",
    "Seasonally adjusted at annual rates<br>April 2022 to ",LastMonth," 2022**"))))
gt_tbl1 <- tab_source_note(gt_tbl1,
    source_note=md(html("@PhilSmith26")))
gt_tbl1 <- cols_align(gt_tbl1,
    align=c("left"),
    columns=c(`Category`))
gt_tbl1 <- fmt_number(gt_tbl1,
    columns=all_of(c(2,3,4,5,10,11,12,13)),
    decimals=0,
    use_seps=TRUE)
gt_tbl1 <- fmt_number(gt_tbl1,
    columns=all_of(c(6,7,8,9)),
    decimals=1,
    use_seps=FALSE)
gt_tbl1 <- cols_label(gt_tbl1,
    `Category`="",
    `V2022M4`=md("**Apr<br>2022**"),
    `V2022M5`=md("**May<br>2022**"),
    `V2022M6`=md("**Jun<br>2022**"),
    `V2022M7`=md("**Jul<br>2022**"),
    `P2022M4`=md("**Apr<br>2022**"),
    `P2022M5`=md("**May<br>2022**"),
    `P2022M6`=md("**Jun<br>2022**"),
    `P2022M7`=md("**Jul<br>2022**"),
    `K2022M4`=md("**Apr<br>2022**"),
    `K2022M5`=md("**May<br>2022**"),
    `K2022M6`=md("**Jun<br>2022**"),
    `K2022M7`=md("**Jul<br>2022**"))
gt_tbl1 <- tab_spanner(gt_tbl1,label="Millions of dollars",
  columns=c(2,3,4,5),id="Cdollars")
gt_tbl1 <- tab_spanner(gt_tbl1,label="Price index, 2012=100",
  columns=c(6,7,8,9),id="Price")
gt_tbl1 <- tab_spanner(gt_tbl1,label="Millions of constant 2012 dollars",
  columns=c(10,11,12,13),id="Kdollars")
gt_tbl1 <- data_color(gt_tbl1,
    columns=all_of(2:13),
    colors=scales::col_numeric(
      palette=c(
        "#E3F2FD"),
      domain=c(-10000000.0,1000000000.0),
    ))
gt_tbl1 <- tab_style(gt_tbl1,style = cell_text(indent=pct(3.5)),
    locations = cells_body(
      columns = 1,
      rows = c(2,3,7,10,16,22,23,27,28,34,35,38,39,40)))
gt_tbl1 <- tab_style(gt_tbl1,style = cell_text(indent=pct(6.5)),
    locations = cells_body(
      columns = 1,
      rows = c(4,5,6,8,9,11,12,13,14,15,17,18,19,20,
        21,24,25,26,29,30,31,32,33,36,37,41,42)))
gt_tbl1 <- tab_style(gt_tbl1, # column label style
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=c(Category,all_of(2:13))))
gt_tbl1 <- tab_style(gt_tbl1,
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
      ),
    locations = cells_title()
  )
gt_tbl1 <- tab_style(gt_tbl1,
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
      ),
    locations = cells_column_spanners(spanners = everything())
  )
gt_tbl1 <- opt_row_striping(gt_tbl1, row_striping = TRUE)
gt_tbl1 <- opt_vertical_padding(gt_tbl1, scale = 0.25)
gt_tbl1 <- tab_style(gt_tbl1,style=cell_borders(sides="right",color="black",
  weight=px(1.5),style="solid"),locations=cells_body(columns=c(5,9),rows=1:42))
gt_tbl1 <- tab_footnote(gt_tbl1,
    footnote = paste0("Estimates derived using time disaggregation methods ",
       "credited to F.T. Denton, P.A. Cholette, G.C. Chow and A.-L. Lin, plus",
       "autoregressive moving-average models and data from Statistics ",
       "Canada tables 36-10-0107-01, 20-10-0008-01, 18-10-0004-01, ",
       "36-10-0434-01, 21-10-0019-01 and 24-10-0054-01. ",Sys.time()),
    locations = cells_title())
gt_tbl1
setwd(paste0("/Users/philipsmith/Documents/R/Households/HH_consumption/Consumption_monthly"))
gtsave(gt_tbl1,tabName)
   
#-------------------------------------------------------------------------------
# Make a table with the percentage change results: value, price, volume
tbl01 <- mutate(Edata,across(2:ncol(Edata),function(x) y <- round(100*(x/lag(x)-1),1)))
tbl02 <- mutate(IPdata,across(2:ncol(Edata),function(x) y <- round(100*(x/lag(x)-1),1)))
tbl03 <- mutate(Kdata,across(2:ncol(Edata),function(x) y <- round(100*(x/lag(x)-1),1)))
tbl01 <- filter(tbl01,Date>=as.Date("2022-04-01"))
tbl02 <- filter(tbl02,Date>=as.Date("2022-04-01"))
tbl03 <- filter(tbl03,Date>=as.Date("2022-04-01"))
tbl11 <- as.data.frame(t(tbl01))
tbl12 <- as.data.frame(t(tbl02))
tbl13 <- as.data.frame(t(tbl03))
df <- cbind(tbl11,tbl12,tbl13)
df <- df[2:nrow(df),]
nms <- rownames(df)
nm <- character()
for (i in 1:nrow(df)) {
  nm[i] <- HHCE[[nms[i]]]$betterName
}
df$nms <- nm
colnames(df) <- c("V2022M4","V2022M5","V2022M6","V2022M7",
  "P2022M4","P2022M5","P2022M6","P2022M7",
  "K2022M4","K2022M5","K2022M6","K2022M7","Category")
df <- select(df,Category,everything())
df <- mutate(df,across(2:ncol(df),as.numeric))
df[40,2:13] <- NA

tabName <- "HHCE_table_July2022_Sep0822PC01.png"
LastMonth <- "July"

gt_tbl1 <- gt(data=df)
gt_tbl1 <- tab_options(gt_tbl1,table.font.size=12,container.width = 1250)
gt_tbl1 <- tab_header(gt_tbl1,
  title=md(html(paste0("**Household final consumption expenditure<br>",
    "Seasonally adjusted<br>One-month percentage change<br>April 2022 to ",
    LastMonth," 2022**"))))
gt_tbl1 <- tab_source_note(gt_tbl1,
    source_note=md(html("@PhilSmith26")))
gt_tbl1 <- cols_align(gt_tbl1,
    align=c("left"),
    columns=c(`Category`))
gt_tbl1 <- sub_missing(gt_tbl1,columns=2:13,rows=40,missing_text="---")
gt_tbl1 <- fmt_number(gt_tbl1,
    columns=all_of(c(2,3,4,5,6,7,8,9,10,11,12,13)),
    decimals=1,
    use_seps=TRUE)
gt_tbl1 <- cols_label(gt_tbl1,
    `Category`="",
    `V2022M4`=md("**Apr<br>2022**"),
    `V2022M5`=md("**May<br>2022**"),
    `V2022M6`=md("**Jun<br>2022**"),
    `V2022M7`=md("**Jul<br>2022**"),
    `P2022M4`=md("**Apr<br>2022**"),
    `P2022M5`=md("**May<br>2022**"),
    `P2022M6`=md("**Jun<br>2022**"),
    `P2022M7`=md("**Jul<br>2022**"),
    `K2022M4`=md("**Apr<br>2022**"),
    `K2022M5`=md("**May<br>2022**"),
    `K2022M6`=md("**Jun<br>2022**"),
    `K2022M7`=md("**Jul<br>2022**"))
gt_tbl1 <- tab_spanner(gt_tbl1,label="Millions of dollars",
  columns=c(2,3,4,5),id="Cdollars")
gt_tbl1 <- tab_spanner(gt_tbl1,label="Price index, 2012=100",
  columns=c(6,7,8,9),id="Price")
gt_tbl1 <- tab_spanner(gt_tbl1,label="Millions of constant 2012 dollars",
  columns=c(10,11,12,13),id="Kdollars")
gt_tbl1 <- data_color(gt_tbl1,
    columns=all_of(2:13),
    colors=scales::col_numeric(
      palette=c(
        "#E3F2FD"),
      domain=c(-10000000.0,1000000000.0),
    ))
gt_tbl1 <- tab_style(gt_tbl1,style = cell_text(indent=pct(3.5)),
    locations = cells_body(
      columns = 1,
      rows = c(2,3,7,10,16,22,23,27,28,34,35,38,39,40)))
gt_tbl1 <- tab_style(gt_tbl1,style = cell_text(indent=pct(6.5)),
    locations = cells_body(
      columns = 1,
      rows = c(4,5,6,8,9,11,12,13,14,15,17,18,19,20,
        21,24,25,26,29,30,31,32,33,36,37,41,42)))
gt_tbl1 <- tab_style(gt_tbl1, # column label style
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns=c(Category,all_of(2:13))))
gt_tbl1 <- tab_style(gt_tbl1, # format for row with NAs
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows=40,
      columns=c(all_of(2:13))))
gt_tbl1 <- tab_style(gt_tbl1,
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
      ),
    locations = cells_title()
  )
gt_tbl1 <- tab_style(gt_tbl1,
    style = list(
      cell_fill(color = "#E3F2FD"),
      cell_text(weight = "bold")
      ),
    locations = cells_column_spanners(spanners = everything())
  )
gt_tbl1 <- opt_row_striping(gt_tbl1, row_striping = TRUE)
gt_tbl1 <- opt_vertical_padding(gt_tbl1, scale = 0.25)
gt_tbl1 <- tab_style(gt_tbl1,style=cell_borders(sides="right",color="black",
  weight=px(1.5),style="solid"),locations=cells_body(columns=c(5,9),rows=1:42))
gt_tbl1 <- tab_footnote(gt_tbl1,
    footnote = paste0("Estimates derived using time disaggregation methods ",
       "credited to F.T. Denton, E.B. Dagum, P.A. Cholette, G.C. Chow and A.-L. Lin, plus ",
       "autoregressive moving-average models and data from Statistics ",
       "Canada tables 36-10-0107-01, 20-10-0008-01, 18-10-0004-01, ",
       "36-10-0434-01, 21-10-0019-01 and 24-10-0054-01. Produced ",Sys.time()),
    locations = cells_title())
gt_tbl1
setwd(paste0("/Users/philipsmith/Documents/R/Households/HH_consumption/Consumption_monthly"))
gtsave(gt_tbl1,tabName)
      
HHCEP01 <- mutate(Kdata,across(2:ncol(Kdata),function (x) round(100*(x/lag(x)-1),1)))
# Make monthly/quarterly series
qtrser <- vector()
for (i in 1:nrow(Kdata)) {
  j <- floor((i-1)/3)+1
  qtrser[i] <- EQdata[j,ncol(EQdata)]
}
qtrser <- unlist(qtrser)
df <- data.frame(Date=as.Date(Kdata$Date),HHFC=Kdata$HHFC,HHFCPC01=HHCEP01$HHFC)
  qtrser=qtrser)
sub <- 1350000
div <- 28500
c1 <- ggplot(filter(df,Date>=as.Date("2020-09-01")))+
  geom_line(aes(x=Date,y=(HHFC-sub)/div),colour="blue",size=1.5)+
  geom_point(aes(x=Date,y=(HHFC-sub)/div),colour="blue",size=2.5)+
  #geom_line(aes(x=Date,y=(qtrser-sub)/div),colour="forestgreen",
  #  size=2.5,linetype="dashed")+
  geom_col(aes(x=Date,y=HHFCPC01),fill="red")+
  #geom_line(aes(x=Date,y=MA),colour="black",size=1.5)+
  labs(title=paste0("Household real final consumption expenditure, September 2020 to ",LastMonth," 2022"),
    subtitle=paste0("The red bars are monthly percentage changes (right axis)\n",
      #"The black line is a 13-month centred moving average (right axis)\n",
      "The blue line is the household final consumption expenditure level in millions of dollars (left axis)"),
    caption=paste0("Own calculations using Statistics Canada data. ",Sys.time()," @PhilSmith26"),x="") +
  scale_x_continuous(breaks=c(seq.Date(as.Date("2020-01-01"),
    as.Date("2022-07-01"),by="3 months")),labels=
    c("Jan\n2020","Apr\n2020","Jul\n2020","Oct\n2020",
    "Jan\n2021","Apr\n2021","Jul\n2021","Oct\n2021","Jan\n2022","Apr\n2022",
    "Jul\n2022"))+
    scale_y_continuous(
      position="right",breaks=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5),
      name="Monthly % change\n",
      sec.axis=sec_axis(~ . * div + sub,name="Millions of constant 2012 dollars",
        breaks=c(1100000,1150000,1200000,1250000)))+
  theme(plot.title = element_text(size=16,face="bold")) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
  theme(panel.border = element_rect(fill=NA,colour="black"))+
  theme(panel.grid.minor=element_line(colour="lightgrey",size=0.5)) +
  theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
  theme(axis.text.x = element_text(angle=0,hjust=1,size=9)) +
  theme(axis.text.y = element_text(size=12))
c1
ggsave("MHHCE_dual_chart_volSep2022.png",c1,height=8.5,width=11,dpi=300)

#-------------------------------------------------------------------------------
ggplot(IPdata)+
  geom_col(aes(Date,100*(HHFC/lag(HHFC,12)-1)),fill="darkgoldenrod",
    colour="black",size=0.3)+
  labs(title="Monthly implicit price indexes, 12-month percentage change",
    y="12-month % change\n")+
  scale_y_continuous(position="right")+
  geom_hline(yintercept=0)+
  geom_hline(yintercept=2,linetype="dashed")
ggplot(IPdata)+
  geom_col(aes(Date,100*(HHFC/lag(HHFC,1)-1)),fill="darkgoldenrod",
    colour="black",size=0.3)+
  labs(title="Monthly implicit price indexes, 1-month percentage change",
    y="1-month % change\n")+
  scale_y_continuous(position="right")+
  geom_hline(yintercept=0)+
  geom_hline(yintercept=100*(1.02^(1/12)-1),linetype="dashed")
ggplot(Kdata)+
  geom_col(aes(Date,100*(HHFC/lag(HHFC,12)-1)),fill="darkgoldenrod",
    colour="black",size=0.3)+
  labs(title=paste0("Monthly consumer expenditure at constant 2012 prices, ",
    "12-month percentage change"),y="12-month % change\n")+
  scale_y_continuous(position="right")+
  geom_hline(yintercept=0)
ggplot(Kdata)+
  geom_col(aes(Date,100*(HHFC/lag(HHFC,1)-1)),fill="darkgoldenrod",
    colour="black",size=0.3)+
  labs(title=paste0("Monthly consumer expenditure at constant 2012 prices, ",
    "1-month percentage change"),y="1-month % change\n")+
  scale_y_continuous(position="right")+
  geom_hline(yintercept=0)
ggplot(Edata)+
  geom_col(aes(Date,100*(HHFC/lag(HHFC,12)-1)),fill="darkgoldenrod",
    colour="black",size=0.3)+
  labs(title=paste0("Monthly consumer expenditure at current prices, ",
    "12-month percentage change"),y="12-month % change\n")+
  scale_y_continuous(position="right")+
  geom_hline(yintercept=0)
ggplot(Edata)+
  geom_col(aes(Date,100*(HHFC/lag(HHFC,1)-1)),fill="darkgoldenrod",
    colour="black",size=0.3)+
  labs(title=paste0("Monthly consumer expenditure at current prices, ",
    "1-month percentage change"),y="1-month % change\n")+
  scale_y_continuous(position="right")+
  geom_hline(yintercept=0)

