


#####initialization#######################
#tempset="t2m" #set for outputdir!

startdate=as.Date("1995/01/02") #start day of the interpolation
stopdate=as.Date("2014/06/29") #stop day of the interpolation
time=seq(startdate,stopdate,by="days") 
#you can also generate a sequence with for example only one day of the months/years
#time=seq(startdate,stopdate,by="months") #now a vector with the 2nd day of all months is created
year=as.numeric(format(time,"%Y"))
time=as.numeric(format(time, "%Y%m%d"))

# finding the directory of Racmo daily data

#RACMO_loc_new<-gsub("tempset",tempset,RACMO_loc)

#to do: Choose the min, mean or max temperature for the interpolation
#where: r.90
##########################################
#DATA IMPORT
inputdata<-read.table("./TemperatuurDag.txt",header=T, sep=",",dec=".",na.strings="/////",colClasses =c(rep("character",2),rep("numeric",6)))
#some files have difficulties because the station at Vlieland has no data, 
#which is displayed in the text file as /////
#so in read.table na.strings="/////" is added
#Now still the data will be read as factor, to overwrite this comand colClasses is used, here the class
#of each column is specified
inputdata = inputdata[inputdata$Datum==HARMONIE_datum,] #The datum of the inputdata equals the RACMO_datum
inputdata[] <- lapply(inputdata, function(x) type.convert(as.character(x)))
#This is to avoid "Factor" problems with the .txt file

coordinates(inputdata) = ~RDN_X+RDN_Y #the coordinates of inputdata, compare to headers

#Choose the min, mean or max temperature interpolation
#inputdata$Tg_new=inputdata$Tn #minimum temperature observation at 1.5m [Celcius]
inputdata$Tg_new=inputdata$Tg #mean temperature observation at 1.5m [Celcius]
#inputdata$Tg_new=inputdata$Tx #maximum temperature observation at 1.5m [Celcius]

inputdata <- inputdata[!is.na(inputdata$Tx),] #removing nan data from the temperature
#kriging and other interpolations wont work with NA values
savedir<-"E:\\R_scripts_HARMONIE\\output_HARMONIE\\output_int_day"
workdir<-'E:\\Temperature_interpolation\\input'
nl.grd <- read.asciigrid("./wn_maskbuffer_001.asc")
distshore.grd<-read.asciigrid('E://Temperature_interpolation//input//wn_distshore_001.asc',colname="distshore")

# from the functions crossvalidate
#' 
#' # Use log(zinc) as dependent variable
#'
#' meuse$log_zinc = log(meuse$zinc)
#' kedlog_cv = crossvalidate(log_zinc~dist, meuse, func = "krige", 
#'         model = autofitVariogram(zinc~dist, meuse, model = "Ste")$var_model, debug.level = 0)
#' 
#'# Test if crossvalidate matches krige.cv
#' dum = krige.cv(zinc~dist, meuse, model = "Ste")
#' all.equal(bla$residual, ked_cv$residual)
#' 
#'
#' #from doNearestneughbor

#'
#Form doTPS
