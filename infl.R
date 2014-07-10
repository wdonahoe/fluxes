#!/usr/bin/env Rscript

args = commandArgs( trailingOnly = TRUE )

#INPUT_FILE <- args[ 2 ]
SCRIPTNAME <- "LGR_fluxes.R"
INPUT_FILE <- "gga25Jun2014_f0001.txt"
OUTPUT_DIR <- "out"
MEASUREMENTS <- "gga25Jun2014_f0001_measurements.txt"
FREQ <- as.numeric( args[ 1 ] )
START <- as.numeric( args[ 3 ] )
END <- as.numeric( args[ 4 ] )

LINE_R <- 0.1 # cm
SLINE1_L <- 99 # cm
SLINE2_L <- 109 # cm
SLINE1_V <- pi * (LINE_R ^ 2) * SLINE1_L # cm3
SLINE2_V <- pi * (LINE_R ^ 2) * SLINE2_L # cm3

LLINE1_L <- 162 # cm
LLINE2_L <- 174 # cm
LLINE1_V <- pi * (LINE_R ^ 2) * LLINE1_L # cm3
LLINE2_V <- pi * (LINE_R ^ 2) * LLINE2_L # cm3

SLINE_V <- SLINE1_V + SLINE2_V # cm3

LC_DIAM <- 17 #cm
LC_HEIGHT <- 7.4 + (7.4 * 0.05) #cm
LC_V <- pi * LC_HEIGHT * (LC_DIAM / 2) ^ 2 # cm3

SC_DIAM <- 11 #cm
SC_HEIGHT <- 11.2 #cm
SC_V <- pi * SC_HEIGHT * (SC_DIAM / 2) ^ 2 #cm3

LGR_V <- 401 # sccm

FORMAT <- "%m/%d/%Y %H:%M:%OS"

# -----------------------------------------------------------------------------
# Time-stamped output function
printlog <- function( msg="", ..., ts=TRUE, cr=TRUE ) {
  if( ts ) cat( date(), " " )
  cat( msg, ... )
  if( cr ) cat( "\n")
} # printlog

loadlibs <- function( liblist ) {
  printlog( "Loading libraries..." )
  loadedlibs <- vector()
  for( lib in liblist ) {
    printlog( "Loading", lib )
    loadedlibs[ lib ] <- require( lib, character.only=TRUE )
    if( !loadedlibs[ lib ] )
      warning( "this package is not installed!" )
  }
  invisible( loadedlibs )
} # loadlibs

read_csv <- function( fn ){
  stopifnot( file.exists( fn ) )
  
  data <- read.csv( fn, skip=1, header=T )
  
  return( data )
} #read_csv

# Save a data frame
savedata <- function( df, extension=".csv" ) {
  stopifnot( file.exists( OUTPUT_DIR ) )
  fn <- paste0( OUTPUT_DIR, "/", deparse( substitute( df ) ), extension )
  printlog( "Saving", fn )
  write.csv( df, fn, row.names=FALSE )
} # savedata

q_control <- function( d ) {
  
  mods <- dlply( d, .( measurement ), lm, formula = as.numeric(CO2) ~ as.numeric(correct_time(d)) )
  co2_r2 <- ldply( mods, .fun=function( x ){ round( summary( x )$r.squared, 2 ) } )
  names( co2_r2 ) <- c( "Measurement", "R2" )
  co2_r2 <- co2_r2[ order( co2_r2$Measurement ), ]
  
  mods <- dlply( d, .( measurement ), lm, formula = as.numeric(CH4) ~ as.numeric(correct_time(d)) )
  ch4_r2 <- ldply( mods, .fun=function( x ){ round( summary( x )$r.squared, 2 ) } )
  names( ch4_r2 ) <- c( "Measurement", "R2" )
  ch4_r2 <- ch4_r2[ order( ch4_r2$Measurement ), ]
  
  mods <- dlply( d, .( measurement ), lm, formula = as.numeric(H2O) ~ as.numeric(correct_time(d)) )
  h2o_r2 <- ldply( mods, .fun=function( x ){ round( summary( x )$r.squared, 2 ) } )
  names( h2o_r2 ) <- c( "Measurement", "R2" )
  h2o_r2 <- h2o_r2[ order( h2o_r2$Measurement ), ]
  
  return( c(co2_r2$R2, ch4_r2$R2, h2o_r2$R2)  )
}

p_val <- function(d,name) {
  n <- length(d[name])
  
  sd <- sd(d[name])
  mean <- mean(d[name])
  p <- lapply(d[name], function(x){ 2*pnorm(-abs((mean-x)/(sd/sqrt(n))))})
  
  return(p)
}

p_vals <- function( d ){
  return( c(t.test(d$CO2)$p.value,t.test(d$CH4)$p.value,t.test(d$H2O)$p.value))
}

splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

# Get data from file and get into proper format for computing flux
get_cleaned_data_table <- function( d ) {
  
  # keep the data we need.
  d <- d[,colnames(d)%in%c("Time","X.CO2.d_ppm","X.CH4.d_ppm","X.H2O._ppm","AmbT_C")]
  names(d) <- c("Time","H2O","CH4","CO2","T")
  
  data <- data.table( d )
  setkey(data,Time)
  data$Time <- as.POSIXct(data$Time,format=FORMAT)
  
  # cleanup garbage rows.
  data <- data[!is.na(Time)]
  data.time <- data$Time
  
  measurements <- read.table(MEASUREMENTS,sep="\t",header=TRUE)
  measurements <- sapply(measurements,as.POSIXct,format=FORMAT)[,1]
  
  # find the nearest timestamp in data for each time stamp in measurements.
  infl <- data.table( data.time,val=data.time )
  setattr( infl,"sorted","data.time" )
  infl <- infl[J(measurements),.I,roll="nearest"]
  
  # get co2 for each experiment, find peak values.
  data.dco2 <- splitAt(data$H2O, infl$.I)[-1]
  infl$peaks <- infl$.I + ldply(data.dco2,function( x ) match( max( x ),x ) )
  
  measures <- paste0( "Measurement",seq( 1:( length( infl$peaks ) - 1 ) ) )
  clean <- list()
  for (i in 1:( nrow( infl ) - 1) ) {
    clean[[i]] <- seq( infl$.I[i],infl$peaks[i] )
  }
  
  meas <- list()
  for (i in 1:length(clean)){
    meas[[i]] <- replicate( length( clean[[i]] ),measures[i] )
  }
  
  # remove data not in experiments.
  data.clean <- data[ unlist( clean ) ]
  data.clean$measurement <- unlist( meas )
  
  return ( data.clean )
  
}

correct_time <- function( d ){d$Time - d$Time[1]}

compute_flux <- function( d ) {

  time <- correct_time(d)
  start_time <- as.POSIXct(d$Time[1],format=FORMAT)
  
  mco2 <- lm( as.numeric(d$CO2) ~ as.numeric(time))
  mch4 <- lm( as.numeric(d$CH4) ~ as.numeric(time))
  mh2o <- lm( as.numeric(d$H2O) ~ as.numeric(time))
  
  Resp_raw_co2 <- as.numeric( coef( mco2 )[2] )  # i.e. the co2 slope.
  Resp_raw_ch4 <- as.numeric( coef( mch4 )[2] )  # i.e. the ch4 slope.
  Resp_raw_h2o <- as.numeric( coef( mh2o )[2] )  # i.e. the h20 slope.
  
  # We want to convert raw respiration (d[CO2]/dt) to a flux using
  # A = dC/dt * V/S * Pa/RT (e.g. Steduto et al. 2002), where
  #   A is CO2 flux (umol/m2/s)
  # dC/dt is raw respiration as above (mole fraction/s)
  #   V is total chamber volume (m3)
  #   ...we are correcting for varying headspaces in the cores
  # S is ground surface area (m2)
  #   ...but we're computing per kg of soil, so using dry mass instead
  # Pa is atmospheric pressure (kPa)
  # R is universal gas constant (8.3 x 10-3 m-3 kPa mol-1 K-1)
  # T is air temperature (K) 
  
  V <- (SLINE_V + SC_V + LGR_V) * 1.0e-6 # m3
  S <- pi * (SC_DIAM / 2) ^ 2
  
  R       <- 8.3145e-3                            # m-3 kPa mol-1 K-1
  Kelvin    <- 273.15                             # C to K conversion
  avg_temp  <- mean( d$T )

  # Convert from umol/g soil/s to mgC/kg soil/day
  R_ewd_CO2 <- Resp_raw_co2 * (V / S) * R * ( Kelvin / (Kelvin + avg_temp ) )
  R_ewd_CH4 <- Resp_raw_ch4 * (V / S) * R * ( Kelvin / (Kelvin + avg_temp ) )
  R_ewd_H2O <- Resp_raw_h2o * (V / S) * R * ( Kelvin / (Kelvin + avg_temp ) )
  
  flux_co2 <- R_ewd_CO2 / 1e6 * 12 * ( 1000 ^ 2) * ( 60 ^ 2 ) * 24
  flux_ch4 <- R_ewd_CH4 / 1e6 * 12 * ( 1000 ^ 2) * ( 60 ^ 2 ) * 24
  flux_h2o <- R_ewd_H2O / 1e6 * 12 * ( 1000 ^ 2) * ( 60 ^ 2 ) * 24

  ret <- c(flux_co2, flux_ch4, flux_h2o, (avg_temp + Kelvin))
  names(ret) <- c("flux_co2", "flux_ch4", "flux_h2o", "Avg_Temp")
  return( ret )
}

#main---------------------------------------------

if( !file.exists( OUTPUT_DIR ) ) {
  printlog( "Creating", OUTPUT_DIR )
  dir.create( OUTPUT_DIR )
}

loadlibs( c( "plyr", "data.table" ) )

data <- get_cleaned_data_table( read_csv("gga25Jun2014_f0001.txt" ) )
cat("Running quality-control.\n")
qc_r2 <- data.frame()
qc_r2 <- as.data.table(ddply(data,.(measurement),.fun=q_control))
setkey(qc_r2,measurement)

qc_p <- data.frame()
qc_p <- as.data.table(ddply(data,.(measurement),.fun=p_vals))
setkey(qc_r2,measurement)

qc <- merge(qc_r2, qc_p)

setnames(qc,c("measurement", "V1.x",  "V2.x",  "V3.x",	"V1.y",	"V2.y",	"V3.y"),c("measurement","CO2_r2","CH4_r2","H2O_r2","CO2_p","CH4_p","H2O_p"))
# sort qc table by measurement number.

fluxes <- ddply(data, .(measurement), .fun=compute_flux)
alldata <- as.data.table(merge(fluxes, qc))
setkey(alldata,measurement)
alldata <- alldata[order(sapply(alldata$measurement,function( x )as.numeric(unlist(strsplit(x,"[:digit:]"))[2])))]

savedata( fluxes )

printlog( "All done with", SCRIPTNAME )
#print( sessionInfo() )








