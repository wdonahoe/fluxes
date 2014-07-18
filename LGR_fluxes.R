#!/usr/bin/env Rscript

# William Donahoe, 2014

args = commandArgs( trailingOnly = TRUE )

END <- args[ 1 ]
INPUT_DIR <- args[ 2 ]
GRAPH <- args[ 3 ]
START <- args[ 4 ]
SCRIPTNAME <- "LGR_fluxes.R"
OUTPUT_DIR <- "out"

LINE_R <- 0.1 # cm
SLINE1_L <- 99 # cm
SLINE2_L <- 109 # cm
SLINE1_V <- pi * ( LINE_R ^ 2 ) * SLINE1_L # cm3
SLINE2_V <- pi * ( LINE_R ^ 2 ) * SLINE2_L # cm3

LLINE1_L <- 162 # cm
LLINE2_L <- 174 # cm
LLINE1_V <- pi * ( LINE_R ^ 2 ) * LLINE1_L # cm3
LLINE2_V <- pi * ( LINE_R ^ 2 ) * LLINE2_L # cm3

SLINE_V <- SLINE1_V + SLINE2_V # cm3

LC_DIAM <- 17 #cm
LC_HEIGHT <- 7.4 + ( 7.4 * 0.05 ) #cm
LC_V <- pi * LC_HEIGHT * ( LC_DIAM / 2 ) ^ 2 # cm3

SC_DIAM <- 11 #cm
SC_HEIGHT <- 11.2 #cm
SC_V <- pi * SC_HEIGHT * ( SC_DIAM / 2 ) ^ 2 #cm3

LGR_V <- 401 # sccm

FORMATS <- "%d/%M/%Y"
FORMATL <- "%m/%d/%Y %H:%M:%OS"

# -----------------------------------------------------------------------------
# get_file_table
# Table with name of .csv data file and
# corresponding measurement file.
# 
get_file_table <- function(){
  files <- unlist( list.files( path=INPUT_DIR,pattern="*.csv" ) )
  range <- time_limit( files )
  files <- files[range]

  print(range)

  measurements <- lapply( files, function(x)paste0( head( unlist( strsplit( x,"*.csv" ) ) ),"_measurements.txt") ) 
  measurements <- unlist( measurements[ order( files ) ] )
  
  files <- paste0( INPUT_DIR,"/",files )
  measurements <- paste0( INPUT_DIR,"/",measurements )
  
  file_measure <- data.frame( files,measurements )
  
  names( file_measure ) <- c( "Filename","Measurement" )
  
  stopifnot( length( file_measure$Filename ) == length( file_measure$Measurement ) )
  
  return( file_measure )
} # get_file_table

time_limit <- function( files ){
  start_posix <- as.POSIXct(START, format=FORMATS)
  end_posix <- as.POSIXct(END, format=FORMATS)

  stripped <- sapply(files, strsplit, split="_", fixed=TRUE)
  files <- list()
  for (i in seq(1:length(stripped))){
    add <- unlist(stripped[[i]])[1]
    add <- substring(add,4)
    files <- c(files,add)
  }
  range <- unlist(lapply(files,as.POSIXct,format="%d%b%Y"))
  to_include <- (range >= start_posix & range <= end_posix)

  return(to_include)
}

# -----------------------------------------------------------------------------
# printlog
# Time-stamped output function
#     Arguments: msg -- A string to print
#                ts  -- Time stamp? Default = TRUE
#                cr  -- New line? Default = TRUE
#
printlog <- function( msg="", ..., ts=TRUE, cr=TRUE ) {
  if( ts ) cat( date(), " " )
  cat( msg, ... )
  if( cr ) cat( "\n")
} # printlog

my_list <- function( l ){ vector("list",l) }

# -----------------------------------------------------------------------------
# loadlibs
# Load a list of libraries
#     Arguments: liblist -- A character vector of library names.
# 
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

# ----------------------------------------------------------
# read_csv
# Reads a .csv file w/ header and skipping first line.
#     Arguments: fn    -- A filename (.csv).
#     Returns:   data  -- A data frame.
read_csv <- function( fn ){
  stopifnot( file.exists( fn ) )
  
  data <- read.csv( fn, skip=1, header=T )
  data$filename <- fn
  
  return( data )
} #read_csv


# ----------------------------------------------------------
# savedata
# Save a data frame
#     Arguments: df        -- A data frame to save.
#                extension -- file extension. Default = .csv
savedata <- function( d, extension=".csv" ) {
  stopifnot( file.exists( OUTPUT_DIR ) )
  fn <- paste0( OUTPUT_DIR, "/", format( Sys.time(),"%d%B%Y" ),"_fluxes",extension )
  printlog( "Saving", fn )
  write.csv( d, fn, row.names=FALSE )
} # savedata

# ------------------------------------------------------------
# plots
# Plot and save time-series for CO2, CH4, and H2O. Requires 'xts.'
#     Arguments: raw  -- A data frame with all gas data, not only those within an experiment.
#
plots <- function( raw ){
  loadlibs( c( "xts" ) )
  
  raw.xts <- as.xts( raw, order.by=as.POSIXct( raw$Time,format=FORMATL ) )
  names <- c( "H2O","CH4","CO2" )
  pdf( file=paste0( OUTPUT_DIR,"/Time_Series_plots.pdf" ) )
  par( cex=.7, las=1, bg="#E0FFFF" )
  for ( i in seq( 4,2 ) ){
    plot( raw.xts[ ,i,with=FALSE ],major.format=F,minor.ticks=FALSE,main=names[ i-1 ],xlab=NA,ylab=NA )
  }
  graphics.off()
}

# ----------------------------------------------------------------------
# r2
# Get r^2 values for each measurement and for CO2, CH4, and H2O.
#   Arguments: d -- A cleaned data frame.
#   Returns      -- A vector containing each r^2 value.
#
r2 <- function( d ) {
  
  mods <- dlply( d, .( Measurement ), lm, formula = as.numeric(CO2) ~ as.numeric(correct_time(d)) )
  co2_r2 <- ldply( mods, .fun=function( x ){ round( summary( x )$r.squared, 2 ) } )
  names( co2_r2 ) <- c( "Measurement", "R2" )
  co2_r2 <- co2_r2[ order( co2_r2$Measurement ), ]
  
  mods <- dlply( d, .( Measurement ), lm, formula = as.numeric(CH4) ~ as.numeric(correct_time(d)) )
  ch4_r2 <- ldply( mods, .fun=function( x ){ round( summary( x )$r.squared, 2 ) } )
  names( ch4_r2 ) <- c( "Measurement", "R2" )
  ch4_r2 <- ch4_r2[ order( ch4_r2$Measurement ), ]
  
  mods <- dlply( d, .( Measurement ), lm, formula = as.numeric(H2O) ~ as.numeric(correct_time(d)) )
  h2o_r2 <- ldply( mods, .fun=function( x ){ round( summary( x )$r.squared, 2 ) } )
  names( h2o_r2 ) <- c( "Measurement", "R2" )
  h2o_r2 <- h2o_r2[ order( h2o_r2$Measurement ), ]
  
  return( c(co2_r2$R2, ch4_r2$R2, h2o_r2$R2)  )
}

#------------------------------------------------------------------
# p_vals
# Find p-values for CO2, CH4, H2O.
#     Arguments: d -- A data frame split by measurement.
#     Returns:     -- A vector containing p-values for CO2, CH4, and H2O.
#
p_vals <- function( d ){
  return( c( t.test( d$CO2 )$p.value,t.test( d$CH4 )$p.value,t.test( d$H2O )$p.value ) )
}

#------------------------------------------------------------------
# split_at
# Split a vector by a list of supplied indices into a list of vectors.
#     Arguments: x   -- A vector.
#                pos -- A list of indices.
#     Returns:       -- A list of vectors split by pos.   
#
split_at <- function( x, pos ) unname( split( x, cumsum( seq_along( x ) %in% pos ) ) )

clean <- function( d ){
  # keep the data we need.
  d <- d[ ,colnames( d )%in%c( "Time","X.CO2.d_ppm","X.CH4.d_ppm","X.H2O._ppm","AmbT_C","filename" ) ]
  names( d ) <- c( "Time","H2O","CH4","CO2","T","Filename" )
  
  data <- data.table( d )
  setkey( data,Time )
  data$Time <- as.POSIXct( data$Time,format=FORMATL )
  
  # cleanup garbage rows.
  data <- data[ !is.na( Time ) ]
  
  return( data )
}

#-------------------------------------------------------------------
# get_cleaned_data_table
# Remove extraneous entries, include measurement times, find peak values, and 
# prepare data table for analysis by removing data outside of measurements.
#     Arguments: d          -- Raw data frame from file-input.
#     Returns:   data.clean -- Cleaned data ready for analysis.
#
get_cleaned_data_table <- function( d, ft ) {
  
  data.time <- d$Time
  
  measurements <- read.table( as.character( ft[ match( d$Filename[ 1 ],ft$Filename ),"Measurement" ] ),sep="\t",header=TRUE )
  measurements <- sapply( measurements,as.POSIXct,format=FORMATL )[ ,1 ]
  
  # find the nearest timestamp in data for each time stamp in measurements.
  infl <- data.table( data.time,val=data.time )
  setattr( infl,"sorted","data.time" )
  infl <- infl[ J(measurements),.I,roll="nearest" ]
  
  # get co2 for each experiment, find peak values.
  data.dco2 <- split_at( d$H2O, infl$.I )[ -1 ]
  infl$peaks <- infl$.I + ldply( data.dco2,function( x ) match( max( x ),x ) )
  
  measures <- paste0( "Measurement",seq( 1:( length( infl$peaks ) - 1 ) ) )
  clean <- list()
  for (i in 1:( nrow( infl ) - 1) ) {
    clean[[i]] <- seq( infl$.I[i],infl$peaks[i] )
  }
  
  # pre-allocate list because it is long.
  l <- length(clean)
  meas <- my_list(l)
  for ( i in 1:l ){
    meas[[i]] <- replicate( length( clean[[i]] ),measures[i] )
  }
  
  # remove data not in experiments.
  data.clean <- d[ unlist( clean ) ]
  data.clean$Measurement <- unlist( meas )
  
  
  return ( data.clean )
  
}

#---------------------------------------------------------
# correct_time
# Get times for a measurement w/ start time at zero.
#     Arguments: d -- data frame containing data from one measurement.
#     Returns:     -- all times minus start time.
#
correct_time <- function( d ){ d$Time - d$Time[ 1 ] }

#---------------------------------------------------------
# compute_flux
# Compute flux values for CO2, CH4, and H2O
#     Arguments: d -- data frame containing data from one measurement.
#     Returns:   ret -- A vector with flux values and average temperature.
#
compute_flux <- function( d ) {

  time <- correct_time( d )
  start_time <- as.POSIXct( d$Time[1],format=FORMATL )
  
  mco2 <- lm( as.numeric(d$CO2) ~ as.numeric(time))
  mch4 <- lm( as.numeric(d$CH4) ~ as.numeric(time))
  mh2o <- lm( as.numeric(d$H2O) ~ as.numeric(time))
  
  Resp_raw_co2 <- as.numeric( coef( mco2 )[ 2 ] )  # i.e. the co2 slope.
  Resp_raw_ch4 <- as.numeric( coef( mch4 )[ 2 ] )  # i.e. the ch4 slope.
  Resp_raw_h2o <- as.numeric( coef( mh2o )[ 2 ] )  # i.e. the h20 slope.
  
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
  
  V <- ( SLINE_V + SC_V + LGR_V ) * 1.0e-6 # m3
  S <- pi * ( SC_DIAM / 2 ) ^ 2
  
  R       <- 8.3145e-3                            # m-3 kPa mol-1 K-1
  Kelvin    <- 273.15                             # C to K conversion
  avg_temp  <- mean( d$T )

  # Convert from umol/g soil/s to mgC/kg soil/day
  R_ewd_CO2 <- Resp_raw_co2 * ( V / S ) * R * ( Kelvin / ( Kelvin + avg_temp ) )
  R_ewd_CH4 <- Resp_raw_ch4 * ( V / S ) * R * ( Kelvin / ( Kelvin + avg_temp ) )
  R_ewd_H2O <- Resp_raw_h2o * ( V / S ) * R * ( Kelvin / ( Kelvin + avg_temp ) )
  
  flux_co2 <- R_ewd_CO2 / 1e6 * 12 * ( 1000 ^ 2) * ( 60 ^ 2 ) * 24
  flux_ch4 <- R_ewd_CH4 / 1e6 * 12 * ( 1000 ^ 2) * ( 60 ^ 2 ) * 24
  flux_h2o <- R_ewd_H2O / 1e6 * 12 * ( 1000 ^ 2) * ( 60 ^ 2 ) * 24

  ret <- c( flux_co2, flux_ch4, flux_h2o, ( avg_temp + Kelvin ) )
  names( ret ) <- c( "flux_co2", "flux_ch4", "flux_h2o", "Avg_Temp" )
  return( ret )
} # compute_flux

#main---------------------------------------------

if( !file.exists( OUTPUT_DIR ) ) {
  printlog( "Creating", OUTPUT_DIR )
  dir.create( OUTPUT_DIR )
}

loadlibs( c( "plyr", "data.table" ) )

printlog( "Getting and cleaning data." )
raw <- data.table()
data <- data.table()

files <- get_file_table()

for ( fn in files$Filename ){
  raw <- rbind( raw,clean( read_csv( fn ) ) )
}

data <- raw[ , get_cleaned_data_table( .SD,files ),by=Filename,.SDcols=colnames( raw ) ]

printlog( "Running quality-control." )
qc_r2 <- data.table()
qc_p <- data.table()

qc_r2 <- as.data.table( ddply( data,.( Filename,Measurement ),.fun=r2 ) )
qc_p <- as.data.table( ddply( data,.( Filename,Measurement ),.fun=p_vals ) )

setkey( qc_r2,Filename )
setkey( qc_p,Filename )

qc <- merge( qc_r2, qc_p,by=c( "Filename","Measurement" ) )

setnames( qc,c( "Measurement", "V1.x",  "V2.x",
              "V3.x",	"V1.y",	"V2.y",	"V3.y" ),
            c( "Measurement","CO2_r2","CH4_r2",
              "H2O_r2","CO2_p","CH4_p","H2O_p" ) )

fluxes <- ddply( data, .( Measurement, Filename ), .fun=compute_flux )
alldata <- as.data.table( merge( fluxes, qc,by=c( "Filename","Measurement" ) ) )
setkey( alldata,Filename )

# sort alldata table by measurement number.
alldata <- alldata[ order( Filename,sapply( Measurement,function( x ){
  as.numeric( unlist( strsplit( x,"[:digit:]" ) )[2] )
  } ) ) ]

savedata( alldata )

if ( GRAPH ){
  plots( raw, data )
}

printlog( "All done with", SCRIPTNAME )








