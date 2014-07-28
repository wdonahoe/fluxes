#!/usr/bin/env Rscript

# William Donahoe, 2014

# TODO: Sort alldata by measurement.

args = commandArgs( trailingOnly = TRUE )

END <- args[ 1 ]
INPUT_DIR <- args[ 2 ]
GRAPH <- args[ 3 ]
MIN_R2 <- args[ 4 ]
START <- args[ 5 ]
SCRIPTNAME <- "LGR_fluxes.R"
OUTPUT_DIR <- "out"
MAX_P <- 0.05

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
  # match all 'gga[DATE](...).csv' files
  files <- unlist( list.files( path=INPUT_DIR,pattern="^gga[0-9]{2}[A-Z][a-z]+[0-9].+\\.csv" ) )
  range <- time_limit( files )
  files <- files[range]

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
    loadedlibs[ lib ] <- require( lib, character.only=TRUE, warn.conflicts=FALSE )
    if( !loadedlibs[ lib ] )
      warning( "this package is not installed! Installing." )
      install.packages( lib, dependencies=TRUE, quiet=TRUE )
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
  par( cex=.7, las=1)
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
quality_control <- function( d ) {
  
  t <- as.numeric(correct_time(d))

  mods <- dlply( d, .( Measurement ), lm, formula = as.numeric(CO2) ~ t )
  co2_p <- ldply(mods, .fun=function( x ){ summary( x )$coefficients[2,4]})
  co2_r2 <- ldply( mods, .fun=function( x ){ round( summary( x )$r.squared, 2 ) } )
  names( co2_p ) <- c( "Measurement", "p_val" )
  names( co2_r2 ) <- c( "Measurement", "R2" )
  co2_p <- co2_p[ order( co2_p$Measurement ), ]
  co2_r2 <- co2_r2[ order( co2_r2$Measurement ), ]
  
  mods <- dlply( d, .( Measurement ), lm, formula = as.numeric(CH4) ~ t )
  ch4_p <- ldply(mods, .fun=function( x ){ summary( x )$coefficients[2,4]})
  ch4_r2 <- ldply( mods, .fun=function( x ){ round( summary( x )$r.squared, 2 ) } )
  names( ch4_p ) <- c( "Measurement", "p_val" )
  names( ch4_r2 ) <- c( "Measurement", "R2" )
  ch4_p <- ch4_p[ order( ch4_p$Measurement ), ]
  ch4_r2 <- ch4_r2[ order( ch4_r2$Measurement ), ]
  
  mods <- dlply( d, .( Measurement ), lm, formula = as.numeric(H2O) ~ t )
  h2o_p <- ldply(mods, .fun=function( x ){ summary( x )$coefficients[2,4]})
  h2o_r2 <- ldply( mods, .fun=function( x ){ round( summary( x )$r.squared, 2 ) } )
  names( h2o_p ) <- c( "Measurement", "p_val" )
  names( h2o_r2 ) <- c( "Measurement", "R2" )
  h2o_p <- h2o_p[ order( h2o_p$Measurement ), ]
  h2o_r2 <- h2o_r2[ order( h2o_r2$Measurement ), ]
  
  return( c(co2_r2$R2, ch4_r2$R2, h2o_r2$R2, co2_p$p_val, ch4_p$p_val, h2o_p$p_val)  )
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
  buffer <- 30

  data.time <- d$Time
  
  measurements <- read.table( as.character( ft[ match( d$Filename[ 1 ],ft$Filename ),"Measurement" ] ),sep="\t",header=TRUE )
  measurements <- sapply( measurements,function(x){as.POSIXct(x,format=FORMATL)+buffer})[ ,1 ]

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
  
  # pre-allocate list because it is long
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

  #l <- lm( as.numeric(d$CO2,d$CH4,d$H2O) ~ as.numeric(time))
  
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
  
  V <- ( SLINE_V + SC_V + LGR_V ) * 1.0e-6 # cm3 -> m3
  S <- (pi * ( SC_DIAM / 2 ) ^ 2) * 1.0e-4 # cm2 -> m2
  
  R       <- 8.3145e-3                            # m-3 kPa mol-1 K-1
  Kelvin    <- 273.15                             # C to K conversion
  avg_temp  <- mean( d$T ) + Kelvin
  Pa <- 101 #kPa TODO: verify with site.

  # umol m-2 sec-1
  flux_CO2 <- Resp_raw_co2 * ( V / S ) * Pa / ( R * avg_temp )
  # nmol m-2 sec-1
  flux_CH4 <- Resp_raw_ch4 * ( V / S ) * Pa / ( R * avg_temp ) * 1.0e3 #umol -> nmol
  # umol m-2 sec-1
  flux_H2O <- Resp_raw_h2o * ( V / S ) * Pa / ( R * avg_temp )

  ret <- c( flux_CO2, flux_CH4, flux_H2O, ( avg_temp - Kelvin ) )
  names( ret ) <- c( "flux_co2 [umol m-2 sec-1]", "flux_ch4 [nmol m-2 sec-1]", "flux_h2o [umol m-2 sec-1]", "Avg_Temp [C]" )
  return( ret )
} # compute_flux

# -------------------------------------------------------
# beyond_threshold
# Determines if a measurement is up to r2 and p value standards. 
#   Arguments:  qc -- Quality control data table.
#  
beyond_threshold <- function( qc ){
  test_rp <- function( row ){
    r2 <- all(as.double(row[3:5]) >= MIN_R2)
    p <- all(as.double(row[6:8]) <= MAX_P)
    return( r2 && p )
  }
  ret <- apply( qc,1,test_rp )
  return( ret )
}

# ------------------------------------------------
# get_alldata
# Compute fluxes and merge with qc table. 
# Arguments:  d  -- Data from all files, cleaned.
#             qc -- Quality control data for all measurements.
#
get_alldata <- function( d, qc ){
  fluxes <- ddply( d, .( Measurement, Filename ), .fun=compute_flux )
  alldata <- as.data.table( merge( fluxes, qc,by=c( "Filename","Measurement" ) ) )

  alldata <- alldata[beyond_threshold( qc )]

  # sort alldata table by measurement number.
  alldata <- alldata[ order(-rank(Filename),
    sapply( Measurement,
      function( x ){
        as.numeric( unlist( strsplit( x,"[:digit:]" ) )[2] )
      }
    ) ) ]

  return( alldata )

}

#main---------------------------------------------

stopifnot( file.exists( INPUT_DIR ) )

if( !file.exists( OUTPUT_DIR ) ) {
  printlog( "Creating", OUTPUT_DIR )
  dir.create( OUTPUT_DIR )
}

loadlibs( c( "plyr", "data.table" ) )

cat("------------------------\n")
printlog( "Getting and cleaning data." )
raw <- data.table()
data <- data.table()

files <- get_file_table()

for ( fn in files$Filename ){
  raw <- rbind( raw,clean( read_csv( fn ) ) )
}

data <- raw[ , get_cleaned_data_table( .SD,files ),by=Filename,.SDcols=colnames( raw ) ]

printlog( "Running quality-control." )
qc <- as.data.table(ddply( data,.( Filename,Measurement ),.fun=quality_control))

setnames( qc,c( "Measurement", "V1",  "V2",
              "V3", "V4", "V5", "V6" ),
            c( "Measurement","CO2_r2","CH4_r2",
              "H2O_r2","CO2_p","CH4_p","H2O_p" ) )

printlog( "Computing fluxes and combining data from all files." )
alldata <- get_alldata( data, qc )

savedata( alldata )

if ( GRAPH ){
  plots( raw )
}

printlog( "All done with", SCRIPTNAME )








