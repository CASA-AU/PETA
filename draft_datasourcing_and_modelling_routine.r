# -------------------------------------------------------------------------------------------------------------------------------------------------------- 
# Data Sourcing mechanism development
# R Script for developing and testing data sourcing mechanism for the Trend Analysis Tool
# -------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     Copyright (C) 2018  Civil Aviation Safety Authority
# 
#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 2 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License along
#     with this program; if not, write to the Free Software Foundation, Inc.,
#     51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

suppressPackageStartupMessages(library(pscl))         ## needed for zeroinfl
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(strucchange))  ## need for breakpoints
library(plyr)
suppressPackageStartupMessages(library(lubridate))    ## Parse date strings
library(tidyr)
library(svglite)
library(yaml)

szIndir <- 'inputs/'
szOutdir <- paste0('outputs/', format(Sys.time(), '%Y%m%d_%H%M'), "/")
szFunctionsdir <- 'subroutines_and_functions/'

stopifnot(dir.exists(szIndir))
stopifnot(dir.exists(szFunctionsdir))

if (!dir.exists(szOutdir)) {
  dir.create(szOutdir)
}

##
## Read configuration
##

config <- read_yaml("PETA.yaml")

# We can only process one source at a time - configure with this variable.
# TODO - See if we can process multiple sources in one run.

source <- 'ASIR'

##
## Read input files
##
## Where the configuration file includes extra information for a file, these operations are applied.
##
l_ply(names(config[[c(source,'Input')]]), function(input) {
  stopifnot(file.exists(paste0(szIndir, config[[c(source,'Input',input,'Filename')]])))
  
  tmp <- read.csv(paste0(szIndir, config[[c(source,'Input',input,'Filename')]]), stringsAsFactors = FALSE)
  
  ##
  ## Identifier is used to nominate a field that should be used as the identifier.
  ##
  if (!is.null(config[[c(source,'Input',input,'Identifier')]])) {
    colnames(tmp)[match(config[[c(source,'Input',input,'Identifier')]], colnames(tmp))] <- config[[c(source,'Identifier')]]
  }
  
  ##
  ## ConcatField is used to concatenate columns of data into one field.
  ##
  if (!is.null(config[[c(source,'Input',input,'ConcatField')]])) {
    for (field in names(config[[c(source,'Input',input,'ConcatField')]])) {
      tmp <- unite(
        tmp,
        !!field,
        config[[c(source,'Input',input,'ConcatField',field,'Field')]],
        sep = config[[c(source,'Input',input,'ConcatField',field,'Separator')]],
        remove = FALSE
      )
    }
  }
  
  ##
  ## MapValues is used to map rubbish data to cleansed values.
  ##
  if (!is.null(config[[c(source,'Input',input,'MapValues')]])) {
    for (field in names(config[[c(source,'Input',input,'MapValues')]])) {
      
      fieldMap <- unlist(lapply(names(config[[c(source,'Input',input,'MapValues',field)]]), function(manu) {
        x <- character(length = length(config[[c(source,'Input',input,'MapValues',field,manu)]]))
        names(x) <- config[[c(source,'Input',input,'MapValues',field,manu)]]
        x[TRUE] <- manu
        return(x)
      }))
      
      tmp[tmp[,field] %in% names(fieldMap), field] <- fieldMap[tmp[tmp[,field] %in% names(fieldMap), field]]
    }
  }
  
  assign(input, tmp, envir = .GlobalEnv)
})

###
### Get the start and end dates from the configuration
### Filter the occurrence data by date.
###

endDate <- ymd(config[[c(source,'Date','End')]])
if (is.null(config[[c(source,'Date','Start')]])) {
  startDate <- endDate - months(60)
} else {
  startDate <- ymd(config[[c(source,'Date','Start')]])
}

dfOcc$Date <- as.Date(dfOcc[, config[[c(source,'Date','Field')]]], format = config[[c(source,'Date','Format')]])
dfOcc <- dfOcc[dfOcc$Date > startDate & dfOcc$Date <= endDate, ]

quarters <- seq(from = startDate, to = endDate, by = "3 months")

dfOcc$yr_Q <- cut(dfOcc$Date, breaks = quarters, right = TRUE)

## Filter the other data.frames by Identifier

dfOccType <- dfOccType[dfOccType[, config[[c(source,'Identifier')]]] %in% dfOcc[, config[[c(source,'Identifier')]]], ]
dfOccAC <- dfOccAC[dfOccAC[, config[[c(source,'Identifier')]]] %in% dfOcc[, config[[c(source,'Identifier')]]], ]

## Add the yr_Q column to the other data.frames

dfOccType <- merge(dfOccType, dfOcc[,c(config[[c(source,'Identifier')]],"yr_Q")], by = config[[c(source,'Identifier')]], all.x=T)
dfOccAC <- merge(dfOccAC, dfOcc[,c(config[[c(source,'Identifier')]],"yr_Q")], by = config[[c(source,'Identifier')]], all.x=T)

###
### Peform source-specific munging of the data.
###

if (source == 'CIRRIS') {
  dfOcc <- dfOcc[!(is.na(dfOcc$Occurrence.Report.Number) | dfOcc$Occurrence.Date == ""), ]
  
  dfOccType <- separate(
    dfOccType,
    Occurrence.Type...Temporary.Key,
    into = c("Occurrence.Report.Number","Occurrence.Type...Key"),
    sep = 11
  )

  dfOccAC[dfOccAC$VH.Aircraft...Registered.Operator.Name..Derived. == "", "VH.Aircraft...Registered.Operator.Name..Derived."] <- "UNKNOWN"

} else if (source == 'ASIR') {

  dfOcc$isFatal <- ifelse(dfOcc$Occurrence.Highest.Injury.Level == "Fatal", "IS_FATAL", "NOT_FATAL")
  
  dfOccType$Occurrence.Type.Description <- sub(' : $', '', dfOccType$Occurrence.Type.Description)
  
  dfOccType <- dfOccType[dfOccType$Occurrence.Type.Description != " : ", ]
  
  ## Aircraft Altitude - combination of Estimate type and actual value
  arEstAlt <- paste0(
    dfOccAC$Aircraft.Altitude.Estimate.Type
    , ifelse(
      is.na(dfOccAC$Aircraft.Altitude.Value)
      , '', paste0(' ', dfOccAC$Aircraft.Altitude.Value)
    )
  )
  arEstAlt <- ifelse(arEstAlt == '', NA, arEstAlt)
  dfOccAC$Aircraft.Altitude.Est.TypeValue <- arEstAlt
  
  ## Full aircraft airspace type - append description for type = "Other" if available
  dfOccAC$Aircraft.Airspace.Type.Full <- paste0(
    dfOccAC$Aircraft.Airspace.Type
    , ifelse(
      dfOccAC$Aircraft.Airspace.Type == 'Other'
      , paste0('_', dfOccAC$Aircraft.Airspace.Type.Other.Description)
      , ''
    )
  )
  
  ## Full aircraft airspace class type - append description for type = "Other" if available
  dfOccAC$Aircraft.Airspace.Class.Type.Full <- paste0(
    dfOccAC$Aircraft.Airspace.Class.Type
    , ifelse(
      dfOccAC$Aircraft.Airspace.Class.Type == 'Other'
      , paste0('_', dfOccAC$Aircraft.Airspace.Class.Other.Description)
      , ''
    )
  )
  
  dfOccAC$Aircraft.Engine.Manufacturer <- toupper(trimws(gsub('"', "", dfOccAC$Aircraft.Engine.Manufacturer)))
  dfOccAC$Aircraft.Engine.Manufacturer <- ifelse(dfOccAC$Aircraft.Engine.Manufacturer %in% c("","1","UNKNO"),"UNKNOWN",dfOccAC$Aircraft.Engine.Manufacturer)
  dfOccAC$Aircraft.Engine.Manufacturer <- ifelse(dfOccAC$Aircraft.Engine.Manufacturer == "AIRCRAFT NOT FITTED WITH ENGINE", "N/A", dfOccAC$Aircraft.Engine.Manufacturer)
  dfOccAC$Aircraft.Engine.Type <- toupper(trimws(dfOccAC$Aircraft.Engine.Type))
  dfOccAC$Aircraft.Engine.Type <- ifelse(dfOccAC$Aircraft.Engine.Type == "", "UNKNOWN", dfOccAC$Aircraft.Engine.Type)
  
  ## Underscore and less/greater than signs cannot be used in formulas
  
  dfOccAC[dfOccAC$Aircraft.Mapped.ICAO.Type.Designator == "_UAV", "Aircraft.Mapped.ICAO.Type.Designator"] <- "XUAVX"
  dfOccAC[dfOccAC$Aircraft.Mapped.ICAO.Type.Designator == "<UK>", "Aircraft.Mapped.ICAO.Type.Designator"] <- "XUNKX"
  
  ## Create an aerodrome column
  
  dfOcc$Aerodrome.Distance.to.Occurrence.NM <- sub(",","", dfOcc$Aerodrome.Distance.to.Occurrence.NM)
  dfOcc$Aerodrome.Distance.to.Occurrence.NM <- as.numeric(dfOcc$Aerodrome.Distance.to.Occurrence.NM)
  dfOcc$Aerodrome <- ifelse(dfOcc$Aerodrome.Distance.to.Occurrence.NM < 10, dfOcc$Aerodrome.Name, "MORE THAN TEN NM")
} else {
  stop('Unknown source')
}

#   --------------------------    Collation of all data frames into a list   ---------------------------------------    #

## Collate into list
lzOcc <- list(dfOcc = dfOcc, dfOccType = dfOccType, dfOccAC = dfOccAC)

## Setting up dummy exposure data as an example
## Note that this exposure data assumes that the timeframe starts 1st quarter and finishes 4th quarter.
## More work is required to make this exposure data really useful.
##
## Note that the exposure data will be specific to a particular segment (e.g. BITRE data on RPT movements,
## or Airservices Australia data on movements at a particular aerodrome.) This tool will use the exposure
## data for every combination of variables so the analyst must be mindful of what combinations make sense.

# yearRange <- seq(year(startDate), year(endDate))
# 
# exposuredat <- data.frame(year = rep(yearRange, each = 4),
#                           quarter = rep(1:4, times = length(yearRange)),
#                           exposure = seq_len(length(yearRange) * 4))

## Load required functions
source(paste0(szFunctionsdir, 'DFBuild.r'))
source(paste0(szFunctionsdir, 'PatchVarname.r'))
source(paste0(szFunctionsdir, 'Modelling_functions.r'))
source(paste0(szFunctionsdir, 'RunTAnalysis.r'))

# This function copied from the package "gdata" to remove the dependency on that package.
# A few small tweaks have been made to simplify the function.

rename.vars <- function(data, from = "", to = "", debug = FALSE) {
  stopifnot(length(from) == length(to))
  dfn <- names(data)
  stopifnot(length(dfn) >= length(to))
  
  chng <- match(from, dfn)
  
  stopifnot(all(from %in% dfn))
  if (debug) {
    cat(to, sep = "\n")
    cat("---")
  }
  stopifnot(length(to) == length(unique(to)))
  
  dfn.new <- dfn
  dfn.new[chng] <- to
  
  names(data) <- dfn.new
  
  return(data)
}

#  --------------- Trend analysis procedures  -----------------#
## Step 1:  Generate a table of counts by the variable of interest for each quarterly period
##		     This is obtained via fnDFBuild funcion as demostrated below.
##		     The table of counts created by the function is called datfrm - it will also contain parameters required for 
##		     fitting the models, such as trend and seasonality terms.
##		     The function also creates a list of values or combination of values from the variable/s of interest to 
## 			fit the models.
## Step 2:  The modelling function is set up to run the analysis on all the values or combination of values in datfrm.  
## 			 In doing so, it takes the field names in datfrm to derive the model formula, and by design, some characters, 
## 			 such as hyphon and backslash, must not be included in the field names.  This step reformats the field names in 
## 			 datfrm to replace such characters with acceptable ones.
## Step 3:  Run trend analysis function - This function saves a table of flags in its raw format in a CSV format to the 
## 			 directory specified by the user as fOutdir.  
## Step 4:  Convert the raw output file to the final output table with Shiny app.
#  -------------------------------------------------------------------- #

l_ply(names(config[[c(source,'Analyse')]]), function(analysis) {
  
  analysisvars <- paste(config[[c(source,'Analyse')]][[analysis]]$variable, collapse='_by_')
  
  ##  Step 1:  Data frame building
  DATA <- fnDFBuild(fVarlist = config[[c(source,'Analyse')]][[analysis]]$variable
                    , fVarClass = config[[c(source,'Analyse')]][[analysis]]$class
                    , fDFList = lzOcc
                    , fRefID = config[[c(source,'Identifier')]]
                    # , fexposuredat = exposuredat
  )
  
  ## Step 2: 
  ## Generate variable names that can be used in models
  DATA$varname <- sapply(DATA$arVarvals, fnPatchVarname)
  ## Rename variables
  DATA$dat <- rename.vars(DATA$datfrm, DATA$arVarvals, DATA$varname)
  
  longData <- t(DATA$dat[, DATA$varname])
  orderedVars <- rownames(longData)[order(rowSums(longData), decreasing = TRUE)]
  
  ## Step 3: Run trend analysis and generate table of tags
  fnRunTAnalysis(
    dat = DATA$dat
    , varname = orderedVars
    , fOutdir = szOutdir
    , fanalysisvar = analysisvars
    , fexposuredat = FALSE
    , plotOutput = TRUE
  )
})

###
### Combine all output files into a single file
###

output_files <- list.files(path = szOutdir, pattern = ".*csv", full.names = TRUE)

output <- ldply(output_files, read.csv, stringsAsFactors = FALSE)

write.csv(output, file = paste0(szOutdir, "Output.csv"), row.names = FALSE)

cat(as.yaml(config), file = paste0(szOutdir, "PETA.yaml"))

