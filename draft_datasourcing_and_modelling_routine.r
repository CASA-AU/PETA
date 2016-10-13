# -------------------------------------------------------------------------------------------------------------------------------------------------------- 
# Data Sourcing mechanism development
# R Script for developing and testing data sourcing mechanism for the Trend Analysis Tool
# -------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     Copyright (C) 2015  Civil Aviation Safety Authority
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


library(gdata)        ## needed for rename.vars
library(pscl)         ## needed for zeroinfl
library(strucchange)  ## need for breakpoints
library(lubridate)    ## Parse date strings

szIndir <- 'inputs/'
szOutdir <- 'outputs/'
szFunctionsdir <- 'subroutines_and_functions/'

##
## Read input files
##

dfOcc <- read.csv(paste0(szIndir, 'OCCURRENCE.csv'), stringsAsFactors = FALSE)
dfOccType <- read.csv(paste0(szIndir, 'OCCURRENCE_TYPE.csv'), stringsAsFactors = FALSE)
dfOccAC <- read.csv(paste0(szIndir, 'OCCURRENCE_AIRCRAFT.csv'), stringsAsFactors = FALSE)

##
## DO NOT use this tool to analyse numerical data; the regression is performed on the sum of the
## values which is unlikely to be what you want. You can derive a new column from the numerical data
## which is probably what you wanted in the first place.
##
## In this example the "Number of People who Sustained a Fatality" is used to create a new "isFatal"
## column which has the values "IS_FATAL" or "NOT_FATAL".
##
dfOcc$isFatal <- ifelse(dfOcc$Number.of.People.who.Sustained.a.Fatality > 0, "IS_FATAL", "NOT_FATAL")
dfOcc[is.na(dfOcc$isFatal), "isFatal"] <- "NOT_FATAL"

##
## If we were to use the isFatal column then we are probably not interested in "NOT_FATAL" occurrences
## and would need to filter those out...
##

# dfOcc <- dfOcc[dfOcc$isFatal == "IS_FATAL", ]

##
## Define the date range for subsetting the data. Note that this software was
## developed on the assumption that 20 quarters would be used.
##

startDate <- as.Date("2010-01-01")
endDate <- as.Date("2014-12-31")

dfOcc$Date <- as.Date(dmy(dfOcc$Occurrence.Date.and.Time))
dfOcc <- dfOcc[dfOcc$Date >= startDate & dfOcc$Date <= endDate, ]
dfOccAC <- dfOccAC[dfOccAC$Occurrence.ID %in% dfOcc$Occurrence.ID, ]
dfOccType <- dfOccType[dfOccType$Occurrence.ID %in% dfOcc$Occurrence.ID, ]


#    --------------------------    OCCURRENCE data preparation   ---------------------------------------    #

## extract a subset of Occurrence ID and corresponding calendar year + quarter 
dfOccDT <- dfOcc[, c('Occurrence.ID'
										, 'Occurrence.Calendar.Year'
										, 'Occurrence.Calendar.Quarter'
										)
								]
dfOccDT <- dfOccDT[order(dfOccDT$Occurrence.Calendar.Year
												 , dfOccDT$Occurrence.Calendar.Quarter
												 )
									 ,]
## Concatenate Year and Quarter to create year_Q tags for each occurrence
dfOccDT$yr_Q <- paste0(dfOccDT$Occurrence.Calendar.Year, '_'
											 , dfOccDT$Occurrence.Calendar.Quarter
											 )

################################
## Merge dfOccDT to dfOcc, dfOccType and dfOccAC

dfOcc <- merge(dfOcc, dfOccDT[, c('Occurrence.ID', 'yr_Q')], by = 'Occurrence.ID')

dfOccType <- merge(dfOccType, dfOccDT, by = 'Occurrence.ID', all.x=T)

dfOccAC <- merge(dfOccAC, dfOccDT, by = 'Occurrence.ID', all.x=T)

################################
## So far we know that occurrence type description levels have to be concatenated into single variable. 
## More programmatical approach will be required if there are more variables to be derived from a combination of existing 
## variables.
arOTDesc <- sub(' : $', ''
								, paste(dfOccType$Occurrence.Type.Description.Level.1
												, dfOccType$Occurrence.Type.Description.Level.2
												, dfOccType$Occurrence.Type.Description.Level.3
												, sep = ' : '
												)
								)
dfOccType$Occurrence.Type.Description <- arOTDesc

dfOccType <- dfOccType[dfOccType$Occurrence.Type.Description != " : ", ]

################################
## Examples on deriving other possible combined variables

## Full aircraft operation type - combine the main and sub type information
dfOccAC$Aircraft.Operation.Type.Full <- paste(dfOccAC$Aircraft.Operation.Type
																							, dfOccAC$Aircraft.Operation.Sub.Type
																							, sep = '_'
																							)

## Aircraft Altitude - combination of Estimate type and actual value
arEstAlt <- paste0(dfOccAC$Aircraft.Altitude.Estimate.Type
									 , ifelse(is.na(dfOccAC$Aircraft.Altitude.Value)
													  , '', paste0(' ', dfOccAC$Aircraft.Altitude.Value)
													  )
									 )
arEstAlt <- ifelse(arEstAlt == '', NA, arEstAlt)
dfOccAC$Aircraft.Altitude.Est.TypeValue <- arEstAlt

## Full aircraft airspace type - append description for type = "Other" if available
arAATypeFull <- paste0(dfOccAC$Aircraft.Airspace.Type
											 , ifelse(dfOccAC$Aircraft.Airspace.Type == 'Other'
																, paste0('_', dfOccAC$Aircraft.Airspace.Type.Other.Description)
																, ''
																)
											 )
dfOccAC$Aircraft.Airspace.Type.Full <- arAATypeFull

## Full aircraft airspace class type - append description for type = "Other" if available
arACTypeFull <- paste0(dfOccAC$Aircraft.Airspace.Class.Type
											 , ifelse(dfOccAC$Aircraft.Airspace.Class.Type == 'Other'
																, paste0('_', dfOccAC$Aircraft.Airspace.Class.Other.Description)
																, ''
																)
											 )
dfOccAC$Aircraft.Airspace.Class.Type.Full <- arACTypeFull


#   --------------------------    Collation of all data frames into a list   ---------------------------------------    #

## Collate into list
lzOcc <- list(dfOcc = dfOcc
							, dfOccType = dfOccType
							, dfOccAC = dfOccAC
							)

# ## Save workspace for future reference/back up
# save.image(paste0('data_loading_done_'
# 									, format(Sys.Date(), '%Y%m%d')
# 									, '.RData'
# 									)
# 					)

## Setting up dummy exposure data as an example
## Note that this exposure data assumes that the timeframe starts 1st quarter and finishes 4th quarter.
## More work is required to make this exposure data really useful.
##
## Note that the exposure data will be specific to a particular segment (e.g. BITRE data on RPT movements,
## or Airservices Australia data on movements at a particular aerodrome.) This tool will use the exposure
## data for every combination of variables so the analyst must be mindful of what combinations make sense.

yearRange <- seq(year(startDate), year(endDate))

exposuredat <- data.frame(year = rep(yearRange, each = 4),
                          quarter = rep(1:4, times = length(yearRange)),
                          exposure = seq_len(length(yearRange) * 4))

#   --------------------------     TREND ANALYSIS TOOL  ------------------------------------    #

## Load required functions
source(paste0(szFunctionsdir, 'DFBuild.r'))
source(paste0(szFunctionsdir, 'PatchVarname.r'))
source(paste0(szFunctionsdir, 'Modelling_functions.r'))
source(paste0(szFunctionsdir, 'RunTAnalysis.r'))

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
## Step 4:  Convert the raw output file to the final output table with "trend_analysis_output_generator.xlsm".
#  -------------------------------------------------------------------- #

## Example 1:  analysing the trend in the number of poeple who sustained a fatality
##  Step 1:  Data frame building
fnDFBuild(fVarlist = c('Aircraft.Operation.Type', 'Occurrence.Category.Type')
          , fVarClass = c('character', 'character')
          , fDFList = lzOcc
          , fRefID = "Occurrence.ID"
          , fexposuredat = exposuredat
					)

datfrm  ## Check data frame
arVarvals  ## Check the variables to analyse


## Step 2: 
## Generate variable names that can be used in models
varname <- sapply(arVarvals, FUN = fnPatchVarname)
## Rename variables
dat <- rename.vars(datfrm, arVarvals, varname)  

## Step 3: Run trend analysis and generate table of tags
tagtable0 <- fnRunTAnalysis(dat
                           , varname
                           , fOutdir = szOutdir
                           , fanalysisvar = analysisvars
                           , fexposuredat = FALSE
                           )
# tagtable0

## Step 4: As stated above.
