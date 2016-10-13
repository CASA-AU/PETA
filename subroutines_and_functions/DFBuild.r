# -------------------------------------------------------------------------------------------------------------------------------------------------------- 
# casa2_function_DFBuild - Data Frame Builder function                                                                                                             
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


## Building a dataframe of counts by analysis variable
fnDFBuild <- function(fVarlist = 'Occurence.Type.Description'
											 , fVarClass = 'chraracter'
											 , fDFList = lzOcc
											 , fRefID = NULL
                       , fexposuredat = NULL
											 ) {

	## Note: The function accounts for cases of analysing single and multiple fVar/s
	## The ordering of the fvar is important, as the data frame merging is done as a left join, where the first variable 
	## in the list becomes the left most data frame.

  stopifnot(fRefID != NULL)
  
	## local function for looking up names of the data frames in fDFList
	fnNamelkup <- function(lkupvar, x) {
		any(grepl(lkupvar, names(x)))
	}

	dfVarDFlkup <- NULL
	for (flp1 in 1:length(fVarlist)) {
		flp1var <- fVarlist[flp1]
		flp1cls <- fVarClass[flp1]
    
    ## tmp stores the name of the list element (in fDFList) that contains the specified variable (from fVarlist)
		tmp <- names(fDFList)[sapply(fDFList, FUN = fnNamelkup, lkupvar=flp1var)]
		
		## Force stop and error message if there were no matches or more than one match
    if (length(tmp) == 0) {
      stop(paste0('Variable ', flp1var
                  , ' was not found in any input data frames. Please ensure the field name is correct.'
                  )
           )
    } else if (length(tmp) > 1) {
			stop(paste0('Variable ', flp1var
									, ' was found in multiple input data frames. Please ensure the field names are unique across all the data being used.'
									)
					)
		}
		
		dfVarDFlkup <- rbind(dfVarDFlkup
												 , data.frame(Var = flp1var
																			, Class = flp1cls
																			, DFname = tmp
																			, stringsAsFactors=F
																			)
												 )
	}

	arDFnames <- unique(dfVarDFlkup$DFname)
	
	## Require separate process to handle:
	## a) single numeric variable
	## b) single non-numeric variable
	## c) combination of non-numeric and numeric variables
	## d) combination of non-numeric variables only
	
	## If statement to check whether there are more than one variable to analyse 
	if (length(fVarlist) == 1) {
		
		## Case a) or b)
		
		## if there is only one variable, then there must be only one data frame
		## Exclude duplicated occurrence IDs  from the data frame so we can get counts of distinct occurrences
    ##
    ## tmp is now a dataframe which holds the OccurrenceID, Year and Quarter, and chosen variable
		tmp <- unique(fDFList[[arDFnames]][, c(fRefID, 'yr_Q', fVarlist)])
		
		## Recode variable classes as indicated by the use input
		for (flp1 in 1:nrow(dfVarDFlkup)) {
			class(tmp[, dfVarDFlkup$Var[flp1]]) <- dfVarDFlkup$Class[flp1]
		}
		
		## Check the variable class and apply appropriate process 
		## If statement to check whether there is a numeric variable
		if (class(tmp[, fVarlist]) %in% c('numeric', 'integer')) {
		
			## Case a)
			datfrm <- aggregate(tmp[fVarlist]
													, by = list(yr_Q = tmp$yr_Q)
													, FUN = sum
													, na.rm=T
													)
			row.names(datfrm) <- datfrm$yr_Q
			datfrm <- datfrm[fVarlist]
		} else {
		
			## Case b)
			datfrm <- as.data.frame(unclass(table(tmp$yr_Q, tmp[, fVarlist])))
		}
		
	} else {
		
		## Case c) or d) - Check whether there is only one data frame to use
		if (length(arDFnames) == 1) {
			
			## No merging required.
			## Exclude duplicated occurrence IDs  from the data frame so we can get counts of distinct occurrences 			
			tmp <- unique(fDFList[[arDFnames]][, c(fRefID, 'yr_Q', fVarlist)])
			
		} else {
			
			## Mergind required - Loop through the array of data frame names and combine to single data frame
			
			## The merging will be done on fRefID - if any of the required data frame do not have this, merging 
			## cannot be done - If statement placed to conduct this check
			arDF_with_fRefID <- names(fDFList)[sapply(fDFList, FUN = fnNamelkup
																								, lkupvar=fRefID
																								)
																				]
			
			## Force stop in case the ifstatement returns TRUE
			if (!all(arDFnames %in% arDF_with_fRefID)) {
				stop(paste0('The reference ID: ', fRefID,  ' was not found in '
										, paste(arDFnames[!arDFnames %in% arDF_with_fRefID]
														, collapse = ' & '
														)
										, '.  Please revise the input data.'
										)
						)
			}
			
			tmp <- NULL
			for (flp1 in arDFnames) {
				
				flp1vars <- dfVarDFlkup$Var[dfVarDFlkup$DFname == flp1]
				flp1cls <- dfVarDFlkup$Class[dfVarDFlkup$DFname == flp1]
				
				## Exclude duplicated occurrence IDs  from the data frame so we can get counts of distinct occurrences 
				tmp0 <- unique(fDFList[[flp1]][, c(fRefID, 'yr_Q', flp1vars)])
				
				if (is.null(tmp)) {
					tmp <- tmp0
				} else {

					## Given the ordering of the variable determines the hierarchy of the variable grouping (e.g. desc then 
					## fatality counts or vice versa), so left join should work. - i.e. we don't want a row from data frame containing 
					## secondary variables where the occurrence ID do not exist in the primary varaible data set.
					
					tmp <- merge(tmp, tmp0
											 , by = c(fRefID, 'yr_Q')
											 , all.x = T
											 )

					## There may be some rows where one or more of the variable value is NA, 
					
				}
			}
		}
		
		## Recode variable classes as indicated by the use input
		for (flp1 in 1:nrow(dfVarDFlkup)) {
			class(tmp[, dfVarDFlkup$Var[flp1]]) <- dfVarDFlkup$Class[flp1]
		}
		
		## generate a table of counts for each value in the selected variable - bind it to "datfrm" as data frame object
		## If any of the variables are numeric, it has to be the last variable to collate
		if (any(mapply(tmp[, fVarlist], FUN = class) %in% c('numeric', 'integer'))) {
			
			## Case c)
			## collate all non-numeric variables into single string variable
			non_nums <- fVarlist[!mapply(tmp[, fVarlist], FUN = class) %in% c('numeric', 'integer')]
			
			## First get rid of any leading and trailling spaces
			tmp[, non_nums] <- apply(tmp[non_nums], MARGIN = 2
															 , FUN = gsub, pattern = ' *^| *$', replacement = ''
															 )
															 
			## Make single string variable - use "_._" (underscore, period, underscore) to express combination of variables
			tmp$x <- apply(tmp[non_nums], MARGI = 1, FUN = paste, collapse = '_._')
			
			## There must be only one numerical variable
			nums <- fVarlist[mapply(tmp[, fVarlist], FUN = class) %in% c('numeric', 'integer')]
			
			if (length(nums) > 1) {
				stop('Infeasible combination of variables to analyse (multiple numeric variables).  Please revise.')
			}
			
			## Aggregate the numeric values over yr_Q and combination of variable values
			datfrm <- aggregate(tmp[nums]
													, by = list(yr_Q = tmp$yr_Q
																			, x = tmp$x
																			)
													, FUN = sum
													, na.rm=T
													)
													
			datfrm <- reshape(datfrm, direction = 'wide'
												, idvar = 'yr_Q'
												, timevar = 'x', sep = '_._'
												)
												
			row.names(datfrm) <- datfrm$yr_Q
			
			## Drop yr_Q field - just so that the datfrm structure is consistent for all cases
			datfrm <- datfrm[, names(datfrm) != 'yr_Q']
		} else {
		
			## Case d)
			tmp$x <- apply(tmp[fVarlist], MARGIN = 1, FUN = paste, collapse = '_._')
			datfrm <- as.data.frame(unclass(table(tmp$yr_Q, tmp[, 'x'])))
		}
		
	}
	
	## Replace all NAs in datfrm with 0
	datfrm[is.na(datfrm)] <- 0
		
	## append necessary columns

	## save a list of unique values of the variable 
	arVarvals <- names(datfrm)

	## Year
	datfrm$year <- as.numeric(sub('_.*', '', row.names(datfrm)))

	## Quarter
	datfrm$quarter <- as.numeric(sub('.*_', '', row.names(datfrm)))

	## Linear Trend
	datfrm$trend <- 1:nrow(datfrm)

	## Flag for testing LastSpecial
	datfrm$lastspecial <- c(rep(0, nrow(datfrm) - 1), 1)

	## Seasonal terms for testing seasonality
	datfrm$sa <- sin(datfrm$trend/4 * 2*pi)
	datfrm$ca <- cos(datfrm$trend/4 * 2*pi)

  ## if supplied with exposure data, merge it on
	if(!(is.null(fexposuredat))){
    ## first check if any value is zero, can not take a log of zero
	  if (sum(fexposuredat[,"exposure"]==0) > 0) {
	    stop('Exposure can not take value of zero for any entry')
	  }
    ## now check the three required variables have been provided
	  if (!(sum(c("year", "quarter", "exposure") %in% colnames(fexposuredat)) == 3)) {
	    stop('Columns of exposure data must include: year, quarter, exposure')
	  }
    ## now merge by year and quarter
	  datfrm <- merge(datfrm, fexposuredat, by=c('year', 'quarter') )
	  datfrm$lexposure <- log(datfrm$exposure)
	}
    
  
	## Reorder columns
	arMetacols <- names(datfrm)[!names(datfrm) %in% arVarvals]
	datfrm <- datfrm[, c(arMetacols, arVarvals)]

	## Copy arVarvals, arMetacols and datfrm to global environment
	assign('arVarvals', arVarvals, envir=.GlobalEnv)
	assign('arMetacols', arMetacols, envir=.GlobalEnv)
	assign('datfrm', datfrm, envir=.GlobalEnv)
	assign('analysisvars', paste(fVarlist, collapse='_by_')
				 , envir=.GlobalEnv)

}

