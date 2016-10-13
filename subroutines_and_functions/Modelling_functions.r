# -------------------------------------------------------------------------------------------------------------------------------------------------------- #
# Collection of modelling fitting and selection algorithms for the trend analysis - "casa2_Modelling_functions.r"                #
# -------------------------------------------------------------------------------------------------------------------------------------------------------- #
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
#
# --------------------------------------------------------------------------------------------------------------------------------------------------------- #
# Glossary of standard model-related functions used:
# anova - Compute analysis of variance (or deviance) tables for one or more fitted model objects.
# glm -fit generalized linear models, must specify family
# formula - input for glm, states model formlua 
# pchisq - probabilities associated with Chi-Squared Distribution
# residuals - extracts model residuals from model objects 
# summary - produces result summaries of model objects
# update - updates and re-fits a model
# zeroinfl -  fits zero-inflated regression models for count data
# --------------------------------------------------------------------------------------------------------------------------------------------------------- #

##### Function: Test dispersion
### The dispersion parameter is the Pearson chisquare divided by the residual df.
### Since quasi-Poisson model calculates the dispersion parameter directly, 
### we can simply get the Pearson chisquare immediately as dispersion parameter * resid df
## inputs:  data and variables and exposure data if available
## outputs: pvalue (which if < 0.05 show overdispersion)
testdis <- function(dat, fvarname, fexposuredat){
  # Fit quasipoisson model (m1) and test dispersion
	if (fexposuredat==TRUE) {
		qp1 <- glm(paste0(fvarname, "~ trend + lastspecial + offset(lexposure)")
							 , family=quasipoisson(link="log")
							 , data=dat
		)
	} else {
    
	  qp1 <- glm(paste0(fvarname, "~ trend + lastspecial")
							 , family=quasipoisson(link="log")
							 , data=dat						 
		)  
	}
  
  Pearsonchisq <- summary(qp1)$dispersion * qp1$df.residual
  pval <- pchisq(Pearsonchisq,qp1$df.residual, lower.tail=FALSE)
  return(pval)
}

#### Function: Test underdispersion
### Underdispersion is flagged when that pval (lower tail) is less than 0.05;
### it might indicate a concern with respect to reporting of data 
### The idication is that the data is less variable than expected if random
## inputs:  data and variables and exposure data if available
## outputs: pvalue (which if < 0.05 show underdispersion)
testunderdis <- function(dat, fvarname, fexposuredat){
  # Fit quasipoisson model (m1) and test for underdispersion
  if (fexposuredat==TRUE) {
    qp1 <- glm(paste0(fvarname, "~ trend + lastspecial + offset(lexposure)")
               , family=quasipoisson(link="log")
               , data=dat
    ) 
  } else {
    qp1 <- glm(paste0(fvarname, "~ trend + lastspecial")
               , family=quasipoisson(link="log")
               , data=dat
    )  
  }

  Pearsonchisq <- summary(qp1)$dispersion * qp1$df.residual
  pval <- pchisq(Pearsonchisq,qp1$df.residual, lower.tail=TRUE)
  return(pval)
}

#### FUNCTION: fit Poisson models (m1-m4)
## inputs: data and variables being analysed and exposure data if available
## outputs: pmods which holds the glm summarys of M1-M4
poissonmods <- function(dat, fvarname, fexposuredat){
  # code for fitting the four Poisson models
  if (fexposuredat==TRUE) {
    p1 <- glm(paste0(fvarname, "~ trend + lastspecial + offset(lexposure)")
              , family=poisson(link="log")
              , data=dat
    )
  } else {
    p1 <- glm(paste0(fvarname, "~ trend + lastspecial")
              , family=poisson(link="log")
              , data=dat          
    )
  }
						
  ## m1 fitted with poisson dist
  p2 <- update(p1,.~. -trend) ## m2 fitted with poisson dist
  p3 <- update(p1,.~.-lastspecial) ## m3 fitted with poisson dist
  p4 <- update(p1,.~.-lastspecial -trend) ## m4 fitted with poisson dist
  
  return(list(p1=p1, p2=p2, p3=p3, p4=p4))
}

#### FUNCTION: fit quasi Poisson models (m1-m4)
## inputs: data and variables being analysed and exposure data if available
## outputs: qpmods which holds the glm summarys of M1-M4
qpoissonmods <- function(dat, fvarname, fexposuredat){
  # Fit quasipoisson models 
  if (fexposuredat==TRUE) {
    qp1 <- glm(paste0(fvarname, "~ trend + lastspecial + offset(lexposure)")
               , family=quasipoisson(link="log")
               , data=dat            
    )
  } else {
    qp1 <- glm(paste0(fvarname, "~ trend + lastspecial")
               , family=quasipoisson(link="log")
               , data=dat
    )
  }
 						 
  ## m1 fitted with quasipoisson dist
  qp2 <- update(qp1,.~.-trend) ## m2 fitted with quasipoisson dist
  qp3 <- update(qp1,.~. -lastspecial) ## m3 fitted with quasipoisson dist
  qp4 <- update(qp1,.~.-lastspecial -trend) ## m4 fitted with quasipoisson dist

  return(list(qp1=qp1, qp2=qp2, qp3=qp3, qp4=qp4))
}

#### FUNCTION: identify zero inflated data
### Model for counts: trend + lastspecial 
### Model for ZI: constant
### Flag is
### 1 - if evidence of zero inflated data
### 0 - if no evidence of zero inflated data
zeroinf <- function(dat, fvarname_for_glm, fexposuredat){
	## We need to ensure there is more than one zero in the first (n-1) points in dat
	## (lastspecial always fits the last point perfectly)
  if(min(dat[-nrow(dat), fvarname_for_glm])> 0) {
		zinfflag <- 0
	} else {
    # Note, the first part of the model formula (before |) relates to counts model;
    # second part of the formula (after |) relates to the ZI model.
	  if (fexposuredat==TRUE) {
	    zinflmod <- zeroinfl(as.formula(paste0(fvarname_for_glm, "~ trend + lastspecial + offset(lexposure) | 1"))
	                         , dist="poisson"
                           , link="logit"
                           , data=dat
	                    )
	  } else {
	    zinflmod <- zeroinfl(as.formula(paste0(fvarname_for_glm, "~ trend + lastspecial | 1"))
	                         , dist="poisson"
                           , link="logit"
                           , data=dat
                           )
	  }

	  
	  #  p-value for whether there is zero-inflation
	  zinflmodpval <- summary(zinflmod)$coef$zero["(Intercept)", "Pr(>|z|)"]  
    
    ### zero inf flag
    zinfflag <- ifelse(zinflmodpval < 0.05, 1, 0)
  }
  
  return(zinfflag)
}

#### FUNCTION: Count of the number of outliers
## inputs:  data, pmods and bestmod as the count relates to bestmod only
##          dist (as both quassi-poisson and poisson variables use the same function)
##          fexposuredat, where exposure data exists
## outputs: outcount_output is the count of the number of outliers detected
## note: This flag only flags large outliers, it may be worthwhile reviewing if it does no perform well in practice
## note: bestform has exposure (or not), hence no explicit mention here
outcount <- function(dat, allmods, bestmod, pdist="poisson") {
  bestlist <- allmods[[bestmod]]
  
  ## extract the residuals and the sqrt of the dispersion
  res <- residuals(allmods[[bestmod]], type="deviance")
  sqrtdis <- sqrt(summary(allmods[[bestmod]])$dispersion)
  
  ## This will only flag quite gross outliers
  outliercount <- sum(res/sqrtdis > 3)
  
  return(outliercount)
}

#### FUNCTION: last special and trend flags - poisson models
## inputs: pmods (which holds the glm summarys of M1-M4)
## outputs:  last special and trend flags, anovatab, and the best fitting model
lstrendflags <- function(pmods) {
  ## likelihood ratio tests for the poisson models
  anovatab <- anova(pmods$p1, pmods$p2, pmods$p4, pmods$p3, pmods$p1
										, test="LR"
										)
  
	## Added 20140529 - fix for the missing p-val due to negligible change in deviance
	## Look for any missing p-value - the first row can be skipped
	if (any(is.na(anovatab[2:5, "Pr(>Chi)"]))) {
		## Which row has NA?
		NARows <- which(is.na(anovatab[2:5, "Pr(>Chi)"]))
	 
		## Loop through NA row/s and fix p-value if due to machine error at zero change in deviance
		## Note flp1 range from 1 to 4 but anovatab has 5 rows, therefore [flp1 + 1] look up is used
		for (flp1 in NARows) {
			
			if (abs(anovatab$"Deviance"[flp1+1]) < 1e-10) {
				## Very small change in deviance - must have a wrong sign to give NA p-value
				anovatab$"Pr(>Chi)"[flp1+1] <- 1
			} else {
				stop(cat('Missing p-value in ANOVA table for m1 to m4.\n'
								 , 'Check', svar, 'in', fanalysisvar
								 , '\n'
								 )
						 )
			}
		}
	}
	
	
  ## use backwards elimination for automated model selection
  ## Lattice diagram
  ##       m1 trend + lastspecial
  ##             /                \ 
  ##          /                      \ 
  ##       /                           \ 
  ##  m2 lastspecial       m3 trend
  ##       \                            /
  ##         \                       /
  ##            \                 /
  ##            m4 constant
  ##

  ######################################################################
	## If Version of the model selection algorithm - With this many if statements, ifelse will not be my first choice of 
	## algorithm coding.
	if (anovatab[5, "Pr(>Chi)"]<0.05) { 
		
		#  m1 beats m3, next test m1 vs m2
		if (anovatab[2, "Pr(>Chi)"]<0.05) { 
			#  m1 beats m2, choose m1
			modsel <- "p1"
		} else { # Otherwise  m2 is OK, test whether m4 is just as good
			if (anovatab[3, "Pr(>Chi)"]<0.05) {
				# m2 beats m4, choose m2
				modsel <- "p2"
			} else { # Otherwise, m4 is OK, choose m4, *** even though m1 vs m3 was signif, but we have m1 to m2 to m4 not signif
			         # (We COULD say record m1 as a contender and add to plot, but will not do this for now.)
					 # (Could even be more sophisticated and choose between m1 and m4 on basis of test between m1 and m4 on 2 df.)
				modsel <- "p4"  
			}
		}
		
	} else {  # m3 as good as m1, next test m1 vs m2
		
		if (anovatab[2, "Pr(>Chi)"]<0.05) {
		
			# m1 beats m2, next test m3 vs m4
			if (anovatab[4, "Pr(>Chi)"]<0.05) {
				# m3 beats m4, choose m3
				modsel <- "p3"
			} else {  # Otherwise  m4 as good as m3, choose m4, *** even though m1 vs m2 was signif, but we have m1 to m3 to m4 not signif
				modsel <- "p4"
			}
			
		} else { # Otherwise either of m2 or m3 is OK; both lastspecial and trend are (individually) NS in m1; 
			
			#which term appears "least significant" and should be dropped first?
			# We COULD record the other of m2 and m3 as a contender and include on plot, but we will not do this for now.
			if (anovatab[5, "Pr(>Chi)"] > anovatab[2, "Pr(>Chi)"]) {
			
				# m3 "better" than m2; proceed from m3 and test m3 vs m4
				if (anovatab[4, "Pr(>Chi)"]<0.05) {
					# m3 beats m4, choose m3
					modsel <- "p3"
				} else {
					# m4 as good as m3, choose m4
					modsel <- "p4"
				}
				
			} else {  # Otherwise  m2 "better" than m3; proceed from m2 and test m2 vs m4
				if (anovatab[3, "Pr(>Chi)"]<0.05) {
					# m2 beats m4, choose m2
					modsel <- "p2"
				} else {
					# m4 as good as m2, choose m4
					modsel <- "p4"
				}
			}
		}
	}
  ######################################################################	
	
  ## set the last special  (lsflag) and trend flags (trendflag) 
  ## based on the selected model, sign of coeff, effect size 
	if (modsel=="p1"){
    lsflag <- ifelse(summary(pmods$p1)$coefficients["lastspecial", "Estimate"]>0, 
                     ifelse(anovatab[5, "Pr(>Chi)"]<0.01, 1, 2),
                     ifelse(anovatab[5, "Pr(>Chi)"]<0.01, 5, 4))
    trendflag <- ifelse(summary(pmods$p1)$coefficients["trend","Estimate"]>0, "up", "down")
  }
	
	if (modsel=="p2") {
		lsflag <- ifelse(summary(pmods$p2)$coefficients["lastspecial", "Estimate"]>0, 
										 ifelse(anovatab[3, "Pr(>Chi)"]<0.01, 1, 2),
										 ifelse(anovatab[3, "Pr(>Chi)"]<0.01, 5, 4))
		trendflag <- "-"
	}

	if (modsel=="p3") {
		lsflag <- 3
		trendflag <- ifelse(summary(pmods$p1)$coefficients["trend","Estimate"]>0, "up", "down")
	} 
	
	if (modsel=="p4") {
		lsflag <- 3
		trendflag <- ""
	}
  
  return(list(lsflag=lsflag, trendflag=trendflag, anovatab=anovatab, modsel=modsel))
}

#### FUNCTION: last special and trend flags - quasi poisson models
## inputs: fqpmods (which holds the glm summarys of M1-M4)
## outputs:  last special and trend flags, anovatab, and the best fitting model
qlstrendflags <- function(fqpmods, svar, varname){
  
  ##  Logic is the same as lstrendflags using anovatab and Chisq tests, but here use anovatabq and F-tests
  ## F tests for the quasipoisson models
  anovatabq <- anova(fqpmods$qp1, fqpmods$qp2, fqpmods$qp4, fqpmods$qp3
										 , fqpmods$qp1, test="F"
										 )

	## Added 20140529 - fix for the missing p-val due to negligible change in deviance
	## Look for any missing p-value - the first row can be skipped
	if (any(is.na(anovatabq[2:5, "Pr(>F)"]))) {
		## Which row has NA?
		NARows <- which(is.na(anovatabq[2:5, "Pr(>F)"]))
	 
		## Loop through NA row/s and fix p-value if due to machine error at zero change in deviance
		## Note flp1 range from 1 to 4 but anovatabq has 5 rows, therefore [flp1 + 1] look up is used
		for (flp1 in NARows) {
			
			if (abs(anovatabq$"Deviance"[flp1+1]) < 1e-10) {
				## Very small change in deviance - must have a wrong sign to give NA p-value
				anovatabq$"Pr(>F)"[flp1+1] <- 1
			} else {
				stop(cat('Missing p-value in ANOVA table for m1 to m4.\n'
								 , 'Check', svar, 'in', varname
								 , '\n'
								 )
						 )
			}
		}
	}

  ## Select the "best" model out of the basic four in the lattice
  ##
  ##                   m1 trend + lastspecial
  ##                       /                          \ 
  ##                row2                        row5             F tests in anovatabq[ rownum , "Pr(>F)" ] 
  ##                 /                                      \ 
  ##    m2 lastspecial                       m3 trend
  ##                 \                                      /
  ##               row3                           row4           F tests in anovatabq[ rownum , "Pr(>F)" ] 
  ##                       \                          /
  ##                           m4 constant
  ##

	if (anovatabq[5, "Pr(>F)"]<0.05) { 
		
		#  m1 beats m3, next test m1 vs m2
		if (anovatabq[2, "Pr(>F)"]<0.05) { 
			#  m1 beats m2, choose m1
			modsel <- "qp1"
		} else { # Otherwise  m2 is OK, test whether m4 is just as good
			if (anovatabq[3, "Pr(>F)"]<0.05) {
				# m2 beats m4, choose m2
				modsel <- "qp2"
			} else { # Otherwise, m4 is OK, choose m4, *** even though m1 vs m3 was signif, but we have m1 to m2 to m4 not signif
				modsel <- "qp4"  
			}
		}
		
	} else {  # m3 as good as m1, next test m3 vs m4
		
		if (anovatabq[2, "Pr(>F)"]<0.05) {
		
			# m1 beats m2, next test m3 vs m4
			if (anovatabq[4, "Pr(>F)"]<0.05) {
				# m3 beats m4, choose m3
				modsel <- "qp3"
			} else {  # Otherwise  m4 as good as m3, choose m4, *** even though m1 vs m2 was signif, but we have m1 to m3 to m4 not signif
				modsel <- "qp4"
			}
			
		} else { # Otherwise either of m2 or m3 is OK; both lastspecial and trend are (individually) NS in m1; 
			
			#which term appears "least significant" and should be dropped first?
			if (anovatabq[5, "Pr(>F)"] > anovatabq[2, "Pr(>F)"]) {
			
				# m3 "better" than m2; proceed from m3 and test m3 vs m4
				if (anovatabq[4, "Pr(>F)"]<0.05) {
					# m3 beats m4, choose m3
					modsel <- "qp3"
				} else {
					# m4 as good as m3, choose m4
					modsel <- "qp4"
				}
				
			} else {  # Otherwise  m2 "better" than m3; proceed from m2 and test m2 vs m4
				if (anovatabq[3, "Pr(>F)"]<0.05) {
					# m2 beats m4, choose m2
					modsel <- "qp2"
				} else {
					# m4 as good as m2, choose m4
					modsel <- "qp4"
				}
			}
		}
	}

  ## set the last special and trend flags based on the selected model
  if (modsel=="qp1") {
    lsflag <- ifelse(summary(fqpmods$qp1)$coefficients["lastspecial", "Estimate"]>0
                     , ifelse(anovatabq[5, "Pr(>F)"]<0.01, 1, 2)
                     , ifelse(anovatabq[5, "Pr(>F)"]<0.01, 5, 4)
										 )
    trendflag <- ifelse(summary(fqpmods$qp1)$coefficients["trend", "Estimate"]>0
												, "up", "down"
												)
  }
  if (modsel=="qp2") {
    lsflag <- ifelse(summary(fqpmods$qp2)$coefficients["lastspecial", "Estimate"]>0
                     , ifelse(anovatabq[3, "Pr(>F)"]<0.01, 1, 2)
                     , ifelse(anovatabq[3, "Pr(>F)"]<0.01, 5, 4)
										 )
    trendflag <- "-"
  }
  if (modsel=="qp3") {
    lsflag <- 3
    trendflag <- ifelse(summary(fqpmods$qp1)$coefficients["trend", "Estimate"]>0
												, "up", "down"
												)
  }
  if (modsel == "qp4") {
    lsflag <- 3
    trendflag <- ""
  }

  return(list(lsflag=lsflag
							, trendflag=trendflag
							, anovatabq=anovatabq
							, modsel=modsel
							)
				)
}

##        m1 trend + lastspecial
##        /         \ 
##      row2        row5          F tests in anovatabq[ rownum , "Pr(>F)" ] 
##      /             \ 
##    m2 lastspecial   m3 trend
##      \             /
##      row3       row4           F tests in anovatabq[ rownum , "Pr(>F)" ] 
##        \         /
##        m4 constant

### FUNCTION: last qtr changes pattern flag, second to last special
## This will flag if
## - the model is different to that fitted without the last quarter of data
## - both the model with and without the last quarter has last special
## codes for model changes
## 1 - excluding the last quarter is m1, including the last quarter is m1
## 2 - excluding the last quarter is m1, including the last quarter is m2
## 3 - excluding the last quarter is m1, including the last quarter is m3
## 4 - excluding the last quarter is m1, including the last quarter is m4
## 5 - excluding the last quarter is m2, including the last quarter is m1
## 6 - excluding the last quarter is m2, including the last quarter is m2
## 7 - excluding the last quarter is m2, including the last quarter is m3
## 8 - excluding the last quarter is m2, including the last quarter is m4
## 9 - excluding the last quarter is m3, including the last quarter is m1
## 10 - excluding the last quarter is m3, including the last quarter is m2
## 11 - excluding the last quarter is m3, including the last quarter is m3
## 12 - excluding the last quarter is m3, including the last quarter is m4
## 13 - excluding the last quarter is m4, including the last quarter is m1
## 14 - excluding the last quarter is m4, including the last quarter is m2
## 15 - excluding the last quarter is m4, including the last quarter is m3
## 16 - excluding the last quarter is m4, including the last quarter is m4

## In the excel template lookup
# Last Quarter Changes Pattern	Last Quarter Changes Pattern
# 1	                                                 Had Trend + LS
# 2	                                                 Had Trend + LS
# 3	                                                 Had Trend + LS
# 4	                                                 Had Trend + LS
# 5	                                                 Had LS
# 6	                                                 Had LS
# 7	                                                 Had LS
# 8	                                                 Had LS
# 9	                                                 
# 10                                                   Had Trend
# 11                                                    
# 12                                                   Had Trend
# 13                                                   Was Constant
# 14                                                   
# 15                                                   Was Constant
# 16                                                   

lqtrchange <- function(dat, fvarname, pdist, lstrendflagsmods, fexposuredat){
  ## trim the last qtr of data (includes exposure data if available)
  dat_nolqtr <- dat[-dim(dat)[1],]
  dat_nolqtr$lastspecial[nrow(dat_nolqtr)] <- 1
  
  ## fit models and get the flags for the dat with the last quarter removed
  ## lstrendflagssl has the flags that identify if last special or trend
  ## were in the model without the last quarter of data
  if (pdist=="poisson"){
    pmods_secondlast <- poissonmods(dat_nolqtr
                                      , fvarname
                                      , fexposuredat=fexposuredat
                                    )
    
    lstrendflagssl <- lstrendflags(pmods_secondlast)
    ## investigating if the last quarter changes the pattern
    lastqrtchangflag <- ifelse(lstrendflagssl$modsel == "p1"
           #if true (i.e. if we exclude the last quarter we get trend and ls)
					 # we next look at model including last quarter
           , ifelse(lstrendflagsmods$modsel == "p1", 1
										, ifelse(lstrendflagsmods$modsel == "p2", 2
														 , ifelse(lstrendflagsmods$modsel == "p3", 3, 4)
														)
									 )
					 #if false (i.e. if we exclude the last quarter we don't get both trend and ls)
           , ifelse(lstrendflagssl$modsel == "p2"
										#if true (i.e. if we exclude the last quarter we get ls but no trend)
										# we next look at model including last quarter
										, ifelse(lstrendflagsmods$modsel == "p1", 5
														 , ifelse(lstrendflagsmods$modsel == "p2", 6
																			, ifelse(lstrendflagsmods$modsel == "p3", 7, 8)
																			)
														)
										#if false
										, ifelse(lstrendflagssl$modsel == "p3"
														 #if true (i.e. if we exclude the last quarter we get trend but no ls)
														 # we next look at model including last quarter
														 , ifelse(lstrendflagsmods$modsel == "p1", 9
																			, ifelse(lstrendflagsmods$modsel == "p2", 10
																							 , ifelse(lstrendflagsmods$modsel == "p3", 11, 12)
																							 )
																			)
														 #if false (i.e. if we exclude the last quarter we get no trend and no ls)
														 # we next look at model including last quarter
														 , ifelse(lstrendflagsmods$modsel == "p1", 13
																			, ifelse(lstrendflagsmods$modsel == "p2", 14
																							 , ifelse(lstrendflagsmods$modsel == "p3", 15, 16)
																							 )
																			)
														)
										)
		)
		
		## Make generic second last special model object for plotting
		mod_secondlast <- pmods_secondlast[[lstrendflagssl$modsel]]
		
  }
  if (pdist=="quasipoisson"){
    qpmods_secondlast <- qpoissonmods(dat_nolqtr
                                      , fvarname
                                      , fexposuredat
                                      )
    lstrendflagssl <- qlstrendflags(qpmods_secondlast) 
    ## investigating if the last quarter changes the pattern
    lastqrtchangflag <- ifelse(lstrendflagssl$modsel == "qp1"
           #if true
           , ifelse(lstrendflagsmods$modsel == "qp1", 1
										, ifelse(lstrendflagsmods$modsel == "qp2", 2
														 , ifelse(lstrendflagsmods$modsel == "qp3", 3, 4)
														 )
										)
           #if false
           , ifelse(lstrendflagssl$modsel == "qp2"
										#if true
										, ifelse(lstrendflagsmods$modsel == "qp1", 5
														 , ifelse(lstrendflagsmods$modsel == "qp2", 6
																			, ifelse(lstrendflagsmods$modsel == "qp3", 7, 8)
																			)
														)
										#if false
										, ifelse(lstrendflagssl$modsel == "qp3"
														 #if true
														 , ifelse(lstrendflagsmods$modsel == "qp1", 9
																			, ifelse(lstrendflagsmods$modsel == "qp2", 10
																							 , ifelse(lstrendflagsmods$modsel == "qp3", 11, 12)
																							 )
																			)
														 #if false
														 , ifelse(lstrendflagsmods$modsel == "qp1", 13
																			, ifelse(lstrendflagsmods$modsel == "qp2", 14
																							 , ifelse(lstrendflagsmods$modsel == "qp3", 15, 16)
																							 )
																			)
														)
										)
		)
		
		## Make generic second last special model object for plotting
		mod_secondlast <- qpmods_secondlast[[lstrendflagssl$modsel]]
		
  }
  
  SecondLastSpecial <- lstrendflagssl$lsflag
	
  return(list(lastqrtchangflag=lastqrtchangflag
							, SecondLastSpecial=SecondLastSpecial
							, mod_secondlast = mod_secondlast
							)
				)
}

### FUNCTION: change in level 
## inputs:  data and variables
##          dist (as both quassi-poisson and poisson variables use the same function)
## outputs:  pvalue and model object
### FUNCTION: change in level 
bp <- function(dat, fvarname, pdist, allmods, fexposuredat){
  ## list the candidate breakpoints
  candidatebps <- seq(1.5, dim(dat)[1]-0.5, by=1)
  
  ntries <- length(candidatebps)
  tries <- data.frame(bps=candidatebps)
  
  for(lpbps in candidatebps){
    if (fexposuredat==TRUE) {
      m5 <- glm(paste0(fvarname, "~I(dat$trend>", lpbps, ") + offset(lexposure)")
                , family=pdist
                , data=dat
                )
    } else {
      m5 <- glm(paste0(fvarname, "~I(dat$trend>", lpbps, ")")
                , family=pdist
                , data=dat
                )
    }
    # summary(p4changelevels)
    # lines(qdat$trend,p4changelevels$fitted.values, col=4,lty=4)
    tries$deviance[tries$bps == lpbps] <- m5$deviance
    tries$df.residual[tries$bps == lpbps] <- m5$df.residual
  }
  
  # This is the breakpoint with smallest residual deviance for model with a change in level at that breakpoint.
  # Selects one if two equal to minimum (in
  m5.bps <- tries$bps[order(tries$deviance)[1]]
  
  # Refit the optimal  model:
  # m5.wrongdf <- glm(paste0(fvarname, "~I(dat$trend>m5.bps)")
  # , family=pdist, data=dat
  # )
  if (fexposuredat==TRUE) {
    m5 <- glm(paste0(fvarname, "~I(dat$trend>", m5.bps, ") + offset(lexposure)")
              , family=pdist
              , data=dat
    )
  } else {
    m5 <- glm(paste0(fvarname, "~I(dat$trend>", m5.bps, ")")
              , family=pdist
              , data=dat
    )
  }
  
  ## For our purposes, the residual df for the model really should be 1 smaller, because we have estimated, 
  ## from the same data, the breakpoint parameter that is a crucial part of the model. 
  ## I.e. this model really has TWO more parameters than the null model,
  ## namely where the breakpoint is, and the changed level.
  ## Make this change manually:
  m5$df.residual <- m5$df.residual - 1
  ## also need to manually change AIC.
  m5$aic <- m5$aic + 2
  
  ## prepare m2 - m2 is nested within m5 therefore appropriate basis to compare with LRT
  m2 <- ifelse(pdist=="poisson", "p2", "qp2")
  ## set test types
  colvar <- ifelse(pdist=="poisson", "Pr(>Chi)", "Pr(>F)")
  testtype <- ifelse(pdist=="poisson", "Chisq", "F")
  ## run an anova
  anovabp <- anova(m5, allmods[[m2]], test=testtype)
  
  ## extract the p-vlaue
  bppval <- anovabp[2, colvar]
  
  return(list(bppval=bppval, m5=m5)) 
  
}



# FUNCTION: check for nonlinearity
## inputs:  data and variables
##          dist (as both quassi-poisson and poisson variables use the same function)
## outputs: p-value of the nonlinear term
##          shape either "concave", "convex"
##          m6 model object
nonlin <- function(dat, fvarname, pdist, fexposuredat){
  if (fexposuredat==TRUE) {
    linmod <- glm(paste0(fvarname, "~  + trend + offset(lexposure)")
                  , family=pdist
                  , data=dat
                  )
  } else {
    linmod <- glm(paste0(fvarname, "~  + trend")
                  , family=pdist
                  , data=dat
                  )
    
  }
  
  ##add a quadratic for a simple test of non-linearity
  m6 <- update(linmod,.~.+I(trend^2))
  
  ## run an anova and report the pval
  test <- ifelse(pdist=="poisson", "LR", "F")
  nonlinanova <- anova(linmod, m6, test=test)
  colvar <- ifelse(pdist=="poisson", "Pr(>Chi)", "Pr(>F)")
  nonlinpval <- nonlinanova[2,colvar]
  
  ## get the sign
  shape <- ifelse(m6$coefficients["I(trend^2)"]<0, "concave", "convex")
  
  return(list(nonlinpval=nonlinpval, shape=shape, m6=m6))
}

### FUNCTION: investigate seasonality
### Note, assumes that the seasonal terms are added during data prep
### Add seasonal terms to m1-m4 using update
## inputs:  data and pmods (as the models are formed using update)
##          dist (as both quassi-poisson and poisson variables use the same function)
## outputs: a p-value
##          model objects m7-m10
seasonal <- function(dat, allmods, pdist){
  
  ## get the models m1-m4
  m1 <- ifelse(pdist=="poisson", "p1", "qp1")
  m2 <- ifelse(pdist=="poisson", "p2", "qp2")
  m3 <- ifelse(pdist=="poisson", "p3", "qp3")
  m4 <- ifelse(pdist=="poisson", "p4", "qp4")
  
  ## setup for extracting the pvalues
  colvar <- ifelse(pdist=="poisson", "Pr(>Chi)", "Pr(>F)")
  test <- ifelse(pdist=="poisson", "LR", "F")
  
  ## m10 is m4 + seasonal terms
  m10 <- update(allmods[[m4]],.~. +sa+ca)
  # Test seasonality terms: by anova()
  anova_m10_m4 <- anova(m10,allmods[[m4]],test=test)
  seaspval1 <- anova_m10_m4[2, colvar]
  
  ## m9 is m3 + seasonal terms
  m9 <- update(allmods[[m3]],.~. +sa+ca)
  # Test seasonality terms: by anova()
  anova_m9_m3 <- anova(m9,allmods[[m3]],test=test)
  seaspval2 <- anova_m9_m3[2, colvar]
  
  ## m8 is m2 + seasonal terms
  m8 <- update(allmods[[m2]],.~. +sa+ca)
  # Test seasonality terms: by anova()
  anova_m8_m2 <- anova(m8,allmods[[m2]],test=test)
  seaspval3 <- anova_m8_m2[2, colvar]
  
  ## m7 is m1 + seasonal terms
  m7 <- update(allmods[[m1]],.~. +sa+ca)
  # Test seasonality terms: by anova()
  anova_m7_m1 <- anova(m7,allmods[[m1]],test=test)
  seaspval4 <- anova_m7_m1[2, colvar]
  

  ## extract pval using colvar - na.rm = T inserted to account for a very rare case where the change in deviance was
	## essentially zero but machine error put it on the wrong side of zero, yielding an NA p-value from the anova.
  seaspval <- min(seaspval1, seaspval2, seaspval3, seaspval4, na.rm=T)
  
  

	seasmod_list <- c('m10', 'm9', 'm8', 'm7')
	pval_list <- c(seaspval1, seaspval2, seaspval3, seaspval4)
	bestseasmod <- seasmod_list[max(which(pval_list == seaspval))]
	
  return(list(seaspval=seaspval, bestseasmod=bestseasmod
							, m10=m10, m9=m9, m8=m8, m7=m7
							)
				 )
}

## only investigate if the dispersion changed if quasipoisson models were fitted
## dispersion can be changed (eliminated) by one of three methods
## 1) Including a breakpoint
## 2) Allowing for non-linearity
## 3) Allowing for seasonality
## inputs: data, varalbes, and all the models
## outputs: if dispersion is reduced a code (up to three digits) containing the information on the reduction
##      1 - dispersion reduced by m5, change in level
##      2 - dispersion reduced by m6, nonlinearity
##      2 - dispersion reduced by m7-m10, seasonality
dischanged <- function(dat, varname, allmods, m5, m6, m7, m8, m9, m10){
  
  ## 1) Including a breakpoint (m5, which has the df adjusted)
  Pearsonchisq2 <- summary(m5)$dispersion * m5$df.residual
  pval1 <- pchisq(Pearsonchisq2,m5$df.residual, lower.tail=FALSE)
  
  ## 2) Allowing for non-linearity (m6)
  Pearsonchisq2 <- summary(m6)$dispersion * m6$df.residual
  pval2 <- pchisq(Pearsonchisq2,m6$df.residual, lower.tail=FALSE)
 
  ## 3) Allowing for seasonality
  Pearsonchisq2 <- summary(m7)$dispersion * m7$df.residual
  pval3a <- pchisq(Pearsonchisq2,m7$df.residual, lower.tail=FALSE)
  
  Pearsonchisq2 <- summary(m8)$dispersion * m8$df.residual
  pval3b <- pchisq(Pearsonchisq2,m8$df.residual, lower.tail=FALSE)
  
  Pearsonchisq2 <- summary(m9)$dispersion * m9$df.residual
  pval3c <- pchisq(Pearsonchisq2,m9$df.residual, lower.tail=FALSE)
  
  Pearsonchisq2 <- summary(m10)$dispersion * m10$df.residual
  pval3d <- pchisq(Pearsonchisq2,m10$df.residual, lower.tail=FALSE)
  
  ## see which pvalues (if any are not significant)
  dischange1 <- ifelse(pval1>0.05, 1, "")
  dischange2 <- ifelse(pval2>0.05, 2, "")
  dischange3 <- ifelse(max(pval3a, pval3b, pval3c, pval3d)>0.05, 3, "")
  
  dischangeflag <- paste0(dischange1, dischange2, dischange3)
  
  return(dischangeflag)
}




