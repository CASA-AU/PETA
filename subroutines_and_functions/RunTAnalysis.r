# -------------------------------------------------------------------------------------------------------------------------------------------------------- #
# Trend analysis modelling and collation of tags - "casa2_function_RunTAnalysis.r"                                                                 #
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
# -------------------------------------------------------------------------------------------------------------------------------------------------------- #
# Glossary of model fitting and selection algorithm functions  
#    testdis - tests for dispersion using the poisson distribution    
#    zeroinf - test for an excess of zeros using zeroinf from the pscl package
#    lqtrchange - refits the base models (M1-M4) with the last observation dropped, determines the associated flags   
#    bp -   testing for a change in level (breakpoint), which is model M5
#    nonlin -  testing for nonlinearity, which is model M6  
#    seasonal -  testing for seasonality, which is model M7-M10
#    outcount - counts the number of outliers (extreme conditions so flags do need investigation)
#   
#    Poisson model functions
#        testunderdisp  - tests for underdispersion using the poisson distribution  
#        poissonmods  - fits the base models (M1-M4) 
#        lstrendflags - reports the best fitting model of M1-M4 and last special flag and trend flag
#    quasi-Poisson model function
#        qpoissonmods - fits the base models (M1-M4) using quasi-poisson models
#        qlstrendflags  - reports the best fitting model of M1-M4 and last special flag and trend flag                                                                #
#        dischanged - investigates if any of M5-M10 reduce the dispersion
#                                                                                        #
# -------------------------------------------------------------------------------------------------------------------------------------------------------- #

fnRunTAnalysis <- function(dat ## dataset with variables of interest
                           , varname ## variable that can be analysised 
                           , fOutdir=NA ## user defined outputs directory (no default)
                           , fanalysisvar='' ## variable to be analysised this run (y variable in glm)
                           , fexposuredat=FALSE ## default exposuredat to no (ie not exposure data provided)
                           , MinCountThres = 20 ## the sum of the counts must be above this value
                           , NumZeroTol = 0.18 ## the proportion of values that must be non-zero
                           , plotOutput = FALSE
                           ) {
  
  # tagtable <- list()
  tagtable <- NULL
  
  for (svar in varname) { ## for each s(pecfic) var in varname
    
    if (grepl('^_._', svar)) {
      ## GLM cannot take _._ as a valid left side of the formula
      cat('\nCombination of values where the first is blank (i.e.'
          , paste0('"', svar, '")'), 'cannot be modelled.'
          )
    } else {
      
      ## if svar starts with a number, as.formula breaks.  Quote it with backticks
      if (grepl('^\\d', svar)) {
        svar_for_glm <- paste0('`', svar, '`')
      } else {
        svar_for_glm <- svar
      }
      
      ## Basic details for svar
      AverageCount <- mean(dat[,svar])
      LastCount <-  dat[dim(dat)[1], svar]
      
      ## Model fitted
      AllZero <- sum(dat[, svar]==0)==dim(dat)[1]
      ## zeroinfl fails not only when the counts are all zeros, but when there are too many zeros.
      ## seems like we need at least 15% of the data to be non zero. Set this tolerance to 18% for some buffer
      EnoughNonZeros <- ifelse(sum(dat[, svar]==0) > 
                               round(nrow(dat) * (1 - NumZeroTol), 0)
                             , 0, 1
                             )
      
      OverallCounts <- sum(dat[, svar])
      # EnoughCounts <- 1 ## Need actual decision making criteria
      EnoughCounts <- ifelse(OverallCounts >= MinCountThres, 1, 0) ## Need actual decision making criteria
      
      ## Fit Flag
      ## 1 - model not fitted, all counts zero
      ## 2 - model not fitted, counts below threshold and too many zeros    
      ## 3 - model not fitted, counts below threshold 
      ## 4 - model not fitted, too many zeros
      ## 5 - model fitted
      FitFlag <- ifelse(AllZero==T, 1, 
                        ifelse(EnoughCounts * EnoughNonZeros==1, 5, 
                               ifelse(EnoughCounts==0 & EnoughNonZeros==0, 2,
                                      ifelse(EnoughCounts==0 & EnoughNonZeros==1, 3, 4)
                        )))
      
      if (FitFlag %in% c(1, 2, 3, 4)) {
        
        tmptable <-  data.frame(
          AnalysisVariable            = fanalysisvar
          , SpecificVariable          = svar
          , AverageCount              = AverageCount 
          , LastCount                 = LastCount
          , ModelFitted               = FitFlag
          , Exposure                  = ""
          , Trend                     = ""
          , LastSpecial               = ""
          , SecondLastSpecial         = ""
          , LastQuarterChangesPattern = ""
          , Underdispersion           = ""
          , Overdispersion            = ""
          , ExcessZeros               = ""
          , Outliers                  = ""
          , ChangeInLevel             = ""
          , Nonlinearity              = ""
          , Seasonality               = ""
          , OverdispersionRemoved     = ""
          , stringsAsFactors = FALSE
        )
        
        if (is.null(tagtable)) {
          tagtable <- tmptable
        } else {
          tagtable <- rbind(tagtable, tmptable)
        }
      
      } else {
      
        ExposureFlag <- 0
        ## Exposure included
        if(fexposuredat==TRUE){
          ExposureFlag <- 1
#           dat$lexposure <- log(dat[,"exposure"])
        }
        
        ### Overdispersion flag
        ## inputs: data and variables being analysed
        ## outputs: overdisflag=1 means there is evidence of Overdispersion
        ##          overdisflag=0 means there is no evidence of Overdispersion
        dis <- testdis(dat, svar_for_glm, fexposuredat)  ## gets pvalue  
        overdisflag <- ifelse(dis < 0.05, 1, 0) 
        
        ## zero inflation flag
        ## inputs: data and variables being analysed
        ## outputs: zflag=1 means there is evidence of zero inflation
        ##          zflag=0 means there is no evidence of zero inflation
        ## note: zero-inflated models only for poisson 
        zflag <- zeroinf(dat, svar_for_glm, fexposuredat)

        if (overdisflag==0){
          ### Underdispersion flag
          ## inputs: data and variables being analysed
          ## outputs: underdisflag=1 means there is evidence of underdispersion
          ##          underdisflag=0 means there is no evidence of underdispersion
          udis <- testunderdis(dat, svar_for_glm, fexposuredat)  ## gets pvalue  
          underdisflag <- ifelse(udis < 0.05, 1, 0) ## convert to flag
          
          ## fit the Poisson models (m1-m4) and returns a list of glm objects
          ## inputs: data and variables being analysed
          ## outputs: pmods which holds the glm summarys of M1-M4
          pmods <- poissonmods(dat, svar_for_glm, fexposuredat)
          
          ## get the trend, last special (1-5), anova table, and model selected
          ## inputs: pmods (which holds the glm summarys of M1-M4)
          ## outputs:  last special and trend flags, and the best fitting model
          lstrendflagsmods <- lstrendflags(pmods)          
          ## extract the model selected and call it best model
          bestmod <- lstrendflagsmods$modsel
          
          ## refit the models with the last observation dropped 
          ## inputs:  data and variables
          ##          need lstrendflagsmods as inputs as it needs to compare models with and without the last point
          ## outputs:  SecondLastSpecial - which is last special with the last observation removed
          ##          lastqrtchangflag - a number from 1-16 saying which pattern is was and is now
          lqtrchange_output <- lqtrchange(dat, svar_for_glm, pdist="poisson", lstrendflagsmods, fexposuredat)
          lqtrchangeflag <- lqtrchange_output$lastqrtchangflag
          SecondLastSpecial <- lqtrchange_output$SecondLastSpecial
          
          ## see if a model with a breakpoint (m5) fits well
          ## inputs:  data and variables
          ##          dist (as both quassi-poisson and poisson variables use the same function)
          ## outputs:  SecondLastSpecial - which is last special with the last observation removed
          ##          lastqrtchangflag - a number from 1-16 saying which pattern is was and is now
          bpout <- bp(dat, fvarname=svar_for_glm, pdist="poisson", allmods=pmods, fexposuredat)
          bpflag <- ifelse(bpout$bppval < 0.05, 1, 0) ## covert p-value to flag

          ## see if there is evidence of a nonlinear trend (m6)
          ## inputs:  data and variables
          ##          dist (as both quassi-poisson and poisson variables use the same function)
          ## outputs: nonlin_output has a p-value and a shape 
          ## nonlinflag has values "concave", "", "convex"
          nonlin_output <- nonlin(dat, svar_for_glm, pdist="poisson", fexposuredat)
          nonlinflag <- ifelse(nonlin_output$nonlinpval > 0.05, ""
                               , nonlin_output$shape)  ## covert p-value to flag 
          
          ## see if there is evidence of seasonal structure (m7-m10)
          ## inputs:  data and pmods (as the models are formed using update)
          ##          dist (as both quassi-poisson and poisson variables use the same function)
          ## outputs: seas has a p-value
          ##    seasonflag=1 means there is evidence of seasonality
          ##    seasonflag=0 means there is no evidence of seasonality
          seas <- seasonal(dat, allmods=pmods, pdist="poisson")
          seasonflag <- ifelse(seas$seaspval < 0.05, 1 ,0)
          
          ## there was no dispersion, set dischangeflag to null
          dischangeflag <- ""
          
          ## count the number of outliers (using best of m1-m4)
          ## inputs:  data, pmods and bestmod as the count relates to bestmod only
          ##          dist (as both quassi-poisson and poisson variables use the same function)
          ## outputs: outcount_output is the count of the number of outliers detected
          outcount_output <- outcount(dat, pmods, bestmod, pdist="poisson")
          
          ## Make general model list for plotting
          modlist <- pmods
          
        } else {
          ### Underdispersion flag set to 0, have overdispersion
          underdisflag <- 0
          
          ## fit the QuasiPoisson models (m1-m4)
          ## inputs: data and variables being analysed
          ## outputs: qpmods which holds the glm summarys of M1-M4
          qpmods <- qpoissonmods(dat, svar_for_glm, fexposuredat)
          
          ## get the trend, last special (1-5), anova table, and model selected
          ## inputs: pmods (which holds the glm summarys of M1-M4)
          ## outputs:  last special and trend flags, and the best fitting model
          lstrendflagsmods <- qlstrendflags(qpmods, svar_for_glm, varname)     
          ## extract the model selected and call it best model
          bestmod <- lstrendflagsmods$modsel
          
          ## refit the models with the last observation dropped 
          ## inputs:  data and variables
          ##          need lstrendflagsmods as inputs as it needs to compare models with and without the last point
          ## outputs:  SecondLastSpecial - which is last special with the last observation removed
          ##          lastqrtchangflag - a number from 1-16 saying which pattern is was and is now
          lqtrchange_output <- lqtrchange(dat, svar_for_glm, pdist="quasipoisson"
                                          , lstrendflagsmods, fexposuredat)
          lqtrchangeflag <- lqtrchange_output$lastqrtchangflag
          SecondLastSpecial <- lqtrchange_output$SecondLastSpecial
          
          ## see if a model with a breakpoint (m5) fits well
          ## inputs:  data and variables
          ##          dist (as both quassi-poisson and poisson variables use the same function)
          ## outputs:  SecondLastSpecial - which is last special with the last observation removed
          ##          lastqrtchangflag - a number from 1-16 saying which pattern is was and is now
          bpout <- bp(dat, fvarname=svar_for_glm, pdist="quasipoisson", allmods=qpmods, fexposuredat)
          bpflag <- ifelse(bpout$bppval < 0.05, 1, 0) 
          
          ## see if there is evidence of a nonlinear trend (m6)
          ## inputs:  data and variables
          ##          dist (as both quassi-poisson and poisson variables use the same function)
          ## outputs: nonlin_output has a p-value and a shape 
          ## nonlinflag has values "concave", "", "convex"
          nonlin_output <- nonlin(dat, svar_for_glm, pdist="quasipoisson", fexposuredat)
          nonlinflag <- ifelse(nonlin_output$nonlinpval > 0.05, ""
                               , nonlin_output$shape) 
          
          
          ## see if there is evidence of seasonal structure (m7-m10)
          ## inputs:  data and pmods (as the models are formed using update)
          ##          dist (as both quassi-poisson and poisson variables use the same function)
          ## outputs: seas has a p-value
          ##    seasonflag=1 means there is evidence of seasonality
          ##    seasonflag=0 means there is no evidence of seasonality
          seas <- seasonal(dat, allmods=qpmods, pdist="quasipoisson")
          seasonflag <- ifelse(seas$seaspval < 0.05, 1 ,0)
          
          ## dischangeflag - investigate if dispersion has been removed with an alternative model
          ## inputs: data, varalbes, and all the models
          ## outputs: if dispersion is reduced a code (up to three digits) containing the information on the reduction
          dischangeflag <- dischanged(dat, svar_for_glm, qpmods, bpout$m5
                                      , nonlin_output$m6, seas$m7, seas$m8
                                      , seas$m9, seas$m10
                                      )
          
          ## count the number of outliers (using best of m1-m4)
          ## inputs:  data, pmods and bestmod as the count relates to bestmod only
          ##          dist (as both quassi-poisson and poisson variables use the same function)
          ## outputs: outcount_output is the count of the number of outliers detected
          outcount_output <- outcount(dat, qpmods, bestmod, pdist="quasipoisson")

          ## Make general model list for plotting
          modlist <- qpmods
          
        }
        
        ## The list of table of flags gets converted to dataframe later - skip the listing process
        tmptable <-  data.frame(
          AnalysisVariable            = fanalysisvar
          , SpecificVariable          = svar
          , AverageCount              = AverageCount
          , LastCount                 = LastCount
          , ModelFitted               = FitFlag
          , Exposure                  = ExposureFlag
          , Trend                     = lstrendflagsmods$trendflag
          , LastSpecial               = lstrendflagsmods$lsflag
          , SecondLastSpecial         = SecondLastSpecial
          , LastQuarterChangesPattern = lqtrchangeflag
          , Underdispersion           = underdisflag
          , Overdispersion            = overdisflag
          , ExcessZeros               = zflag
          , Outliers                  = outcount_output
          , ChangeInLevel             = bpflag
          , Nonlinearity              = nonlinflag
          , Seasonality               = seasonflag
          , OverdispersionRemoved     = dischangeflag
          , stringsAsFactors = FALSE
        )
        
        if (is.null(tagtable)) {
          tagtable <- tmptable
        } else {
          tagtable <- rbind(tagtable, tmptable)
        }
        
        ## -----------------  ##
        ## Plotting 
        if (fexposuredat==TRUE) {
          ylab = 'Counts per unit Exposure'
          yvar <- dat[, svar]/dat[,"exposure"]
          bmfitted <- (modlist[[bestmod]]$fitted)/dat[,"exposure"]
          bpfitted <- bpout[['m5']]$fitted/dat[,"exposure"]
          nlfitted <- nonlin_output[['m6']]$fitted/dat[,"exposure"]
          sefitted <- seas[[seas$bestseasmod]]$fitted/dat[,"exposure"]
          slsfitted <- c(lqtrchange_output[['mod_secondlast']]$fitted, NA)/dat[,"exposure"]
        } else {
          ylab <- 'Counts'
          yvar <- dat[, svar]
          bmfitted <- modlist[[bestmod]]$fitted
          bpfitted <- bpout[['m5']]$fitted
          nlfitted <- nonlin_output[['m6']]$fitted
          sefitted <- seas[[seas$bestseasmod]]$fitted
          slsfitted <- c(lqtrchange_output[['mod_secondlast']]$fitted, NA)
        }

        if (plotOutput) {
          svglite(
            file = paste0(fOutdir, fanalysisvar, '-', svar, '.svg')
            , height = 5
            , width = 10
          )
          
          par(
            mai = par("mai") + c(0,0,0,5) # Margin - inches
          )
          
          plot(
            y = yvar,
            x = dat$trend
            , xlab = 'Quarter'
            , ylab = ylab
            , xaxt = 'n'
            , col = 'black'
            , pch = 1
            , lwd = dat$lastspecial+1
            , main = paste(fanalysisvar, '\n', gsub('_._', ' & ', svar))
            , cex.main = 0.7
          )
          
          axis(
            1
            , at = dat$trend
            , label = paste(dat$year, dat$quarter, sep=' Q')
            , las = 2
            , cex.axis = 0.7
          )
          
          ## Plot the best fitting model
          lines(
            bmfitted
            , type ='l'
            , lty = 1
            , col = 'blue'
            , lwd=3
          )
          
          flegend <- ifelse(
            grepl('1', bestmod), 'Trend + LastSpecial'
            , ifelse(
              grepl('2', bestmod), 'LastSpecial'
              , ifelse(
                grepl('3', bestmod), 'Trend'
                , 'Constant'
              )
            )
          )
          
          ## Make legend list
          flegend <- paste(flegend, '(best fit out of m1 to m4)')
          flegend.col <- 'blue'
          flegend.lty <- 1
          flegend.lwd <- 3
          
          ## Plot change in level model if flagged
          if (bpflag == 1) {
            lines(
              bpfitted
              , type ='l'
              , lty = 2
              , col = 'red'
              , lwd=2
            )
            
            ## Update legend list
            flegend <- c(flegend, 'Change in level')
            flegend.col <- c(flegend.col, 'red')
            flegend.lty <- c(flegend.lty, 2)
            flegend.lwd <- c(flegend.lwd, 2)
          }
          
          ## Plot non-linear model if flagged
          if (nonlinflag != '') {
            lines(
              nlfitted
              , type ='l'
              , lty = 5
              , col = 'green3'
              , lwd=2
            )
            
            ## Update legend list
            flegend <- c(flegend, 'Quadratic')
            flegend.col <- c(flegend.col, 'green3')
            flegend.lty <- c(flegend.lty, 5)
            flegend.lwd <- c(flegend.lwd, 2)
          }
          
          ## Plot seasonal model if flagged
          if (seasonflag == 1) {
            lines(
              sefitted
              , type ='l'
              , lty = 4
              , col = 'magenta'
              , lwd=2
            )
            
            smodtype <- ifelse(
              grepl('10', seas$bestseasmod), 'Constant + Seasonal'
              , ifelse(
                grepl('9', seas$bestseasmod), 'Trend + Seasonal'
                , ifelse(
                  grepl('8', seas$bestseasmod), 'LastSpecial + Seasonal'
                  , 'Trend + LastSpecial + Seasonal'
                )
              )
            )
            
            ## Update legend list
            flegend <- c(flegend, smodtype)
            flegend.col <- c(flegend.col, 'magenta')
            flegend.lty <- c(flegend.lty, 4)
            flegend.lwd <- c(flegend.lwd, 2)
          }
          
          ## Plot second last special  model if flagged
          if (SecondLastSpecial != 3) {
            lines(
              slsfitted
              , type ='l'
              , lty = 3
              , col = 'grey'
              , lwd=2
            )
            
            ## Update legend list
            flegend <- c(flegend, 'Second Last Special')
            flegend.col <- c(flegend.col, 'grey')
            flegend.lty <- c(flegend.lty, 3)
            flegend.lwd <- c(flegend.lwd, 2)
          }
          
          legend(
            "right"
            , inset=c(-1,0)
            , legend = flegend
            , title = ifelse(overdisflag == 0, 'Poisson models', 'Quasi-Poisson models')
            , lty = flegend.lty
            , col = flegend.col
            , lwd = flegend.lwd
            , cex = 1
            , bty = "n"
            , xpd = NA
          )
          
          dev.off()
        }
      }
    }
  }

  ## Save to CSV if the output path (fOutdir) is provided
  if (!is.na(fOutdir)) {
    write.csv(
      tagtable
      , file = paste0(
        fOutdir, fanalysisvar
        , ifelse(fanalysisvar == '', '', '_')
        , 'trend_analysis_table_of_flags.csv'
      )
      , row.names=F
    )
  }
  
  return()
}