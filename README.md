# PETA
PETA is a collection of scripts written in R (<https://www.r-project.org/>) that are used to perform trend analysis on the aviation occurrence data held by the Civil Aviation Safety Authority ([CASA](https://www.casa.gov.au/)).

PETA is an acronym for Poisson Event Trend Analysis. The scripts were originally developed in 2014 under contract by Data Analysis Australia ([DAA](http://www.daa.com.au/)). The software continues to be actively developed by CASA and was released by the agency under the [GNU General Public License, version 2](http://www.gnu.org/licenses/gpl-2.0.html) in 2015.

Please send questions, comments, or other correspondence to: peta (AT) casa.gov.au.

## PETA Environment

In order to run PETA there are a few pieces of software which are required to be installed correctly on your system.

### Obtaining the software

The required software is shown in the following table:

Software | Description                                              | URL
---------|----------------------------------------------------------|----
R        | Microsoft R Open, including the Intel Math Kernel Library (MKL) | <https://mran.microsoft.com/>
RStudio  | Integrated Development Environment (IDE) for R | <https://www.rstudio.com/>
Git      | Distributed version control system | <https://git-scm.com/>

### Setting up the environment

Install all the software as per the instructions on the respective websites. Before you can obtain PETA you will need to perform the following configuration:

* RStudio - Tell RStudio where to find Git by following the instructions [here](https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN)
* Git - Depending upon your network configuration you may need to provide Git with your network proxy settings so that it can communicate with GitHub. This document is not intended to be a comprehensive Git reference so detailed information on configuring Git can be found in the [Pro Git](https://git-scm.com/book/en/v2) book.

## Obtaining PETA

Once the environment has been setup correctly the PETA repository can be cloned by:

* clicking on **Project: (None)** in the top-right corner of the RStudio window and selecting **New Project...**.
* Choose **Version Control** on the first page, followed by **Git** on the second page, and then...
* enter the PETA repository URL which is: <https://github.com/CASA-AU/PETA.git>

## Running PETA

Coming soon...
