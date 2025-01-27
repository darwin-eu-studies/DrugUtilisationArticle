# Restore the renv to start
renv::activate()
renv::restore()
library(DBI)
library(dplyr)
library(log4r)
library(CDMConnector)
library(here)
library(DrugUtilisation)
library(tidyr)
library(SqlRender)
library(CodelistGenerator)
library(PatientProfiles)
library(CohortConstructor)
library(omopgenerics)
library(RPostgres)
# Connection details
db <- DBI::dbConnect("...")
databaseAcronym <- "..."
cdmDatabaseSchema <- "..."
resultsDatabaseSchema <- "..."
resultsStem <- "..."

# minimum cell count suppression
minCellCount <- 5

# create the cdm object
cdm <- CDMConnector::cdmFromCon(
  con = db,
  cdmSchema = cdmDatabaseSchema,
  writeSchema = resultsDatabaseSchema,
  cdmName = databaseAcronym,
  writePrefix = resultsStem
)

# run analysis
source("RunStudy.R")
