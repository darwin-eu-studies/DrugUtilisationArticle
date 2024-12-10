# Restore the renv to start
renv::activate()
renv::restore()

library(DBI)
library(CDMConnector)
library(DrugUtilisation)
library(CodelistGenerator)
library(PatientProfiles)
library(dplyr)
library(here)
library(log4r)
library(readr)
library(zip)
library(IncidencePrevalence)
library(CohortCharacteristics)

# Connection details
db <- DBI::dbConnect("...")

# connection details
databaseAcronym <- "..."
cdmDatabaseSchema <- "..."
resultsDatabaseSchema <- "..."
resultsStem <- "..."

cdm <- CDMConnector::cdmFromCon(
  con = db,
  cdmSchema = cdmDatabaseSchema,
  writeSchema = resultsDatabaseSchema,
  cdmName = databaseAcronym,
  writePrefix = resultsStem
)

# run analysis
source("RunStudy.R")
