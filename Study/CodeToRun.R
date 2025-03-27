# Restore the renv to start
renv::activate()
renv::restore()

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
