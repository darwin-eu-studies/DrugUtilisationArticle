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

ingredients <- c(
  "lovastatin", "pravastatin", "fluvastatin", "atorvastatin", "cerivastatin",
  "rosuvastatin", "pitavastatin", "ezetimibe", "evolocumab", "alirocumab",
  "evinacumb", "simvastatin"
)

codes <- CodelistGenerator::getDrugIngredientCodes(
  cdm = cdm, name = ingredients
)

omopgenerics::exportCodelist(codes, path = here::here("Ingredients"), type = "csv")

# convert json files to csv
codes <- CodelistGenerator::codesFromConceptSet(path = here::here("Conditions"), cdm = cdm)
omopgenerics::exportCodelist(codes, path = here::here("Conditions"), type = "csv")

codes <- omopgenerics::importCodelist(here::here("Indications"))
omopgenerics::exportCodelist(codes, path = here::here("Indications"), type = "csv")

codes <- omopgenerics::importCodelist(here::here("Medications"))
omopgenerics::exportCodelist(codes, path = here::here("Medications"), type = "csv")
