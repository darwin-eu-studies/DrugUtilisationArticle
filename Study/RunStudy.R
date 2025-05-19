# parameters ----
ageGroup <- list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))
strata <- list("age_group", "sex", c("age_group", "sex"))
prevalentCohort <- "prevalent_users_cohort"
newUsersCohort <- "new_users_cohort"
newUsersWithFuturePrescriptions <- "new_users_future"
denominatorCohort <- "denominator"
conditionsCohort <- "conditions"
medicationsCohort <- "medications"
indicationCohort <- "indications"
alternativeCohort <- "alternative"
studyPeriod <- as.Date(c("2010-01-01", "2021-12-31"))
ingredientConceptId <- 1539403

# initiate project ----
resultsFolder <- here::here("Results")
if (!dir.exists(resultsFolder)) dir.create(resultsFolder)
logName <- paste0("log_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".txt")
logFile <- file.path(resultsFolder, logName)
logger <- log4r::create.logger(logfile = logFile, level = "INFO")
printLog <- function(logger, message, nm = "i") {
  log4r::info(logger, message)
  date <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
  "{.pkg {date}} {message}" |>
    rlang::set_names(nm) |>
    cli::cli_inform()
}
printLog(logger, "Log file created", "v")

# read codelists ----
printLog(logger, "Reading codelists")
codes <- omopgenerics::importCodelist(
  path = here::here("Codelists", "Ingredients"), type = "csv"
)
simvastatin <- codes["simvastatin"]
alternativeIngredients <- codes[names(codes) != "simvastatin"]
conditions <- omopgenerics::importCodelist(
  path = here::here("Codelists", "Conditions"), type = "csv"
)
medications <- omopgenerics::importCodelist(
  path = here::here("Codelists", "Medications"), type = "csv"
)
indications <- omopgenerics::importCodelist(
  path = here::here("Codelists", "Indication"), type = "csv"
)
printLog(logger, "All codelist imported", "v")

# cdm snapshot ----
printLog(logger, "Extracting cdm snapshot")
snapshot <- OmopSketch::summariseOmopSnapshot(cdm)
printLog(logger, "cdm snapshot done", "v")

# benchmarking ----
bm  <- DrugUtilisation::benchmarkDrugUtilisation(
  cdm,
  ingredient = "simvastatin",
  alternativeIngredient = names(alternativeIngredients),
  indicationCohort = NULL
)

# create prevalent user cohorts ----
printLog(logger, "Create prevalent users cohorts")
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
  cdm = cdm,
  conceptSet = simvastatin,
  name = prevalentCohort,
  gapEra = 30
)
countsPrevalent <- cdm[[prevalentCohort]] |>
  CohortCharacteristics::summariseCohortCount()
attritionPrevalent <- cdm[[prevalentCohort]] |>
  CohortCharacteristics::summariseCohortAttrition()
printLog(logger, "Prevalent cohorts instantiated", "v")

# incidence and prevalence ----
printLog(logger, "Create denominator cohorts")
cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm,
  name = denominatorCohort,
  cohortDateRange = studyPeriod,
  ageGroup = c(list(c(0, 150)), ageGroup),
  sex = c("Both", "Male", "Female"),
  daysPriorObservation = 365,
  requirementInteractions = TRUE
)
countsDenominator <- cdm[[denominatorCohort]] |>
  CohortCharacteristics::summariseCohortCount()
attritionDenominator <- cdm[[denominatorCohort]] |>
  CohortCharacteristics::summariseCohortAttrition()
printLog(logger, "Denominator cohorts created", "v")

printLog(logger, "Calculate period prevalence")
prevalence <- IncidencePrevalence::estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = denominatorCohort,
  outcomeTable = prevalentCohort,
  interval = c("months", "years")
)
printLog(logger, "Period prevalence calculated", "v")

printLog(logger, "Calculate incidence")
incidence <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = denominatorCohort,
  outcomeTable = prevalentCohort,
  outcomeWashout = 365,
  interval = c("months", "years")
)
printLog(logger, "Incidence calculated", "v")

# create new user cohorts ----
printLog(logger, "Create new users cohorts")
cdm[[newUsersCohort]] <- cdm[[prevalentCohort]] |>
  dplyr::compute(name = newUsersCohort, temporary = FALSE) |>
  omopgenerics::newCohortTable(
    cohortSetRef = omopgenerics::settings(cdm[[prevalentCohort]]) |>
      dplyr::mutate(cohort_name = "new_user_simvastatin")
  ) |>
  DrugUtilisation::requirePriorDrugWashout(days = 365) |>
  DrugUtilisation::requireObservationBeforeDrug(days = 365) |>
  DrugUtilisation::requireDrugInDateRange(dateRange = studyPeriod) |>
  DrugUtilisation::requireIsFirstDrugEntry()
countsNewUsers <- cdm[[newUsersCohort]] |>
  CohortCharacteristics::summariseCohortCount()
attritionNewUsers <- cdm[[newUsersCohort]] |>
  CohortCharacteristics::summariseCohortAttrition()
printLog(logger, "New users cohorts created", "v")

# add sex and ageGroup to stratify latter ----
printLog(logger, "Add stratification variables")
cdm[[newUsersCohort]] <- cdm[[newUsersCohort]] |>
  PatientProfiles::addDemographics(
    sex = TRUE,
    age = FALSE,
    ageGroup = ageGroup,
    priorObservation = FALSE,
    futureObservation = FALSE,
    dateOfBirth = FALSE
  )
printLog(logger, "Stratification variables added", "v")

# instantiate characterisation cohorts  ----
printLog(logger, "Instantiate characterisation cohorts")
cdm[[conditionsCohort]] <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = conditions,
  name = conditionsCohort,
  exit = "event_start_date"
  # TO DO issue 400 in CC
  # subsetCohort = newUsersCohort
)
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = medicationsCohort,
  conceptSet = medications,
  subsetCohort = newUsersCohort
)
printLog(logger, "Characterisation cohorts instantiated", "v")

# summarise characteristics ----
printLog(logger, "Summarise new users characteristics")
characteristics <- CohortCharacteristics::summariseCharacteristics(
  cohort = cdm[[newUsersCohort]],
  strata = strata,
  ageGroup = ageGroup,
  tableIntersectCount = list("Visits in prior year" = list(
    tableName = "visit_occurrence", window = c(-365, -1)
  )),
  cohortIntersectFlag = list(
    "Conditions any time prior" = list(
      targetCohortTable = conditionsCohort, window = c(-Inf, -1)
    ),
    "Medications prior year" = list(
      targetCohortTable = medicationsCohort,  window = c(-365, -1)
    )
  )
)
printLog(logger, "New users characteristics summarised", "v")

# summarise indication ----
printLog(logger, "Instantiate indications cohorts")
cdm[[indicationCohort]] <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = indications,
  name = indicationCohort,
  exit = "event_start_date"
  #subsetCohort = newUsersCohort
)
printLog(logger, "Indications cohorts instantiated", "v")

printLog(logger, "Summarise indications")
indication <- cdm[[newUsersCohort]] |>
  DrugUtilisation::summariseIndication(
    strata = strata,
    indicationCohortName = indicationCohort,
    indicationWindow = list(c(0, 0), c(-30, 0), c(-180, 0), c(-Inf, 0)),
    unknownIndicationTable = c("observation", "condition_occurrence"),
    mutuallyExclusive = FALSE
  )
printLog(logger, "Indications summarised", "v")

# summarise drug utilisation ----
printLog(logger, "Summarise drug utilisation")
drugUtilisation <- cdm[[newUsersCohort]] |>
  DrugUtilisation::summariseDrugUtilisation(
    strata = strata,
    conceptSet = simvastatin,
    ingredientConceptId = ingredientConceptId,
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    restrictIncident = TRUE,
    gapEra = 30,
    estimates = c(
      "count", "percentage", "count_missing", "percentage_missing", "min",
      "q25", "median", "q75", "max", "density"
    )
  )
printLog(logger, "Drug utilisation summarised", "v")

# new users with future prescriptions ----
cdm[[newUsersWithFuturePrescriptions]] <-  cdm[[newUsersCohort]] |>
  dplyr::select("subject_id", "start_date" = "cohort_start_date", "age_group", "sex") |>
  dplyr::inner_join(cdm[[prevalentCohort]], by = c("subject_id")) |>
  dplyr::filter(.data$cohort_start_date >= .data$start_date) |>
  dplyr::select(!"start_date") |>
  dplyr::compute(name = newUsersWithFuturePrescriptions, temporary = FALSE) |>
  omopgenerics::newCohortTable()

# summarise proportion of patients covered ----
printLog(logger, "Summarise PPC")
ppc <- cdm[[newUsersWithFuturePrescriptions]] |>
  DrugUtilisation::summariseProportionOfPatientsCovered(
    strata = strata,
    followUpDays = 365
  )
printLog(logger, "PPC summarised", "v")

# instantiate alternative ingredients ----
printLog(logger, "Instantiate alternative ingredients cohorts")
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
  cdm = cdm,
  conceptSet = alternativeIngredients,
  name = alternativeCohort,
  subsetCohort = newUsersCohort
)
printLog(logger, "Alternative ingredients cohorts instantiated", "v")

# summarise comedications before start ----
printLog(logger, "Summarise comedications")
comedications <- cdm[[newUsersCohort]] |>
  DrugUtilisation::summariseTreatment(
    window = list(
      c(-360, -271), c(-270, -181), c(-180, -91), c(-90, -1), c(0, 0), c(1, 90),
      c(91, 180), c(181, 270), c(271, 360)
    ),
    treatmentCohortName = alternativeCohort,
    strata = strata,
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    mutuallyExclusive = FALSE
  )
comedications <- comedications |>
  omopgenerics::newSummarisedResult(
    settings = omopgenerics::settings(comedications) |>
      dplyr::mutate(index_date = "cohort_start_date")
  )
printLog(logger, "Comedications summarised", "v")

# drug restart ----
printLog(logger, "Summarise drug restart")
drugRestart <- cdm[[newUsersWithFuturePrescriptions]] |>
  DrugUtilisation::summariseDrugRestart(
    switchCohortTable = alternativeCohort,
    strata = strata,
    followUpDays = c(90, 180, 270, 360),
    restrictToFirstDiscontinuation = TRUE,
    incident = FALSE
  )
printLog(logger, "Drug restart summarised", "v")

# summarise medications after end ----
printLog(logger, "Summarise medications after discontinuation")
cdm <- omopgenerics::bind(cdm[[prevalentCohort]], cdm[[alternativeCohort]], name = alternativeCohort)
switchMedications <- cdm[[newUsersCohort]] |>
  DrugUtilisation::summariseTreatment(
    window = list(c(-90, -1), c(0, 0), c(1, 90), c(91, 180), c(181, 270), c(271, 360)),
    treatmentCohortName = alternativeCohort,
    strata = strata,
    indexDate = "cohort_end_date",
    censorDate = NULL,
    mutuallyExclusive = FALSE
  )
switchMedications <- switchMedications |>
  omopgenerics::newSummarisedResult(
    settings = omopgenerics::settings(switchMedications) |>
      dplyr::mutate(index_date = "cohort_end_date")
  )
printLog(logger, "Medications after discontinuation summarised", "v")

# exporting results ----
printLog(logger, "Exporting results")
omopgenerics::exportSummarisedResult(
  snapshot,
  bm,
  countsPrevalent,
  attritionPrevalent,
  countsDenominator,
  attritionDenominator,
  prevalence,
  incidence,
  countsNewUsers,
  attritionNewUsers,
  characteristics,
  indication,
  drugUtilisation,
  ppc,
  comedications,
  drugRestart,
  switchMedications,
  minCellCount = minCellCount,
  fileName = "drug_utilisation_article_{cdm_name}_{date}.csv",
  path = resultsFolder
)
zip::zip(
  zipfile = here::here(
    resultsFolder, paste0("Results_", omopgenerics::cdmName(cdm), ".zip")
  ),
  files = list.files(resultsFolder, pattern = ".csv|.txt"),
  root = resultsFolder
)
printLog(logger, "Results exported", "v")

cli::cli_inform(c("v" = "Study fnished"))
cli::cli_inform(c("i" = "Please see the csv exported, the log file and the zipped results containing both."))

# drop the permanent tables created during the analysis ----
dropAllTables <- function() {
  answer <- "2"
  if (rlang::is_interactive()) {
    cli::cli_inform(c(
      "!" = "Do you want to drop tables cretaed during the study?. Enter choice 1 or 2:",
      " " = "1) Delete tables",
      " " = "2) Keep tables"
    ))
    answer <- readline()
    while (!answer %in% c("1", "2")) {
      cli::cli_inform(c("x" = "Invalid input. Please choose 1 to delete or 2 to keep them:"))
      answer <- readline()
    }
  }
  if (answer == "1") {
    tables <- omopgenerics::listSourceTables(cdm = cdm)
    cli::cli_inform(c("!" = "The following tables will be deleted: {.pkg {tables}}. Can you confirm?", " " = "1) YES", " " = "2) NO"))
    answer <- readline()
    while (!answer %in% c("1", "2")) {
      cli::cli_inform(c("x" = "Invalid input. Please choose 1 to confirm or 2 to cancel:"))
      answer <- readline()
    }
    if (answer == "1") {
      cli::cli_inform(c("i" = "Deleting tables"))
      omopgenerics::dropSourceTable(cdm = cdm, name = tables)
    }
  }
  invisible()
}
dropAllTables()
