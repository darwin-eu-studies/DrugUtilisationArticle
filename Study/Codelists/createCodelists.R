
ingredients <- c(
  "lovastatin", "pravastatin", "fluvastatin", "atorvastatin", "cerivastatin",
  "rosuvastatin", "pitavastatin", "ezetimibe", "evolocumab", "alirocumab",
  "evinacumb", "simvastatin"
)

codes <- CodelistGenerator::getDrugIngredientCodes(
  cdm = cdm, name = ingredients, nameStyle = "{concept_name}"
)

omopgenerics::exportCodelist(
  x = codes, path = here::here("Codelists", "Ingredients"), type = "csv"
)

# convert json files to csv
# codes <- CodelistGenerator::codesFromConceptSet(
#   path = here::here("Codelists", "Conditions"), cdm = cdm
# )
# omopgenerics::exportCodelist(
#   codes, path = here::here("Codelists", "Conditions"), type = "csv"
# )
#
# codes <- omopgenerics::importCodelist(here::here("Codelists", "Indication"))
# omopgenerics::exportCodelist(
#   codes, path = here::here("Codelists", "Indication"), type = "csv"
# )
#
# codes <- CodelistGenerator::codesFromConceptSet(
#   path = here::here("Codelists", "Medications"), cdm = cdm
# )
# omopgenerics::exportCodelist(
#   codes, path = here::here("Codelists", "Medications"), type = "csv"
# )
