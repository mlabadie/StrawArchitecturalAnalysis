########################################################
# Reader is a script containing import function data
#  - csv_reader
########################################################


#' CSV data import
#'
#' @description This function allows to import csv data issue from data extraction at different scale from Web openalea.strawberry application
#' @param filepath string Path of file
#' @param header logical csv file contain header TRUE else False. default TRUE
#' @param sep string. type of separator, by default ";"
#' @param dec string type of decimal by default "."
#' @param variable.characters vector of string. List of variables names corresponding to character
#'     by default c("Genotype", "modality", "type_of_crown", "stage", "complete_module")
#' @param variable.integer vector of string List of variables names corresponding to integer
#'     by default c("plant","order","no_visible_leaves","no_foliar_primordia","no_total_leaves","no_open_flowers",
#'                  "no_aborted_flowers","no_total_flowers","no_fruits","no_stolons","no_vegetative_bud","no_initiated_bud",
#'                  "no_floral_bud"),
#' @param variable.date vector of string List of variables names corresponding to date, by default date
#' @param date_format string format of the date, by default "%d/%m/%Y"
#' @param timezone string. timezone of the experiment, by default "UTC"
#'
#' @return a Dataframe
#' @export
#'
#' @usage csv_reader(filepath="path", header, sep, dec, variable.characters, variable.integer, variable.date, date_format, timezone)
#' @examples csv_reader(filepath = "D:/Mes Donnees/BreedingValue/R/RStrawberryAnalysis/data/raw/Module_scale_data.csv",
#'                      header = TRUE,
#'                      sep = ";",
#'                      dec=".",
#'                      variable.characters = c("Genotype", "modality", "type_of_crown", "stage", "complete_module"), # nolint
#'                      variable.integer = c("plant",
#'                                        "order",
#'                                        "no_visible_leaves",
#'                                        "no_foliar_primordia",
#'                                        "no_total_leaves",
#'                                        "no_open_flowers",
#'                                        "no_aborted_flowers",
#'                                        "no_total_flowers",
#'                                        "no_fruits",
#'                                        "no_stolons",
#'                                        "no_vegetative_bud",
#'                                        "no_initiated_bud",
#'                                        "no_floral_bud"),
#'                  variable.date = c("date"),
#'                  date_format = "%d/%m/%Y",
#'                  timezone= "UTC")
csv_reader <- function(filepath,
                       header = TRUE,
                       sep = ";",
                       dec = ".",
                       variable.characters = c("Genotype", "modality", "type_of_crown", "stage", "complete_module"),
                       variable.integer = c("plant",
                                            "order",
                                            "no_visible_leaves",
                                            "no_foliar_primordia",
                                            "no_total_leaves",
                                            "no_open_flowers",
                                            "no_aborted_flowers",
                                            "no_total_flowers",
                                            "no_fruits",
                                            "no_stolons",
                                            "no_vegetative_bud",
                                            "no_initiated_bud",
                                            "no_floral_bud",
                                            "vid",
                                            "plant_vid"),
                       variable.date = c("date"),
                       date_format = "%d/%m/%Y",
                       timezone = "UTC"){

  df <- read.csv(file = filepath, header = header, sep = sep, dec = dec)

  string <- variable.characters
  date <- variable.date
  int <- variable.integer

  df[, string] <- lapply(df[, string], as.character)
  df[, date] <- as.Date(df[, date], format=date_format, tz = timezone)
  df[, int] <- lapply(df[, int], FUN = as.integer)

  return(df)
}
