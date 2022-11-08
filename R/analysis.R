############################################################
# Analysis is a source script containing analytic functions
#  - summarize_extract_plant
#  - summarize_extract_module
#  - summarize_extract_node
#  - distribution.crown_type
#  - test.kruskal
#  - test.chi
########################################################



#' Table of mean and standard deviation according to genotype, date, modality
#'
#' @description  Calculate mean and standard variation according to Genotype, date, modality
#' @param data Dataframe. Dataframe issue from extraction at plant scale
#' @param variables list of character/string. Variables which mean and standard deviation will be calculated
#'
#' @return Dataframe. where mean and standard deviation are compute for all variable
#' @export
#'
#' @usage summarize_extract_plant(data,variables)
#' @examples
#'    load("data/plant_data.rdata")
#'    plant_variables <- c("no_visible_leaves","no_foliar_primodium","no_total_leaves",
#'                         "no_open_flowers", "no_aborted_flowers","no_closed_flowers",
#'                         "no_total_flowers","no_fruits","no_stolons","no_vegetative_bud",
#'                         "no_initiated_bud","no_floral_bud","no_inflorescences",
#'                         "no_branch_crown","no_extension_crown","no_ramifications",
#'                         "order_max")
#'
#'     summarize_extract_plant(data = plant_data, variables=plant_variables)
#'
#' @import dplyr
summarize_extract_plant <- function(data, variables){

  df <- data %>%
    group_by(Genotype,date, modality) %>%
    summarise_at(variables,funs(mean=mean(.),sd=sd(.)))

  return(df)
}

#' Table of mean and standard deviation according to genotype, date, modality, order
#'
#' @description  Calculate mean and standard variation according to Genotype, date, modality, order
#' @param data Dataframe. Dataframe issue from extraction at module scale
#' @param variables list of string. list of character/string. Variables which mean and standard deviation will be calculated.
#'
#' @return Dataframe. where mean and standard deviation are compute for all variables
#' @export
#'
#' @usage summarize_extract_module(data, variables)
#' @examples
#'     load("data/module_data.rdata")
#'     variables <- c("no_visible_leaves",
#'                          "no_foliar_primordia",
#'                          "no_total_leaves",
#'                          "no_open_flowers",
#'                          "no_aborted_flowers",
#'                          "no_total_flowers",
#'                          "no_fruits",
#'                          "no_stolons",
#'                          "no_vegetative_bud",
#'                          "no_initiated_bud",
#'                          "no_floral_bud")
#'    summarize_data <- summarize_extract_module(data = module_data, variables = variables)
#'
#' @import dplyr
summarize_extract_module <-function(data, variables){

  df <- data %>%
    group_by(Genotype,date, order) %>%
    summarise_at(variables,funs(mean=mean(.), sd=sd(.)))

  return(df)
}

#' Table of mean and standard deviation according to genotype, date, modality, order
#'
#' @description  Calculate mean and standard variation according to Genotype, date, modality and order
#'
#' @param data Dataframe. Dataframe issue from extraction at node scale
#' @param variables list string. Variables which mean and standard deviation will be calculated
#'
#' @return Dataframe
#' @export
#'
#' @usage summarize_extract_node(data,variables)
#' @examples
#'      load("data/node_data.rdata")
#'
#'      node_variables <- c("no_visible_leaves","no_foliar_primordia","no_total_leaves",
#'      "no_open_flowers","no_aborted_flowers","no_total_flowers","no_fruits",
#'      "no_stolons","no_vegetative_bud","no_initiated_bud","no_floral_bud")
#'
#'      summarize_extract_node(data = node_data, variables = node_variables)
#'
#' @import dplyr
summarize_extract_node <- function(data, variables){

  df <- data %>%
    group_by(Genotype, date, order, rank) %>%
    summarise_at(variables, funs(mean = mean(.), sd = sd(.)))

  return(df)
}

#' Table of distribution of the type of crown
#'
#' @description Contingency table of Type of Crowns distribution.
#'     Return the contengency table in frequency or probability according frequency_type option
#' @param data Dataframe. Data at module scale
#'
#' @return Dataframe. data containing for each crown type frequency (n) and relative frequency (prob) for each genotype and order
#' @export
#' @usage distribution.crown_type(data)
#' @examples
#'     load("data/module_data.rdata")
#'     distribution.crown_type(data = module_data)
#' @import dplyr
distribution.crown_type <- function(data){

  df <- data%>%
    group_by(Genotype, order, type_of_crown)%>%
    summarise(n = n()) %>% # frequency
    mutate(prob = n / sum(n))%>% #relative frequency
    as.data.frame()


  return(df)
}

#' Table of stage distribution at plant scale
#'
#' @description Table of distribution stage at plant scale from module scale data
#'
#' @param data Dataframe. Dataframe at module scale
#'
#' @return Dataframe. data containing stage frequency (n) and relative frequency (prob) of stage for each genotype and date
#' @export
#'
#' @usage distribution.stage(data)
#' @examples
#'     load("data/module_data.rdata")
#'     distribution.sate(data = module_data)
#' @import dplyr
distribution.stage <- function(data){

  df = data %>%
    group_by(Genotype, modality, date, stage)%>%
    summarize(n = n()) %>%
    mutate(total = sum(n),prob = (n / sum(n)) * 100) %>%
    as.data.frame()

  return(df)
}

#' Table of axillary production at node scale
#'
#' @description Contingency table of axillary production
#' @param data Dataframe. Data issue of the extraction at node scale
#'
#' @return DataFrame. Data containing frequency (n) and relative frequency (prob) for each genotype
#' @export
#'
#' @usage distribution.axillary_production(data)
#' @examples
#'     load("data/node_data.rdata")
#'     distribution.axillary_production(data = node_data)
#' @import dplyr
distribution.axillary_production<- function(data){

  df <- data%>%
    group_by(rank,Genotype, order, branching_type)%>%
    summarise(n = n())%>%
    mutate(total= sum(n), prob= n/sum(n))

  return(df)
}

#' ANOVA on the Rank test (Kruskal Wallis)
#'
#' @description Kruskal Wallis Test from agricolae package.
#'
#' @param data Dataframe. Dataframe from module scale extraction
#' @param variable string. response, name of the variable wish you execute comparison test
#' @param trt treatment Variables or intercept function
#' @param letter logical. TRUE display group with letters else FALSE, by default TRUE
#' @param p.adj Method for adjusting p values, by default "holm" ("none","holm","hommel", "hochberg", "bonferroni", "BH", "BY", "fdr")
#' @param alpha float. level signification, by default 0.05
#' @param display bool. print output, by default TRUE
#'
#' @return dataframe containing kruskal Wallis output
#' @export
#'
#' @usage test.kruskal(data, variable, trt, letter = TRUE, p.adj = "holm", alpha = 0.05, display = TRUE)
#' @examples
#'     load("data/module_data.rdata")
#'     test.kruskal(data = module_data,
#'                  variable = "no_total_leaves",
#'                  letter = FALSE,
#'                  trt = interaction(module_data[, "Genotype"], module_data[, "order"])
#'                  )
#'
#' @import agricolae
test.kruskal <- function(data, variable, trt, letter = TRUE, p.adj = "holm", alpha = 0.05, display = TRUE){
  kruskal(y = data[,variable],
          trt = trt,
          group = letter,
          p.adj = p.adj,
          alpha = alpha,
          console = display)
}

#' Chi-test pairwise comparison to analyse crowns distribution by order
#'
#' @description @description reshape distribution data and perform pairwise chi-test comparison between genotype for one order from rstatix library
#' @param data Dataframe. Data issue from distribution.crown_type
#' @param order nt. Order for which analysis is performed.
#' @param crown_type string. "Branch" or "Extension" Crowns
#'
#' @return Dataframe containing pairwise chi-test output
#' @export
#'
#' @usage test.chi(data = data, order = 1, crown_type = "Branch")
#' @examples
#'     load("data/module_data.rdata")
#'     dist <- distribution.crown_type(data = module_data)
#'     test.chi(data = dist, order = 1, crown_type = "Branch")
#' @import rstatix
test.chi <- function(data, order = 1, crown_type = "Branch"){

  #reshape data
  t <- reshape(data,
               idvar = c("Genotype", "order"),
               timevar = "type_of_crown",
               direction = "wide",
               drop = c("relative_freq"))

  t[is.na(t)] <- 0 # replace NA by 0

  # change colname
  colnames(t) <- c("Genotype","order", "Main", "Branch", "Extension")
  # filter by order
  t <- t[t[, "order"] == order, ]
  # add Genotype as rownames index
  rownames(t) <- t[, "Genotype"]
  t <- t[, -c(1, 2, 3)] # delete Main

  # transpose data
  t <- t(t)

  # Transform data into dataframe
  t <- as.data.frame(t)

  # excute pairwise chi-test from rstatix library
  comparison <- pairwise_chisq_gof_test(t[crown_type,], p.adjust.method = "holm")

  return(comparison)
}
