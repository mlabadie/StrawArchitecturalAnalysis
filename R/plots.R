############################################################
# Plot is a source script containing plot functions
#  - plot_pointwisemean
#  - plot_distribution.crown_type
#
############################################################


#' Graphical Theme for publication
#'
#' @param base_size int. text size
#' @param base_family sting. Police
#' @param base_line_size  int. line size
#'
#' @return ggplot theme for publication
#'
#' @import ggplot2
theme_publish <- function(base_size = 12, base_family = "",
                          base_line_size = 0.8) {
  half_line <- base_size / 2
  small_rel <- 0.8
  small_size <- small_rel * base_size

  # TODO: replace size with linewidth in `element_rect()`
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      rect = element_rect(fill = "transparent", colour = NA, color = NA,
                          size = 0, linetype = 0),
      text = element_text(family = base_family, face = "plain",
                          colour = "black", size = base_size, hjust = 0.5,
                          vjust = 0.5, angle = 0, lineheight = 0.9,
                          margin = ggplot2::margin(), debug = F),

      axis.text = element_text(size = small_size),
      axis.text.x = element_text(margin = ggplot2::margin(t = small_size/4),
                                 vjust = 1),
      axis.text.y = element_text(margin = ggplot2::margin(r = small_size/4),
                                 hjust = 1),
      axis.title.x = element_text(margin = ggplot2::margin(t = small_size,
                                                           b = small_size)),
      axis.title.y = element_text(angle = 90,
                                  margin = ggplot2::margin(r = small_size,
                                                           l = small_size/4)),
      axis.ticks = element_line(colour = "black", size = base_line_size),
      axis.ticks.length = unit(0.25, 'lines'),

      axis.line = element_line(colour = "black", size = base_line_size),
      axis.line.x = element_line(colour = "black", size = base_line_size),
      axis.line.y = element_line(colour = "black", size = base_line_size),

      legend.spacing = unit(base_size/4, "pt"),
      legend.key = element_blank(),
      legend.key.size = unit(1 * base_size, "pt"),
      legend.key.width = unit(1.5 * base_size, 'pt'),
      legend.text = element_text(size = rel(small_rel)),
      legend.title = element_text(size = rel(small_rel), face = 'bold'),
      legend.position = 'right',
      legend.box = 'horizontal',

      panel.spacing = unit(1, "lines"),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_line(colour="lightgray"),
      panel.grid.minor = element_blank(),

      strip.text = element_text(size = base_size),
      strip.background = element_rect(fill = NA, colour = "black",
                                      size = 0.125),
      strip.text.x = element_text(face = 'bold', hjust = 0,
                                  margin = ggplot2::margin(b = small_size/2,
                                                           t = small_size/4)),
      strip.text.y = element_text(angle = -90, face = 'bold',
                                  margin = ggplot2::margin(l = small_size/2,
                                                           r = small_size/4)),

      plot.margin = unit(c(5,5,0,0), "pt"),
      plot.background = element_blank(),
      plot.title = element_text(face = "bold", size = 1.2 * base_size,
                                margin = ggplot2::margin(b = half_line),
                                hjust = 0)
    )
}

#' Plot pointwise mean
#'
#' @description Plot pointwise mean
#'
#' @param data Dataframe. Data from extraction at plant and module scale
#' @param scale string. "plant" or "module" according to data extraction
#' @param variable string. Variable plotted
#' @param display_sd logical. If you want display standard deviation or not, by default TRUE
#'
#' @return Dataframe
#' @export
#'
#' @usage plot_pointwisemean(data, scale, variable, display_sd = TRUE)
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
#'     summarize_data <- summarize.extract_module(data = module_data, variables = variables)
#'     plot_pointwisemean(summarize_data, variable = "no_total_leaves",scale = "module", display_sd = FALSE)
#' @import ggplot2
#' @import scales
#' @import stringr
plot_pointwisemean <- function(data, scale, variable, display_sd = TRUE){

  if (scale == "plant"){
    plot <- ggplot(data = data,
                   mapping = aes(x = date,
                                 y = unlist(data[,paste(variable, "mean", sep = "_")]),
                                 color = unlist(data[,'Genotype'])
                   )
    )+
      geom_point()+
      geom_line()+
      ylab(paste('Mean', str_replace_all(variable, "_", " "), sep = " ")) +
      xlab('Date')+
      labs(title =  paste("Pointwise mean", str_replace_all(variable, "_", " "), "by order", sep= " "), color = "Genotype")+
      theme_publish()

  }else if (scale == "module") {
    plot <- ggplot(data = data,
                   mapping = aes(x = unlist(data[,"order"]),
                                 y = unlist(data[,paste(variable, "mean", sep = "_")]),
                                 color = unlist(data[,'Genotype'])
                   )
    )+
      geom_line()+
      geom_point()+
      ylab(paste('Mean', str_replace_all(variable, "_", " "), sep = " ")) +
      xlab("Order")+
      labs(title =  paste("Pointwise mean", str_replace_all(variable, "_", " "), "by order", sep= " "), color = "Genotype")+
      scale_x_continuous(breaks = pretty_breaks())+
      theme_publish()
  }

  if (display_sd == TRUE){
    plot<- plot+
      geom_errorbar(data = data,
                    aes(ymin = unlist(data[,paste(variable, "mean", sep = "_")]) - unlist(data[,paste(variable, "sd", sep = "_")])),
                    ymax = unlist(data[,paste(variable, "mean", sep = "_")]) + unlist(data[,paste(variable, "sd", sep = "_")]),
                    width=0.2)
  }

  return(plot)
}

#' Crown type plot distribution
#'
#' @description Plot crown type distribution
#'
#' @param data DataFrame. Crown type distribution data from distribution.crown_type function
#' @param crown_type string. type of crown ("Branch", "Extension")
#' @param variable string. "freq" for frequency or "rfreq" for relative frequency, by default "freq"
#'
#' @return plot of crown_type frequency or relative frequency distribution
#'
#' @usage plot_distribution.crown_type(data, crown_type,variable="freq")
#' @examples
#'     load("data/module_data.rdata")
#'     dist <- distribution.crown_type(data = module_data)
#'     plot_distribution.crown_type(data=dist, crown_type = "Extension", variable = "freq")
#' @import ggplot2
#' @import scales
#' @export
plot_distribution.crown_type <- function(data, crown_type,variable="freq"){

  if (variable=="freq"){
    y <- data[data["type_of_crown"]==crown_type,"n"]
    title = paste("Frequency distribution of ",crown_type, "crowns")
    ylab = "Frequency"
  }else{
    y <- data[data["type_of_crown"]==crown_type,"prob"]
    title = paste("Relative frequency distribution of ",crown_type, "crowns")
    ylab = "Relative frequency"
  }

  plot<- ggplot(data = data[data["type_of_crown"]==crown_type,],
                mapping = aes(x = order,y = y,
                              color = Genotype))+
    geom_point() +
    geom_line()+
    labs(title= title)+
    ylab(ylab)+
    scale_x_continuous(breaks = pretty_breaks())+
    theme_publish()


  return(plot)

}

#' Plot of axillary production accoring to node rank
#'
#' @description Plot of axillary production according to genotype and order as function of node rank
#'
#' @param data DataFrame. DataFrame issue from distribution.axillary_production function
#' @param genotype string. Genotype name selected
#' @param order int. Order selected, by default 0
#' @param variable string. "n" for frequency or "prob" for relative frequency
#'
#' @return plot with distribution of axillary production as function of node rank
#'
#' @usage plot_axillary_production(data, genotype, order = 0, variable = "n")
#' @examples
#'     load("data/node_data.rdata")
#'     dist <- distribution.axillary_production(node_data)
#'     plot_axillary_production(data = table, genotype = "Belle_et_Bonne", order = 0, variable = "n")
#' @import ggplot2
#' @import scales
#' @export
plot_axillary_production<-function(data, genotype, order = 0, variable = "n"){

  data <- data[data[, "Genotype"] == genotype & data[, "order"] == order, ]

  plot <- ggplot(data=data,
                 mapping = aes(x = rank, y = unlist(data[,variable]), color = branching_type)) +
    geom_point() +
    geom_line() +
    labs(title = paste("Axillary production distribution for ", genotype, " at order ", order),
         color= "Axillary production") +
    xlab("node rank") +
    scale_x_continuous(breaks = pretty_breaks())+
    theme_publish()

  if (variable == "n"){
    plot + ylab("Frequency")
  }else{
    plot + ylab("Probability")
  }
}

#' Pie Plot distribution of stage at plant scale
#'
#' @description Pie plot distribution of stage at plant scale
#'
#' @param data Dataframe. Dataframe issue from distribution stage function
#' @param genotype string. Genotype name selected
#' @param date string. Date selected
#' @param variable string. n for frequency and percent for percentage, by default n
#'
#' @return Pie plot
#'
#' @usage plot_pie.stage(data, genotype, date, variable)
#' @examples
#'     load("data/module_data.rdata")
#'     df=distribution.stage(module_data)
#'     plot_pie.stage(data = df, genotype = "Belrubi", date = "2022-04-25",variable = "percent")
#'
#'@export
plot_pie.stage<- function(data, genotype, date, variable='n'){

  df = data[data[, "Genotype"] == genotype & data[, "date"] == date,]

  plot <- ggplot(data = df, mapping = aes(x = " ", y = df[,variable],fill=stage))+
    geom_col(color="black")+
    geom_text(aes(label= round(df[,variable],2)), position= position_stack(vjust=0.5))+
    coord_polar("y")+
    scale_fill_brewer(palette = "Pastel1")+
    labs(title = paste("Plant scale stage distribution at", date, "for",genotype))+
    xlab(label = "")+
    ylab(label = "")+
    theme_publish()

  return(plot)
}
