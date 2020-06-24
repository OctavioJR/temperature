#' Fahrenheit to Kelvin conversion
#'
#' This function takes a temperature expressed in Fahrenheit and convert it to Kelvin.
#'
#' @param temp A numeric variable. A temperature expressed in Fahrenheit
#'
#'@examples
#' fahr_to_kelvin(100)
#'
#' @export
fahr_to_kelvin <- function(temp) {
  stopifnot(is.numeric(temp))
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}



#' Kelvin to Celsius conversion
#'
#' This function takes a temperature expressed in Kelvin and convert it to celsius.
#'
#' @param temp A numeric variable. A temperature expressed in kelvin

#'@examples
#' kelvin_to_celsius(273.15)
#'
#' @export
kelvin_to_celsius <- function(temp) {
  celsius <- temp - 273.15
  return(celsius)
}



#' Celsius to Kelvin conversion
#'
#' This function takes a temperature expressed in celsius and convert it to Kelvin.
#'
#' @param temp A numeric variable. A temperature expressed in celsius
#'
#'@examples
#' celsius_to_kelvin(0)
#'
#' @export
celsius_to_kelvin <- function(temp) {
  kelvin <- temp + 273.15
  return(kelvin)
}


#' Convert wide to long temperature datasets
#'
#' Convert wide to long temperature datasets
#'
#' @param dat Input data in wide format
#' @param id.vars x
#' @param variable.name x
#' @param value.name x
#'
#'@examples
#' data(tuggeranong)
#' Wide_To_Long_Temperature(tuggeranong)
#'
#' @export
Wide_To_Long_Temperature <- function(dat, id.vars="Year",
                                     variable.name = "Month",
                                     value.name = "Temperature"){
  # require(reshape2) #  this is not correct, because packages should not be loaded internally in functions
  stopifnot(expr={is.data.frame(dat)
    id.vars %in% colnames(dat)
  })
  melttemp <- reshape2::melt(dat, id.vars = id.vars,
                   variable.name = variable.name,
                   value.name = value.name)
  return(melttemp)
}




#' Plots temperatues of every month?
#'
#' Convert wide to long temperature datasets
#'
#' @param dat Input data in wide format
#'
#'@examples
#'data(tuggeranong)
#'plot_trend(tuggeranong)
#'
#'@export
plot_trend <- function(dat){
  melttemp <- Wide_To_Long_Temperature(dat)
  #model <- summary(lm(Temperature ~ Year + Month, data = melttemp))
  model <- summary(stats::lm(melttemp$Temperature ~ melttemp$Year + melttemp$Month))
  year_effect <- round(model$coefficients["Year",],3)
  #library(ggplot2)
  ggplot2::ggplot(data=melttemp,
                  ggplot2::aes(x=melttemp$Year,y=melttemp$Temperature, color=melttemp$Month))+
    ggplot2::geom_point() + ggplot2::geom_smooth(alpha=0.2) +
    ggplot2::annotate("label", x = mean(melttemp$Year),
             y = max(melttemp$Temperature),
             label = paste0(ifelse(year_effect[1]>=0, "+", "-"),
                            year_effect[1], "\u2103 C/year, p=",year_effect[4]))
}
