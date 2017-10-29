# Función para leer datos raw del IDEAM
raw2page <- function(rawdata) {
  # Esta función toma un vector alfanumérico de una página de datos
  # IDEAM y retorna un data.frame tidy
  require("lubridate")
  require("stringr")
  require("magrittr")
  # Definir los encabezado como (fila, inicio, fin)
  headerBounds <- rbind(scale = cbind(3, 45, 51),
                        variable = cbind(3, 64, 100),
                        year = cbind(5, 60, 63),
                        stationId = cbind(5, 105, 112),
                        stationName = cbind(5, 115, 132),
                        grados_lat = cbind(7,16,17),
                        min_lat = cbind(7,18,19),
                        grados_long = cbind(8,16,17),
                        min_long = cbind(8,18,19),
                        elev = cbind(9, 16, 19),
                        dpto = cbind(7,81,103),
                        mpio = cbind(8,81,103),
                        corriente = cbind(9, 81, 103))
  colnames(headerBounds) <- c("line","start","end")
  framebounds <- as.data.frame(headerBounds)
  # Obtener los datos del encabezado
  framedata <- as.data.frame(rbind(with(framebounds, substr(rawdata[line],start,end))))
  colnames(framedata) <- c("scale", "variable", "year",
                           "stationId", "stationName",
                           "grados_lat", "min_lat",
                           "grados_long", "min_long",
                           "elev", "dpto", "mpio",
                           "corriente")
  # Recortar los espacios en blanco de los datos
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  framedata$stationName <- trim(framedata$stationName)
  framedata$elev <- trim(framedata$elev)
  framedata$dpto <- trim(framedata$dpto)
  framedata$mpio <- trim(framedata$mpio)
  framedata$corriente <- trim(framedata$corriente)
  framedata$variable <- framedata$variable %>%
    str_replace("\\(.*\\)", "") %>%
    trim() %>%
    str_replace(" ", ".")
  framedata$scale <- trim(framedata$scale)
  # Multiplicar la columna para todos los días del año
  st <- as.Date(paste(framedata$year,"-01-01",sep=""))
  en <- as.Date(paste(framedata$year,"-12-31",sep=""))
  date <- seq(as.Date(st),as.Date(en), by=1)
  pagedata <- cbind(framedata,date)
  # Definir las ubicaciones finales de los datos para cada mes
  mboundaries<-c(25,34,43,52,61,70,79,88,97,106,115,124)
  # Usar las fechas como coordenadas para leer los valores respectivos
  value <- as.numeric(substr(rawdata[14+mday(pagedata$date)],
                             mboundaries[month(pagedata$date)]-5,
                             mboundaries[month(pagedata$date)]))
  # Unir los datos al data.frame con el encabezado
  page <- cbind(pagedata, value)
  # Retornar el tidy data.frame
  page
}

# Función que lee un archivo de lluvia en formato IDEAM
# y lo transforma en un data.frame
read_IDEAM <- function(IDEAM_file, linesPerPage = 45) {
  require(dplyr)
  require(tidyr)
  IDEAM_raw <- readLines(IDEAM_file)
  headers <- grep("I D E A M  - ", IDEAM_raw)
  IDEAM <- bind_rows(lapply(headers, function(x) {
    start <- x
    end <- start + linesPerPage
    raw2page(IDEAM_raw[start:end])
  })) %>%
    transmute(fecha = ymd(date),
              ano = as.integer(year(fecha)),
              mes = factor(month(fecha), labels = month.abb),
              dia = mday(fecha),
              cod = as.integer(as.character(stationId)),
              estacion = stationName,
              grados_lat = as.integer(as.character(grados_lat)),
              min_lat = as.integer(as.character(min_lat)),
              grados_long = as.integer(as.character(grados_long)),
              min_long = as.integer(as.character(min_long)),
              elev = as.numeric(elev),
              dpto,
              mpio,
              corriente,
              variable = paste(variable, scale, sep = "."),
              value) %>%
    spread(variable, value) %>%
    arrange(fecha)
  # names(IDEAM)[ncol(IDEAM)] <- valueName
  return(IDEAM)
}