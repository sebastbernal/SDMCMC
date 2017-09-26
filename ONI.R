# Función que identifica eventos consecutivos con ONI > 0.5 durante 4 meses consecutivos
eventosENSO <- function(data) {
  # El primer paso es identificar los eventos ONI >= 0.5
  posicionEN <- which(data$oni >= 0.5)
  posicionLN <- which(data$oni <= -0.5)
  # Calcular la diferencia de posiciónes para determinar si el evento es
  # consecutivo o no con el que le sigue
  consecutivoEN <- c(diff(posicionEN) == 1, FALSE)
  consecutivoLN <- c(diff(posicionLN) == 1, FALSE)
  # Calcular las posiciones iniciales y finales
  posicionInicialEN <- posicionEN[which(c(consecutivoEN[1], diff(consecutivoEN)) == 1)]
  posicionFinalEN <- posicionEN[which(c(consecutivoEN[1], diff(consecutivoEN)) == -1)]
  posicionInicialLN <- posicionLN[which(c(consecutivoLN[1], diff(consecutivoLN)) == 1)]
  posicionFinalLN <- posicionLN[which(c(consecutivoLN[1], diff(consecutivoLN)) == -1)]
  # Calcular la cantidad de eventos
  eventosEN <- posicionFinalEN - posicionInicialEN + 1
  eventosLN <- posicionFinalLN - posicionInicialLN + 1
  validosEN <- which(eventosEN >= 5)
  validosLN <- which(eventosLN >= 5)
  # Generar un indice de posiciones a los que asignarles el identificador de ENSO
  idEN <- unlist(sapply(validosEN,
                        function(i, ti, tf) ti[i]:tf[i],
                        posicionInicialEN, posicionFinalEN))
  idLN <- unlist(sapply(validosLN,
                        function(i, ti, tf) ti[i]:tf[i],
                        posicionInicialLN, posicionFinalLN))
  # Generar la nueva columna en donde va a estar el id ENSO
  data$ENSO <- "Normal"
  # Asignar los identificadores de ENSO en las posiciones calculadas
  data$ENSO[idEN] <- "El Nino"
  data$ENSO[idLN] <- "La Nina"
  # Entregar la base de datos con los identificadores de los eventos ENSO
  return(data)
}