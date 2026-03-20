#' Clasificar la pendiente a partir de un DEM
#'
#' Esta función calcula la pendiente desde un modelo digital de elevaciones (DEM)
# Aquí empieza la descripción larga.
#' y la clasifica en cuatro categorías:
#' 1 = llana, 2 = suave, 3 = moderada, 4 = fuerte.
#'
#' @param dem Un objeto `terra::SpatRaster` que representa un DEM.
#' @param unit Unidad de la pendiente. Por defecto `"degrees"`.
#'
#' @return Un `terra::SpatRaster` con clases de pendiente.
#' @export

clase_pendiente <- function(dem, unit = "degrees") {
  # Se define la función llamada clase_pendiente.

  if (!inherits(dem, "SpatRaster")) {
    # Comprueba si el objeto 'dem' es de clase SpatRaster.

    stop("`dem` debe ser un SpatRaster.")
    # Detiene la función y muestra este error.
    # Sirve para evitar que el usuario pase algo incorrecto.
  }

  slope <- terra::terrain(dem, v = "slope", unit = unit)
  # Calcula la pendiente a partir del DEM usando terra::terrain().
  # - dem: raster de entrada
  # - v = "slope": indica que queremos la pendiente
  # - unit = unit: usa la unidad elegida por el usuario
  # El resultado es un raster continuo con valores de pendiente.

  out <- slope
  # Crea una copia del raster de pendiente.
  # 'out' será el raster final reclasificado.

  out[] <- NA
  # Sustituye todos los valores de 'out' por NA.
  # Así partimos de un raster vacío pero con la misma geometría

  out[slope < 5] <- 1
  # A todas las celdas donde la pendiente sea menor que 5, les asigna la clase 1.

  out[slope >= 5 & slope < 15] <- 2
  # A las celdas con pendiente entre 5 y menor que 15, les asigna la clase 2.

  out[slope >= 15 & slope < 30] <- 3
  # A las celdas con pendiente entre 15 y menor que 30, les asigna la clase 3.

  out[slope >= 30] <- 4
  # A las celdas con pendiente igual o mayor que 30, les asigna la clase 4.

  levels(out) <- data.frame(
    # Aquí se define la tabla de categorías del raster.

    value = 1:4,
    # Columna con los valores que existen en el raster: 1, 2, 3 y 4.

    class = c("llana", "suave", "moderada", "fuerte")
    # Columna con la etiqueta asociada a cada valor.
    # Así, 1 equivale a "llana", 2 a "suave", etc.
  )

  names(out) <- "slope_class"
  # Cambia el nombre de la capa raster.
  # Esto ayuda a que la salida quede más clara.

  return(out)
  # Devuelve el raster final ya clasificado.
}

