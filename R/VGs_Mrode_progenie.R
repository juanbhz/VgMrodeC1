#' Estima valores genéticos individuales en base a registros de progenie (para padres y madres)
#'
#' Esta función y todas las del paquete están basadas en el cálculo de VGs por
#' ayudas de selección propuestas por Mrode en su libro:
#' LINEAR MODELS FOR THE PREDICTION OF ANIMAL BREEDING VALUES - SECOND EDITION
#'
#' Esta función utiliza para el cálculo del VG para padres y madres, utilizando los registros de
#' sus diferentes crías.
#'
#' En la ejecución de la función se pedirá por consola la variable a evaluar
#' y la columna en la cuál se encuentran los padres y madres
#'
#' @param df data.frame con registros ajustados (recomendado: correr antes AjustEfectFix()) asignados a un id, además información de padre y madre
#' @param h2 Heredabilidad
#' @param i Intensidad de selección
#' @return Data.frame con VG, precisión y ranking
#' @export
#' @import tidyverse

VGs_Mrode_progenie <- function(df, h2, i) {
  # Validación básica
  if (!is.data.frame(df)) stop("El argumento 'df' debe ser un data.frame.")

  # Mostrar nombres de columnas disponibles
  cat("Aviso: Variables disponibles en el data.frame:\n")
  print(names(df))
  cat("(Escriba la variable tal cual sale)\n")

  cat("\n\nCALCULO DE VALORES GENÉTICOS\n")
  cat("Se recomienda seleccionar la variable ya ajustada por los efectos fijos\n")

  # Solicitar la variable sobre la cual se calcularán los valores genéticos
  repeat {
    variable_ajustada <- readline(prompt = "¿Cuál variable desea usar para calcular los valores genéticos?: ")

    if (variable_ajustada %in% names(df)) {
      break
    } else {
      cat("La variable ingresada no existe en el data.frame. Intente de nuevo.\n")
      print(names(df))
    }
  }

  # Pedir que el usuario seleccione donde está el padre
  repeat {
    col_padre <- readline(prompt = "¿En qué columna se encuentra el padre?: ")

    if (col_padre %in% names(df)) {
      break
    } else {
      cat("La variable ingresada no existe en el data frame. Intente de nuevo.\n \n")
      cat("Variables disponibles en el data.frame:\n")
      print(names(df))
    }
  }

  # Pedir que el usuario seleccione donde está la madre
  repeat {
    col_madre <- readline(prompt = "¿En qué columna se encuentra la madre: ")

    if (col_madre %in% names(df)) {
      break
    } else {
      cat("La variable ingresada no existe en el data frame. Intente de nuevo.\n \n")
      cat("Variables disponibles en el data.frame:\n")
      print(names(df))
    }
  }

  # Calcular valores genéticos para los PADRES
  vg_padres <- df %>%
    group_by_at(col_padre) %>%
    summarise(
      n = n(),
      prom_hijos = mean(.data[[variable_ajustada]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      b = (2 * n * h2) / (4 + (n - 1) * h2),
      vg = b * (prom_hijos - mean(df[[variable_ajustada]], na.rm = TRUE)),
      k = 4 * (1 - h2) / h2,
      precision = sqrt(n / (n + k)),
      progreso = i * (n / (n + k)) * sqrt(var(df[[variable_ajustada]], na.rm = TRUE))
    ) %>%
    arrange(desc(vg)) %>%
    mutate(rank = row_number()) %>%
    select(
      !!sym(col_padre),
      vg_padres_prog = vg,
      precision_padres_prog = precision,
      rank_padres_prog = rank
    )

  # Calcular valores genéticos para las MADRES
  vg_madres <- df %>%
    group_by_at(col_madre) %>%
    summarise(
      n = n(),
      prom_hijos = mean(.data[[variable_ajustada]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      b = (2 * n * h2) / (4 + (n - 1) * h2),
      vg = b * (prom_hijos - mean(df[[variable_ajustada]], na.rm = TRUE)),
      k = 4 * (1 - h2) / h2,
      precision = sqrt(n / (n + k)),
      progreso = i * (n / (n + k)) * sqrt(var(df[[variable_ajustada]], na.rm = TRUE))
    ) %>%
    arrange(desc(vg)) %>%
    mutate(rank = row_number()) %>%
    select(
      !!sym(col_madre),
      vg_madres_prog = vg,
      precision_madres_prog = precision,
      rank_madres_prog = rank
    )

  # Retornar ambos resultados como lista
  return(list(
    VG_padres = vg_padres,
    VG_madres = vg_madres
  ))
}
