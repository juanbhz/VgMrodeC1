#' Estima valores genéticos individuales en base a un registro propio único
#'
#' Esta función y todas las del paquete están basadas en el cálculo de VGs por
#' ayudas de selección propuestas por Mrode en su libro:
#' LINEAR MODELS FOR THE PREDICTION OF ANIMAL BREEDING VALUES - SECOND EDITION
#'
#' Esta función utiliza para el cálculo del VG una medida única propia de cada animal
#' En la ejecución de la función se pedirá por consola la variable a evaluar
#'
#' @param df data.frame con registros ajustados (recomendado: correr antes AjustEfectFix()) asignados a un id
#' @param h2 Heredabilidad
#' @param i Intensidad de selección
#' @return Data.frame con VG, precisión y ranking
#' @export
#' @import tidyverse

VGs_Mrode_Unic <- function(df, h2, i) {
  # Validar entrada
  if (!is.data.frame(df)) {
    stop("df debe ser de la clase data.frame.")
  }

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

  # Pedir que el usuario seleccione donde está el id
  repeat {
    id <- readline(prompt = "¿En qué columna se encuentra el id del individuo a evaluar?: ")

    if (id %in% names(df)) {
      break
    } else {
      cat("La variable ingresada no existe en el data frame. Intente de nuevo.\n \n")
      cat("Variables disponibles en el data.frame:\n")
      print(names(df))
    }
  }

  # Calcular valores genéticos, precisión, progreso y ranking
  df <- df %>%
    mutate(
      !!paste0("vg_", variable_ajustada, "_Unic") := round(
        h2 * (.data[[variable_ajustada]] - mean(.data[[variable_ajustada]])), 2
      ),
      !!paste0("precision_", variable_ajustada, "_Unic") := sqrt(h2),
      !!paste0("Prog_", variable_ajustada, "_Unic") := i * h2 * sqrt(var(.data[[variable_ajustada]]))
    ) %>%
    arrange(desc(!!sym(paste0("vg_", variable_ajustada, "_Unic")))) %>%
    mutate(
      !!paste0("Rank_", variable_ajustada, "_Unic") := seq_along(.data[[paste0("vg_", variable_ajustada, "_Unic")]])
    ) %>%
    select(
      !!sym(id),
      !!sym(paste0("vg_", variable_ajustada, "_Unic")),
      !!sym(paste0("precision_", variable_ajustada, "_Unic")),
      !!sym(paste0("Rank_", variable_ajustada, "_Unic"))
    )

  return(df)
}
