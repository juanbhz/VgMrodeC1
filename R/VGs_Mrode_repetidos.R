#' Estima valores genéticos individuales en base a registros propios repetidos (variables que se toman varias veces durante la vida del individuo)
#'
#' Esta función y todas las del paquete están basadas en el cálculo de VGs por
#' ayudas de selección propuestas por Mrode en su libro:
#' LINEAR MODELS FOR THE PREDICTION OF ANIMAL BREEDING VALUES - SECOND EDITION
#'
#' Esta función utiliza para el cálculo del VG propio basado en muestras repetidas para una variable
#'
#' En la ejecución de la función se pedirá por consola la variable a evaluar
#' y la columna en la cuál se encuentran el id del individuo a evaluar
#'
#' Esta función además pide como entrada un valor de repetibilidad, para la variable a evaluar
#'
#' @param df data.frame con registros ajustados (recomendado: correr antes AjustEfectFix()) asignados a un id, además información de padre y madre
#' @param h2 Heredabilidad
#' @param i Intensidad de selección
#' @param repet Repetibilidad
#' @return Data.frame con VG, precisión y ranking
#' @export
#' @import tidyverse

VGs_Mrode_repetidos <- function(df, h2, repet, i) {
  # Mostrar columnas
  cat("Variables disponibles en el data.frame:\n")
  print(names(df))

  # Solicitar variable cruda (ej. IEP)
  repeat {
    variable_cruda <- readline(prompt = "¿Cuál es la variable cruda (varios registros por individuo)?: ")
    if (variable_cruda %in% names(df)) break
    cat("Variable no encontrada. Intente de nuevo.\n")
  }

  # Solicitar columna ID
  repeat {
    id <- readline(prompt = "¿En qué columna está el ID del individuo a evaluar?: ")
    if (id %in% names(df)) break
    cat("Columna no encontrada. Intente de nuevo.\n")
  }

  # Nombres dinámicos
  vg_col <- paste0("vg_", variable_cruda, "_rep")
  precision_col <- paste0("precision_", variable_cruda, "_rep")
  prog_col <- paste0("Prog_", variable_cruda, "_rep")
  rank_col <- paste0("Rank_", variable_cruda, "_rep")

  # Calcular media global sobre la variable cruda (antes de promediar)
  media_global <- mean(df[[variable_cruda]], na.rm = TRUE)
  var_global <- var(df[[variable_cruda]], na.rm = TRUE)

  # Calcular promedio individual y n de repeticiones
  df_agg <- df %>%
    group_by_at(id) %>%
    summarise(
      n = n(),
      promedio_indiv = mean(.data[[variable_cruda]], na.rm = TRUE),
      .groups = "drop"
    )

  # Aplicar cálculos
  df_result <- df_agg %>%
    mutate(
      b = (n * h2) / (1 + (n - 1) * repet),
      !!vg_col := b * (promedio_indiv - media_global),
      !!precision_col := sqrt((n * h2) / (1 + (n - 1) * repet)),
      !!prog_col := i * (4.8 * h2 / (1 + (4.8 - 1) * repet)) * sqrt(var_global)
    ) %>%
    arrange(desc(!!sym(vg_col))) %>%
    mutate(!!rank_col := row_number()) %>%
    select(
      !!sym(id),
      !!sym(vg_col),
      !!sym(rank_col),
      !!sym(precision_col),
      !!sym(prog_col)
    )

  return(df_result)
}
