#' Estima valores genéticos individuales en base a un registro propio único
#'
#' Esta función y todas las del paquete están basadas en el cálculo de VGs por
#' ayudas de selección propuestas por Mrode en su libro:
#' LINEAR MODELS FOR THE PREDICTION OF ANIMAL BREEDING VALUES - SECOND EDITION
#'
#' @param df data.frame con registros ajustados asignados a un id.
#' @param h2 Heredabilidad (valor numérico entre 0 y 1).
#' @param i Intensidad de selección (valor numérico).
#' @return Data.frame con VG, precisión, progreso genético y ranking.
#' @export
#' @import dplyr
#' @import cli

VGs_Mrode_Unic <- function(df, h2, i) {

  # --- Validaciones iniciales ---
  if (!is.data.frame(df)) stop("El argumento 'df' debe ser un data.frame.")
  if (missing(h2) || missing(i)) stop("Debes proporcionar la heredabilidad (h2) y la intensidad de selección (i).")

  # --- FUNCIÓN DE SEGURIDAD (BOTÓN DE PÁNICO) ---
  verificar_salida <- function(entrada) {
    if (toupper(entrada) == "SALIR") {
      cli::cli_alert_danger("Proceso cancelado por el usuario.")
      stop("Ejecución finalizada.", call. = FALSE)
    }
    return(entrada)
  }

  pedir_columna_safe <- function(mensaje, df_temp) {
    repeat {
      var <- readline(prompt = paste0(mensaje, " (o escribe 'SALIR'): "))
      var <- verificar_salida(var)
      if (var %in% names(df_temp)) return(var)
      cli::cli_alert_danger("Variable '{var}' no encontrada.")
    }
  }

  # --- INICIO DE LA INTERFAZ ---
  cat("\014")
  cli::cli_h1("CÁLCULO DE VALORES GENÉTICOS (REGISTRO ÚNICO)")
  cli::cli_alert_info("Metodología: Ayudas de selección de Mrode")
  cli::cli_alert_info("Tip: En cualquier momento escribe 'SALIR' para detener todo.\n")

  vars_dispo <- names(df)
  cli::cli_inform("Variables disponibles: {.var {paste(vars_dispo, collapse = ', ')}}")
  cat("\n")

  # 1. Selección de ID
  id <- pedir_columna_safe("1. Columna que contiene el ID del individuo", df)
  cli::cli_alert_success("ID configurado: {.var {id}}\n")

  # 2. Selección de Variable Fenotípica
  cli::cli_alert_warning("Se recomienda seleccionar la variable ya ajustada por efectos fijos.")
  variable_ajustada <- pedir_columna_safe("2. Variable fenotípica a evaluar", df)
  cli::cli_alert_success("Variable a evaluar: {.var {variable_ajustada}}\n")

  # --- PANTALLA DE CONFIRMACIÓN ---
  cat("\014")
  cli::cli_h2("RESUMEN DE PARÁMETROS GENÉTICOS")
  cli::cli_bullets(c(
    "v" = "Individuos (ID): {.var {id}}",
    "v" = "Rasgo a evaluar: {.var {variable_ajustada}}",
    "v" = "Heredabilidad (h2): {.val {h2}}",
    "v" = "Intensidad (i): {.val {i}}"
  ))

  entrada_conf <- readline(prompt = "\nPresione Enter para iniciar el cálculo (o 'SALIR' para abortar)...")
  verificar_salida(entrada_conf)

  # --- CÁLCULOS ---
  cli::cli_process_start("Calculando VG, Precisión y Ranking...")

  # Nombres dinámicos para las columnas de salida
  col_vg   <- paste0("vg_", variable_ajustada, "_Unic")
  col_prec <- paste0("precision_", variable_ajustada, "_Unic")
  col_prog <- paste0("Prog_", variable_ajustada, "_Unic")
  col_rank <- paste0("Rank_", variable_ajustada, "_Unic")

  # Ejecución de la matemática usando tidyverse
  df_resultado <- df %>%
    dplyr::mutate(
      # VG = h2 * (Observación - Media poblacional)
      !!sym(col_vg) := round(h2 * (.data[[variable_ajustada]] - mean(.data[[variable_ajustada]], na.rm = TRUE)), 3),

      # Precisión = Raíz cuadrada de la heredabilidad
      !!sym(col_prec) := round(sqrt(h2), 4),

      # Progreso = i * h2 * Desviación estándar del rasgo
      !!sym(col_prog) := round(i * h2 * sqrt(var(.data[[variable_ajustada]], na.rm = TRUE)), 4)
    ) %>%
    # Ordenar de mayor a menor VG para establecer el ranking
    dplyr::arrange(dplyr::desc(!!sym(col_vg))) %>%
    dplyr::mutate(
      !!sym(col_rank) := dplyr::row_number()
    ) %>%
    # Seleccionar solo las columnas de interés genético
    dplyr::select(
      !!dplyr::sym(id),
      !!dplyr::sym(col_vg),
      !!dplyr::sym(col_prec),
      !!dplyr::sym(col_prog),
      !!dplyr::sym(col_rank)
    )

  cli::cli_process_done()

  cli::cli_h1("¡PROCESO COMPLETADO!")
  cli::cli_alert_success("Se evaluaron {nrow(df_resultado)} registros.")

  return(df_resultado)
}
