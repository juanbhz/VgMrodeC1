#' Estima valores genéticos individuales en base a registros propios repetidos
#'
#' Esta función y todas las del paquete están basadas en el cálculo de VGs por
#' ayudas de selección propuestas por Mrode en su libro:
#' LINEAR MODELS FOR THE PREDICTION OF ANIMAL BREEDING VALUES - SECOND EDITION
#'
#' Esta función utiliza para el cálculo del VG propio basado en muestras repetidas
#' para una variable (ej. peso al destete de diferentes camadas de una misma cerda).
#'
#' @param df data.frame con registros ajustados asignados a un id.
#' @param h2 Heredabilidad (valor numérico entre 0 y 1).
#' @param repet Repetibilidad (valor numérico entre 0 y 1).
#' @param i Intensidad de selección (valor numérico).
#' @return Data.frame con VG, precisión, progreso genético y ranking por individuo.
#' @export
#' @import dplyr
#' @import cli

VGs_Mrode_repetidos <- function(df, h2, repet, i) {

  # --- Validaciones iniciales ---
  if (!is.data.frame(df)) stop("El argumento 'df' debe ser un data.frame.")
  if (missing(h2) || missing(repet) || missing(i)) {
    stop("Debes proporcionar la heredabilidad (h2), la repetibilidad (repet) y la intensidad (i).")
  }

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

  # --- VALIDACIONES MATEMÁTICAS Y BIOLÓGICAS ---
  if (h2 < 0 || h2 > 1) stop("Error: La heredabilidad (h2) debe ser una proporción entre 0 y 1 (Ej: 0.15, no 15).")
  if (repet < 0 || repet > 1) stop("Error: La repetibilidad (repet) debe ser una proporción entre 0 y 1.")

  if (h2 > repet) {
    cli::cli_alert_warning("¡ALERTA BIOLÓGICA! La heredabilidad ({h2}) es mayor que la repetibilidad ({repet}).")
    cli::cli_inform("Esto viola los supuestos genéticos y generará precisiones > 1.")
    entrada_alerta <- readline(prompt = "Presione Enter si desea continuar de todos modos, o 'SALIR' para abortar: ")
    verificar_salida(entrada_alerta)
  }

  # --- INICIO DE LA INTERFAZ ---
  cat("\014")
  cli::cli_h1("CÁLCULO DE VALORES GENÉTICOS (REGISTROS REPETIDOS)")
  cli::cli_alert_info("Metodología: Ayudas de selección de Mrode")
  cli::cli_alert_info("Tip: En cualquier momento escribe 'SALIR' para detener todo.\n")

  vars_dispo <- names(df)
  cli::cli_inform("Variables disponibles: {.var {paste(vars_dispo, collapse = ', ')}}")
  cat("\n")

  # 1. Selección de ID
  id <- pedir_columna_safe("1. Columna que contiene el ID del individuo (madre/padre/etc)", df)
  cli::cli_alert_success("ID configurado: {.var {id}}\n")

  # 2. Selección de Variable Fenotípica
  cli::cli_alert_warning("Se recomienda seleccionar la variable ya ajustada por efectos fijos.")
  variable_cruda <- pedir_columna_safe("2. Variable fenotípica a evaluar (con varios registros por ID)", df)
  cli::cli_alert_success("Variable a evaluar: {.var {variable_cruda}}\n")

  # --- PANTALLA DE CONFIRMACIÓN ---
  cat("\014")
  cli::cli_h2("RESUMEN DE PARÁMETROS GENÉTICOS")
  cli::cli_bullets(c(
    "v" = "Agrupación (ID): {.var {id}}",
    "v" = "Rasgo a evaluar: {.var {variable_cruda}}",
    "v" = "Heredabilidad (h2): {.val {h2}}",
    "v" = "Repetibilidad (r): {.val {repet}}",
    "v" = "Intensidad (i): {.val {i}}"
  ))

  entrada_conf <- readline(prompt = "\nPresione Enter para iniciar el cálculo (o 'SALIR' para abortar)...")
  verificar_salida(entrada_conf)

  # --- CÁLCULOS ---
  cli::cli_process_start("Agrupando registros y calculando VGs...")

  # Nombres dinámicos
  vg_col <- paste0("vg_", variable_cruda, "_rep")
  precision_col <- paste0("precision_", variable_cruda, "_rep")
  prog_col <- paste0("Prog_", variable_cruda, "_rep")
  rank_col <- paste0("Rank_", variable_cruda, "_rep")

  # 1. Parámetros globales de la población
  media_global <- mean(df[[variable_cruda]], na.rm = TRUE)
  var_global <- var(df[[variable_cruda]], na.rm = TRUE)

  # 2. Agrupación por individuo
  df_agg <- df %>%
    dplyr::group_by(!!dplyr::sym(id)) %>%
    dplyr::summarise(
      n = dplyr::n(),
      promedio_indiv = mean(.data[[variable_cruda]], na.rm = TRUE),
      .groups = "drop"
    )

  # Calcular el promedio real de registros por individuo en esta población
  n_promedio_pob <- mean(df_agg$n, na.rm = TRUE)

  # 3. Aplicar cálculos matemáticos
  df_result <- df_agg %>%
    dplyr::mutate(
      b = (n * h2) / (1 + (n - 1) * repet),
      !!dplyr::sym(vg_col) := round(b * (promedio_indiv - media_global), 3),
      !!dplyr::sym(precision_col) := round(sqrt(b), 4),
      !!dplyr::sym(prog_col) := round(i * sqrt((n_promedio_pob * h2) / (1 + (n_promedio_pob - 1) * repet)) * sqrt(var_global), 4)
    ) %>%
    dplyr::arrange(dplyr::desc(!!dplyr::sym(vg_col))) %>%
    dplyr::mutate(
      !!dplyr::sym(rank_col) := dplyr::row_number()
    ) %>%
    dplyr::select(
      !!dplyr::sym(id),
      n,
      !!dplyr::sym(vg_col),
      !!dplyr::sym(precision_col),
      !!dplyr::sym(prog_col),
      !!dplyr::sym(rank_col)
    )

  cli::cli_process_done()

  cli::cli_h1("¡PROCESO COMPLETADO!")
  cli::cli_alert_success("Se evaluaron {nrow(df_result)} individuos a partir de {nrow(df)} registros totales.")
  cli::cli_alert_info("Promedio de registros por individuo: {.val {round(n_promedio_pob, 1)}}")

  return(df_result)
}
