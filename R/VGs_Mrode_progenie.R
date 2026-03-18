#' Estima valores genéticos individuales en base a registros de progenie
#'
#' Esta función y todas las del paquete están basadas en el cálculo de VGs por
#' ayudas de selección propuestas por Mrode en su libro:
#' LINEAR MODELS FOR THE PREDICTION OF ANIMAL BREEDING VALUES - SECOND EDITION
#'
#' Esta función calcula el VG para padres y madres utilizando el promedio de los
#' registros fenotípicos de sus crías (Half-sib / Full-sib evaluation).
#'
#' @param df data.frame con registros ajustados asignados a un id, además de la información de padre y madre.
#' @param h2 Heredabilidad (valor numérico entre 0 y 1).
#' @param i Intensidad de selección (valor numérico).
#' @return Una lista con dos data.frames: `VG_padres` y `VG_madres`, cada uno con VG, precisión y ranking.
#' @export
#' @import dplyr
#' @import cli

VGs_Mrode_progenie <- function(df, h2, i) {

  # --- Validaciones iniciales ---
  if (!is.data.frame(df)) stop("El argumento 'df' debe ser un data.frame.")
  if (missing(h2) || missing(i)) stop("Debes proporcionar la heredabilidad (h2) y la intensidad (i).")

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

  # --- VALIDACIÓN MATEMÁTICA ---
  if (h2 <= 0 || h2 > 1) stop("Error: La heredabilidad (h2) debe ser una proporción entre 0 y 1 (Ej: 0.15).")

  # --- INICIO DE LA INTERFAZ ---
  cat("\014")
  cli::cli_h1("CÁLCULO DE VALORES GENÉTICOS (POR PROGENIE)")
  cli::cli_alert_info("Metodología: Ayudas de selección de Mrode")
  cli::cli_alert_info("Tip: En cualquier momento escribe 'SALIR' para detener todo.\n")

  vars_dispo <- names(df)
  cli::cli_inform("Variables disponibles: {.var {paste(vars_dispo, collapse = ', ')}}")
  cat("\n")

  # 1. Selección de Variable Fenotípica (Hijos)
  cli::cli_alert_warning("Se recomienda seleccionar la variable ya ajustada por efectos fijos.")
  variable_ajustada <- pedir_columna_safe("1. Variable fenotípica a evaluar (el registro de la cría)", df)
  cli::cli_alert_success("Variable a evaluar: {.var {variable_ajustada}}\n")

  # 2. Selección de Padre y Madre
  col_padre <- pedir_columna_safe("2. Columna que contiene el ID del PADRE", df)
  cli::cli_alert_success("ID Padre configurado: {.var {col_padre}}\n")

  col_madre <- pedir_columna_safe("3. Columna que contiene el ID de la MADRE", df)
  cli::cli_alert_success("ID Madre configurado: {.var {col_madre}}\n")

  # --- PANTALLA DE CONFIRMACIÓN ---
  cat("\014")
  cli::cli_h2("RESUMEN DE PARÁMETROS GENÉTICOS")
  cli::cli_bullets(c(
    "v" = "Rasgo a evaluar (hijos): {.var {variable_ajustada}}",
    "v" = "Columna Padre: {.var {col_padre}}",
    "v" = "Columna Madre: {.var {col_madre}}",
    "v" = "Heredabilidad (h2): {.val {h2}}",
    "v" = "Intensidad (i): {.val {i}}"
  ))

  entrada_conf <- readline(prompt = "\nPresione Enter para iniciar el cálculo (o 'SALIR' para abortar)...")
  verificar_salida(entrada_conf)

  # --- CÁLCULOS ---
  cli::cli_process_start("Agrupando registros de progenie y calculando VGs...")

  # Parámetros globales de la población base (los hijos)
  media_global <- mean(df[[variable_ajustada]], na.rm = TRUE)
  var_global <- var(df[[variable_ajustada]], na.rm = TRUE)
  sd_y <- sqrt(var_global) # Desviación estándar fenotípica

  # 1. Calcular valores genéticos para los PADRES
  vg_padres <- df %>%
    dplyr::filter(!is.na(!!dplyr::sym(col_padre))) %>%
    dplyr::group_by(!!dplyr::sym(col_padre)) %>%
    dplyr::summarise(
      n_hijos = dplyr::n(),
      prom_hijos = mean(.data[[variable_ajustada]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      b = (2 * n_hijos * h2) / (4 + (n_hijos - 1) * h2),
      k = 4 * (1 - h2) / h2,
      vg_padres_prog = round(b * (prom_hijos - media_global), 3),
      precision_padres_prog = round(sqrt(n_hijos / (n_hijos + k)), 4),
      prog_padres_prog = round(i * (n_hijos / (n_hijos + k)) * sd_y, 4)
    ) %>%
    dplyr::arrange(dplyr::desc(vg_padres_prog)) %>%
    dplyr::mutate(rank_padres_prog = dplyr::row_number()) %>%
    dplyr::select(
      !!dplyr::sym(col_padre),
      n_hijos,
      vg_padres_prog,
      precision_padres_prog,
      prog_padres_prog,
      rank_padres_prog
    )

  # 2. Calcular valores genéticos para las MADRES
  vg_madres <- df %>%
    dplyr::filter(!is.na(!!dplyr::sym(col_madre))) %>%
    dplyr::group_by(!!dplyr::sym(col_madre)) %>%
    dplyr::summarise(
      n_hijos = dplyr::n(),
      prom_hijos = mean(.data[[variable_ajustada]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      b = (2 * n_hijos * h2) / (4 + (n_hijos - 1) * h2),
      k = 4 * (1 - h2) / h2,
      vg_madres_prog = round(b * (prom_hijos - media_global), 3),
      precision_madres_prog = round(sqrt(n_hijos / (n_hijos + k)), 4),
      prog_madres_prog = round(i * (n_hijos / (n_hijos + k)) * sd_y, 4)
    ) %>%
    dplyr::arrange(dplyr::desc(vg_madres_prog)) %>%
    dplyr::mutate(rank_madres_prog = dplyr::row_number()) %>%
    dplyr::select(
      !!dplyr::sym(col_madre),
      n_hijos,
      vg_madres_prog,
      precision_madres_prog,
      prog_madres_prog,
      rank_madres_prog
    )

  cli::cli_process_done()

  cli::cli_h1("¡PROCESO COMPLETADO!")
  cli::cli_alert_success("Se evaluaron {nrow(vg_padres)} Padres y {nrow(vg_madres)} Madres.")

  # Retornar ambos resultados como lista
  return(list(
    VG_padres = vg_padres,
    VG_madres = vg_madres
  ))
}
