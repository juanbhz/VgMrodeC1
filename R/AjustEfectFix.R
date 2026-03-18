#' Realizar ajustes por efectos fijos de forma interactiva
#'
#' Esta función guía al usuario a través de una interfaz de consola para:
#' 1. Seleccionar variables núcleo (Y, ID, Padre, Madre).
#' 2. Aplicar filtros numéricos opcionales.
#' 3. Definir un modelo lineal inicial.
#' 4. Ejecutar un proceso Stepwise (AIC) para seleccionar efectos significativos.
#' 5. Calcular factores de corrección (FC) y generar una variable ajustada.
#'
#' @param df Un data.frame que contenga todas las variables potenciales para el análisis.
#'
#' @details
#' La función utiliza \code{step()} para la selección de variables. Si el modelo resultante
#' queda vacío (solo intercepto), la función se detiene con un aviso para evitar errores en
#' el cálculo de VIF o ANOVA. Los factores de corrección para variables continuas se basan
#' en la pendiente (beta), mientras que para categóricas se basan en la relación entre
#' medias de niveles.
#'
#' @return Un data.frame (tibble) que contiene las columnas originales, los factores de
#' corrección generados (\code{fc_variable}) y la variable final ajustada (\code{variable_ajust}).
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import cli
#' @importFrom car Anova
#' @importFrom stats as.formula lm coef step
#'
#' @examples
#' \dontrun{
#' # Supongamos que tienes un dataframe llamado 'mis_cerdos'
#' df_ajustado <- AjustEfectFix(mis_cerdos)
#' }

AjustEfectFix <- function(df) {

  # --- FUNCIÓN DE SEGURIDAD (BOTÓN DE PÁNICO) ---
  verificar_salida <- function(entrada) {
    if (toupper(entrada) == "SALIR") {
      cli::cli_alert_danger("Proceso cancelado por el usuario.")
      # Detiene la ejecución de la función padre y vuelve a la consola
      stop("Ejecución finalizada.", call. = FALSE)
    }
    return(entrada)
  }

  pedir_columna_safe <- function(mensaje, df_temp) {
    repeat {
      var <- readline(prompt = paste0(mensaje, " (o escribe 'SALIR'): "))
      var <- verificar_salida(var) # Chequeo inmediato
      if (var %in% names(df_temp)) return(var)
      cli::cli_alert_danger("Variable '{var}' no encontrada.")
    }
  }

  # --- INICIO ---
  cat("\014")
  cli::cli_h1("INTERFAZ DE AJUSTE LINEAL")
  cli::cli_alert_info("Tip: En cualquier momento escribe 'SALIR' para detener todo.\n")

  # 1. Variables Núcleo
  vars_dispo <- names(df)
  cli::cli_inform("Variables: {.var {paste(vars_dispo, collapse = ', ')}}")

  var_y     <- pedir_columna_safe("\n1. Variable Y", df)
  var_id    <- pedir_columna_safe("2. Columna ID", df)
  var_padre <- pedir_columna_safe("3. Columna Padre", df)
  var_madre <- pedir_columna_safe("4. Columna Madre", df)

  nucleo <- c(var_y, var_id, var_padre, var_madre)

  # 2. Sección de Filtros
  repeat {
    cat("\014")
    cli::cli_h1("CONFIGURACIÓN: {var_y}")
    cli::cli_li("ID: {var_id} | P: {var_padre} | M: {var_madre}")
    cat("--------------------------------------------------\n")

    # Mostrar disponibles (excluyendo lo ya usado)
    dispo_actual <- setdiff(names(df), nucleo)
    cli::cli_inform("Disponibles para filtrar: {.var {paste(dispo_actual, collapse = ', ')}}")

    opcion <- utils::menu(c("Filtrar variable numérica", "Continuar al modelo", "SALIR (Abortar)"),
                          title = "\n¿Qué desea hacer?")

    if (opcion == 3) verificar_salida("SALIR")
    if (opcion != 1) break

    v_f <- pedir_columna_safe("Variable a filtrar", df)

    if (is.numeric(df[[v_f]])) {
      cli::cli_inform("Rango de {v_f}: [{min(df[[v_f]], na.rm=T)} - {max(df[[v_f]], na.rm=T)}]")
      v_min <- as.numeric(verificar_salida(readline("Mínimo: ")))
      v_max <- as.numeric(verificar_salida(readline("Máximo: ")))

      n_antes <- nrow(df)
      df <- df %>% dplyr::filter(!!rlang::sym(v_f) >= v_min & !!rlang::sym(v_f) <= v_max)
      cli::cli_alert_success("Filtro aplicado. Quedan {nrow(df)} registros.")
      readline("Presione Enter para continuar...")
    } else {
      cli::cli_alert_warning("No es numérica.")
      readline("Presione Enter...")
    }
  }

  # 3. Predictores
  predictores <- c()
  repeat {
    cat("\014")
    cli::cli_h1("DEFINICIÓN DEL MODELO")
    dispo_modelo <- setdiff(names(df), c(nucleo, predictores))
    cli::cli_inform("Disponibles: {.var {paste(dispo_modelo, collapse = ', ')}}")
    cat("\nSeleccionadas: [", paste(predictores, collapse = " + "), "]\n")

    entrada <- readline(prompt = "Ingrese variable (o 'FIN' para procesar, 'SALIR' para abortar): ")
    entrada <- verificar_salida(entrada)

    if (toupper(entrada) == "FIN") {
      if(length(predictores) > 0) break
      cli::cli_alert_danger("¡Meté al menos una variable, manito!")
      next
    }

    if (entrada %in% dispo_modelo) {
      predictores <- c(predictores, entrada)
    } else {
      cli::cli_alert_danger("Variable no válida o ya seleccionada.")
    }
  }

  # --- 3.5 NATURALEZA DE LAS VARIABLES ---
  cat("\014")
  cli::cli_h1("NATURALEZA DE LOS PREDICTORES")
  for (pred in predictores) {
    # Si ya es texto o factor, R ya sabe que es categórica
    if (is.character(df[[pred]]) || is.factor(df[[pred]])) {
      df[[pred]] <- as.factor(df[[pred]])
      cli::cli_alert_info("'{pred}' detectada y asignada como CATEGÓRICA.")
    } else {
      # Si es numérica, le preguntamos al usuario
      cli::cli_inform("\nLa variable {.var {pred}} es numérica. ¿Cómo deseas tratarla?")
      tipo <- utils::menu(c(
        "Continua (Calculará una pendiente / Beta único)",
        "Categórica (Factor - Calculará efectos por nivel)"
      ), title = paste("Naturaleza de", pred))

      if (tipo == 2) {
        df[[pred]] <- as.factor(df[[pred]])
        cli::cli_alert_success("'{pred}' convertida a CATEGÓRICA.")
      } else if (tipo == 1) {
        cli::cli_alert_success("'{pred}' se mantiene como CONTINUA.")
      } else {
        verificar_salida("SALIR") # Por si le da a la opción 0 para salir
      }
    }
  }
  readline(prompt = "\nPresione Enter para iniciar el ajuste del modelo...")

  # --- 4. CÁLCULOS Y STEPWISE ---
  cat("\014")
  cli::cli_process_start("Construyendo modelo inicial y ejecutando Stepwise...")

  formula_str <- paste(var_y, "~", paste(predictores, collapse = " + "))
  modelo_inicial <- lm(as.formula(formula_str), data = df)
  modelo_step <- step(modelo_inicial, direction = "both", trace = 0)

  cli::cli_process_done()

  predictores_finales <- attr(terms(modelo_step), "term.labels")

  # Si el modelo quedó vacío (sin significancia)
  if (length(predictores_finales) == 0) {
    cli::cli_alert_danger("RESULTADO STEPWISE: No se encontraron efectos significativos.")
    cli::cli_inform("El modelo se redujo a solo el intercepto ({var_y} ~ 1).")

    cat("\n")
    cli::cli_h2("Análisis de Varianza Tipo III (Modelo Inicial)")
    tryCatch({ print(car::Anova(modelo_inicial, type = "III")) },
             error = function(e) cli::cli_alert_warning("Error Anova: {e$message}"))

    cat("\n")
    cli::cli_h2("Pasos de eliminación (Stepwise)")
    print(modelo_step$anova)

    cli::cli_alert_info("Devolviendo el dataframe original sin modificaciones.")
    return(df)
  }

  # --- 5. RESULTADOS DEL MODELO GANADOR ---
  cli::cli_h1("RESULTADOS DEL MODELO FINAL (STEPWISE)")
  cli::cli_alert_success("Variables sobrevivientes: {.var {paste(predictores_finales, collapse = ', ')}}")

  cat("\n")
  cli::cli_h2("1. Significancia de Variables (car::Anova Tipo III)")
  tryCatch({
    print(car::Anova(modelo_step, type = "III"))
  }, error = function(e) cli::cli_alert_warning("Error Anova: {e$message}"))

  cat("\n")
  cli::cli_h2("2. Solución de Efectos Fijos (Betas)")
  print(summary(modelo_step)$coefficients)

  readline(prompt = "\nRevise los resultados arriba. Presione Enter para aplicar los factores de corrección al dataframe...")

  # --- 6. CÁLCULO DE FACTORES (El código que ya arreglamos) ---
  cli::cli_process_start("Calculando factores de corrección...")

  df$variable_ajust <- df[[var_y]]

  for (pred in predictores_finales) {
    col_fc <- paste0("fc_", pred)
    df[[col_fc]] <- 0

    if (is.numeric(df[[pred]])) {
      # Variable Continua
      beta <- coef(modelo_step)[pred]
      if (!is.na(beta)) {
        media_ref <- mean(df[[pred]], na.rm = TRUE)
        df[[col_fc]] <- beta * (media_ref - df[[pred]])
      }
    } else {
      # Variable Categórica (Factor)
      coeficientes <- coef(modelo_step)
      coefs_var <- coeficientes[startsWith(names(coeficientes), pred)]

      for (nombre_coef in names(coefs_var)) {
        beta <- coefs_var[nombre_coef]
        if (!is.na(beta)) {
          nivel <- sub(paste0("^", pred), "", nombre_coef)
          idx <- which(as.character(df[[pred]]) == nivel)
          if (length(idx) > 0) {
            df[[col_fc]][idx] <- -1 * beta
          }
        }
      }
    }
    df$variable_ajust <- df$variable_ajust + df[[col_fc]]
  }

  cli::cli_process_done()
  return(df)
}
