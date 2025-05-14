#' Realizar ajustes por los efectos fijos de un modelo lineal
#'
#' Esta función toma los valores reales de alguna característica y los ajusta
#' por los efectos significativos que indica el modelo stepwise
#' Además tiene un apartado para realizar filtros de variables numéricas
#'
#' En la ejecución de la función se pedirá por consola la variable a evaluar
#' además también se pedirá por consola información adicional, como variables a filtrar y sus filtros
#' y las variables que quiere añadir al modelo stepwise
#'
#' @param df Un data.frame con información de ID, Padre, Madre, Variable Y, y Variables del modelo
#'
#' @return el dataframe inicial más los factores de ajuste para los efectos utilizados y un Y_ajust
#' @export
#'
#' @import tidyverse
#' @import car

AjustEfectFix <- function(df) {
# Validar que el argumento sea un data.frame
if (!is.data.frame(df)) {
  stop("El argumento debe ser un data.frame.")
}

# Mostrar nombres de columnas disponibles
cat("Aviso: Variables disponibles en el data.frame:\n")
print(names(df))
cat("(Escriba la variable tal cual sale)\n")



# Solicitar nombre de variable a evaluar
repeat {
  variable <- readline(prompt = "¿Cuál variable desea evaluar?: ")

  if (variable %in% names(df)) {
    break
  } else {
    cat("La variable ingresada no existe en el data frame. Intente de nuevo.\n \n")
    cat("Variables disponibles en el data.frame:\n")
    print(names(df))
  }
}

# Solicitar variable donde se encuentra el padre, madre y el id
repeat {
  padre <- readline(prompt = "¿En qué columna se encuentra el padre?: ")

  if (padre %in% names(df)) {
    break
  } else {
    cat("La variable ingresada no existe en el data frame. Intente de nuevo.\n \n")
    cat("Variables disponibles en el data.frame:\n")
    print(names(df))
  }
}

repeat {
  madre <- readline(prompt = "¿En qué columna se encuentra la madre?: ")

  if (madre %in% names(df)) {
    break
  } else {
    cat("La variable ingresada no existe en el data frame. Intente de nuevo.\n \n")
    cat("Variables disponibles en el data.frame:\n")
    print(names(df))
  }
}

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


# Verificar si se harán filtros para las variables
repeat {
  filtrar <- as.logical(readline(prompt = "¿Desea realizar filtros para las variables?: [TRUE / FALSE] "))

  if (filtrar %in% c(TRUE,FALSE)) {
    break
  } else {
    cat("Ingresar solo TRUE o FALSE")
  }
}

# FILTROS

while (filtrar) {
  cat("Variables disponibles en el data.frame:\n")
  print(names(df))
  cat("(Escriba la variable tal cual sale)\n")

  repeat {
    var_filtro <- readline(prompt = "¿Qué variable desea filtrar?: ")

    if (var_filtro %in% names(df)) {
      break
    } else {
      cat("La variable ingresada no existe en el data frame. Intente de nuevo.\n \n")
    }
  }

  cat(paste0("La variable ", var_filtro, " es tipo ", class(df[[var_filtro]]), "\n \n"))

  if (class(df[[var_filtro]]) != "numeric") {
    cat("por ahora solo tenemos filtro para variables numéricas. \n") # Aquí luego podemos filtrar por n dentro de cada nivel o por ds
  } else {
    var_min <- as.numeric(readline(prompt = "Rango mínimo: "))
    var_max <- as.numeric(readline(prompt = "Rango máximo: "))

    df_temp <- df %>%
      filter(df[[var_filtro]] >= var_min & df[[var_filtro]] <= var_max)

    filtrados <- nrow(df) - nrow(df_temp)

    cat(paste0("fueron filtrados ", filtrados, " registros. \n"))
    guardar_filtro <- as.logical(readline(prompt = "¿Desea guardar el filtro?: [TRUE / FALSE] "))

    if(guardar_filtro){
      df <- df_temp
      cat("Filtro realizado \n")
      head(df)
    }
  }
  filtrar <- as.logical(readline(prompt = "¿Desea realizar otro filtro?: [TRUE / FALSE] "))
}

# AJUSTES MULTIPLICATIVOS
cat("\n \n VARIABLES PARA EL MODELO. \n \n")

cat("Aviso: Los ajustes se realizarán en base a las variables significativas en el modelo StepWise \n \n")

# Vector para las variables del modelo
variables_modelo <- c()

repeat {
  cat("Variables disponibles en el data.frame:\n")
  print(names(df))
  nueva_var <- readline(prompt = "Ingrese una variable para el modelo (o escriba FIN para terminar): ")

  if (nueva_var == "FIN") break

  if (!(nueva_var %in% names(df))) {
    cat("La variable no existe en el data.frame.\n")
  } else if (nueva_var == variable) {
    cat("Esa es la variable dependiente. No puede incluirse como predictora.\n")
  } else if (nueva_var == padre) {
    cat("Ese es el padre del individuo. No puede incluirse como predictora.\n")
  } else if (nueva_var == madre) {
    cat("Esa es la madre del individuo. No puede incluirse como predictora.\n")
  } else if (nueva_var == id) {
    cat("Ese es el id. No puede incluirse como predictora.\n")
  } else if (nueva_var %in% variables_modelo) {
    cat("Esa variable ya fue añadida al modelo.\n")
  } else {
    variables_modelo <- c(variables_modelo, nueva_var)
    cat(paste0("Variable '", nueva_var, "' añadida al modelo.\n"))
  }
}

cat("El modelo finalmente quedó: \n")
modelo <- paste0(variable, " ~ ", paste(variables_modelo, collapse = " + "))
cat(modelo)

cat("\n\nAviso: Se eliminarán los NA dentro de las variables del modelo \n")
# Eliminar NA en las variables involucradas
variables_usadas <- c(variable, variables_modelo)
df <- df %>% drop_na(all_of(variables_usadas))


# STEPWISE
cat("\n \n STEPWISE. \n \n")

m1 <- lm(as.formula(paste0(variable, " ~ 1")), data = df)
m2 <- lm(as.formula(paste0(variable, " ~ ", paste(variables_modelo, collapse = " + "))), data = df)

stepw <- step(m1, scope = list(lower = m1, upper = m2), direction = "both")

print(Anova(stepw, type = "III"))
print(vif(stepw))

anova_global <- Anova(stepw, type = "III")
anova_df <- as.data.frame(anova_global)

# Agregar nombres de filas como columna
anova_df$Variable <- rownames(anova_df)

# Filtrar por p < 0.1
variables_significativas <- anova_df %>%
  filter(`Pr(>F)` < 5 & Variable != "(Intercept)") %>%
  pull(Variable)

# Resultado
cat(paste0("\n\nAviso: las variables significativas para ", variable, " fueron: ", paste(variables_significativas, collapse = " - ")))



# AJUSTES
cat("\n \n AJUSTES. \n \n")

# Coeficientes del modelo ajustado con stepwise
coefs <- coef(stepw)

# Media de la variable dependiente (por ejemplo, pd)
media_y <- mean(df[[variable]])

# Vector para guardar los nombres de las columnas de FC
fc_vars <- c()

# Recorrer cada variable significativa
for (var in variables_significativas) {

  nombre_fc <- paste0("fc_", var)

  if (is.numeric(df[[var]])) {
    # Variable continua
    beta <- coefs[var]
    media_var <- mean(df[[var]])

    df <- df %>%
      mutate(!!nombre_fc := media_y / (media_y - beta * (!!sym(var) - media_var)))

    fc_vars <- c(fc_vars, nombre_fc)

  } else {
    # Variable categórica
    tabla_fc <- df %>%
      group_by_at(var) %>%
      summarise(media_var = mean(.data[[variable]], na.rm = TRUE), .groups = "drop") %>%
      mutate(!!nombre_fc := media_var / max(media_var)) %>%
      select(all_of(var), all_of(nombre_fc))

    df <- left_join(df, tabla_fc, by = var)

    fc_vars <- c(fc_vars, nombre_fc)
  }
}

# Aplicar los factores de corrección al valor original
nombre_ajustado <- paste0(variable, "_ajust")

df <- df %>%
  mutate(!!nombre_ajustado := .data[[variable]] * reduce(across(all_of(fc_vars)), `*`))
}


