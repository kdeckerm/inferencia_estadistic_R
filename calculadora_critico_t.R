# Cargar el paquete tcltk
if (!requireNamespace("tcltk", quietly = TRUE)) install.packages("tcltk")
library(tcltk)

# Crear ventana principal
ventana <- tktoplevel()
tkwm.title(ventana, "Calculadora gráfica de t crítico")

# Variables para entrada
nivel_confianza_var <- tclVar("99.5")
n_var <- tclVar("120")

# Crear widgets de entrada
tkpack(tklabel(ventana, text = "Nivel de confianza (%)"))
entrada_conf <- tkentry(ventana, textvariable = nivel_confianza_var)
tkpack(entrada_conf)

tkpack(tklabel(ventana, text = "Tamaño de la muestra"))
entrada_n <- tkentry(ventana, textvariable = n_var)
tkpack(entrada_n)

# Etiqueta para mostrar resultado
resultado <- tklabel(ventana, text = "")
tkpack(resultado)

# Función para calcular t crítico
calcular_t <- function() {
  # Leer y validar entradas
  nc_pct <- suppressWarnings(as.numeric(tclvalue(nivel_confianza_var)))
  n <- suppressWarnings(as.numeric(tclvalue(n_var)))
  
  if (is.na(nc_pct) || is.na(n) || nc_pct <= 0 || nc_pct >= 100 || n < 2) {
    tkconfigure(resultado, text = "⚠️ Ingrese valores válidos (confianza entre 0 y 100, muestra ≥ 2)")
    return()
  }
  
  # Conversión a decimal y cálculo del cuantil correcto
  nc <- nc_pct / 100
  df <- n - 1
  alfa <- 1 - nc
  cuantil <- 1 - alfa / 2
  
  # Cálculo del valor t crítico
  t_critico <- stats::qt(cuantil, df)
  
  # Mostrar resultados
  texto <- paste("✅ t crítico =", round(t_critico, 4),
                 "\nConfianza:", nc_pct, "%",
                 "\nNivel (decimal):", round(nc, 4),
                 "\nGrados de libertad:", df,
                 "\nCuantil usado:", round(cuantil, 5))
  tkconfigure(resultado, text = texto)
}

# Botón para ejecutar cálculo
tkpack(tkbutton(ventana, text = "Calcular", command = calcular_t))

