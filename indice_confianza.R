# Cargar el paquete tcltk
if (!requireNamespace("tcltk", quietly = TRUE)) install.packages("tcltk")
library(tcltk)

# Crear ventana principal
ventana <- tktoplevel()
tkwm.title(ventana, "Simulación de satisfacción de clientes")

# Variables para entrada
n_var <- tclVar("126")
media_var <- tclVar("74")
desv_var <- tclVar("8")

# Entradas gráficas
tkpack(tklabel(ventana, text = "Número de encuestas"))
tkpack(tkentry(ventana, textvariable = n_var))

tkpack(tklabel(ventana, text = "Promedio de satisfacción (esperado)"))
tkpack(tkentry(ventana, textvariable = media_var))

tkpack(tklabel(ventana, text = "Desviación estándar"))
tkpack(tkentry(ventana, textvariable = desv_var))

# Resultado
txt_resultado <- tclVar("")
resultado <- tklabel(ventana, textvariable = txt_resultado, justify = "left")
tkpack(resultado)

# Función para simular, calcular IC y graficar
graficar_satisfaccion <- function() {
  n <- as.numeric(tclvalue(n_var))
  media_esperada <- as.numeric(tclvalue(media_var))
  desv <- as.numeric(tclvalue(desv_var))
  
  if (any(is.na(c(n, media_esperada, desv))) || n < 2) {
    tclvalue(txt_resultado) <- "⚠️ Ingrese valores numéricos válidos"
    return()
  }
  
  set.seed(123)
  satisfaccion <- rnorm(n, mean = media_esperada, sd = desv)
  
  media <- mean(satisfaccion)
  sd_muestral <- sd(satisfaccion)
  df <- n - 1
  error <- stats::qt(0.975, df = df) * sd_muestral / sqrt(n)
  ic_inf <- media - error
  ic_sup <- media + error
  
  # Mostrar resultados
  texto <- paste("Media muestral:", round(media, 2),
                 "\nIC 95%: [", round(ic_inf, 2), ",", round(ic_sup, 2), "]")
  tclvalue(txt_resultado) <- texto
  
  # Mostrar ventana gráfica con histograma
  dev.new()  # Abre ventana de gráficos
  hist(satisfaccion,
       breaks = 15,
       col = "skyblue",
       border = "black",
       main = "Distribución de la Satisfacción de Clientes",
       xlab = "Puntaje de satisfacción",
       ylab = "Frecuencia")
  
  abline(v = media, col = "red", lwd = 2, lty = 2)
  abline(v = ic_inf, col = "darkgreen", lwd = 2, lty = 3)
  abline(v = ic_sup, col = "darkgreen", lwd = 2, lty = 3)
  
  legend("topright",
         legend = c(paste("Media =", round(media, 2)),
                    paste("IC 95% Inf =", round(ic_inf, 2)),
                    paste("IC 95% Sup =", round(ic_sup, 2))),
         col = c("red", "darkgreen", "darkgreen"),
         lty = c(2, 3, 3), lwd = 2, box.lty = 0)
}

# Botón para ejecutar
boton <- tkbutton(ventana, text = "Simular y graficar", command = graficar_satisfaccion)
tkpack(boton)