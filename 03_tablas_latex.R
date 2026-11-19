# Exporta las tablas del artículo en formato LaTeX (booktabs) para la versión IEEEtran.
# Requiere haber corrido 01_analisis.R (resultados_articulo.rds).
library(tidyverse)
source("graficos.R")
r <- readRDS("resultados_articulo.rds")

dir.create("latex/tablas", recursive = TRUE, showWarnings = FALSE)

a_latex <- function(tab, align, archivo) {
  x <- knitr::kable(tab, format = "latex", booktabs = TRUE, align = align,
                    linesep = "", escape = TRUE)
  writeLines(as.character(x), file.path("latex/tablas", archivo))
}

a_latex(tabla_flujo(r),       c("l", "r"),            "tabla1_flujo.tex")
a_latex(tabla_desc(r),        c("l", "r", "r"),       "tabla2_descriptivos.tex")
a_latex(tabla_lr(r),          c("l", "r", "r", "r"),  "tabla3_razon_verosimilitudes.tex")
a_latex(tabla_coef(r),        c("l", rep("r", 6)),    "tabla4_coeficientes.tex")
a_latex(tabla_comparacion(r), c("l", "r", "r"),       "tabla5_completo_vs_reducido.tex")
a_latex(tabla_robustez(r),    c("l", rep("r", 5)),    "tabla6_robustez.tex")

writeLines(ecuacion_modelo(r, por_linea = 3), "latex/tablas/ecuacion_modelo.tex")
