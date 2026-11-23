# Funciones de figuras y tablas del artículo (las usan el .qmd y 02_figuras_tablas.R)
library(tidyverse)
library(patchwork)

# ---- Formato ----
f2  <- function(x, d = 2) formatC(x, format = "f", digits = d)
fn  <- function(x) format(x, big.mark = " ", scientific = FALSE)
dec <- function(t) ifelse(t %in% c("edad", "anio_c"), 3, 2)

fuente <- local({
  fams <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character())
  if ("TeX Gyre Termes" %in% fams) "TeX Gyre Termes" else if ("Times New Roman" %in% fams) "Times New Roman" else "serif"
})
tinta <- "#1A1A1A"; gris <- "#8C8C8C"; acento <- "#8F3B22"

tema_articulo <- function() {
  theme_classic(base_size = 7.5, base_family = fuente) +
    theme(panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.25),
          axis.line = element_line(colour = "grey40", linewidth = 0.25),
          axis.ticks = element_line(colour = "grey40", linewidth = 0.25),
          axis.text = element_text(colour = tinta),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 7),
          plot.margin = margin(2, 4, 2, 2))
}

# ---- Etiquetas ----
orden_x <- c("mujerSí", "edad", "educPrimaria", "educSecundaria", "educSuperior", "lengua_originariaSí",
             "tam_hogar", "parcelas", "riegoSí", "tituloSí", "pecuariaSí", "semillaSí", "fertilizSí",
             "capacitaSí", "asistSí", "asocSí", "regionSierra", "regionSelva", "anio_c")
etq <- c("mujerSí" = "Mujer", "edad" = "Edad", "educPrimaria" = "Ed. primaria",
         "educSecundaria" = "Ed. secundaria", "educSuperior" = "Ed. superior",
         "lengua_originariaSí" = "Lengua originaria", "tam_hogar" = "Tamaño del hogar",
         "parcelas" = "Parcelas", "riegoSí" = "Riego", "tituloSí" = "Título de propiedad",
         "pecuariaSí" = "Actividad pecuaria", "semillaSí" = "Semilla certificada",
         "fertilizSí" = "Fertilizantes", "capacitaSí" = "Capacitación", "asistSí" = "Asistencia técnica",
         "asocSí" = "Asociación", "regionSierra" = "Sierra", "regionSelva" = "Selva", "anio_c" = "Año")
etq_corta <- c(etq, "tam_hogar" = "Tam. hogar", "tituloSí" = "Título", "pecuariaSí" = "Pecuaria",
               "semillaSí" = "Semilla cert.", "asistSí" = "Asist. técnica", "lengua_originariaSí" = "Lengua orig.",
               "capacitaSí" = "Capacitación")
etq_corta <- etq_corta[!duplicated(names(etq_corta), fromLast = TRUE)]
etq_x <- setNames(paste0("X", seq_along(orden_x), " ", etq_corta[orden_x]), orden_x)
dep_nom <- c("01"="Amazonas","02"="Áncash","03"="Apurímac","04"="Arequipa","05"="Ayacucho","06"="Cajamarca",
  "07"="Callao","08"="Cusco","09"="Huancavelica","10"="Huánuco","11"="Ica","12"="Junín","13"="La Libertad",
  "14"="Lambayeque","15"="Lima","16"="Loreto","17"="Madre de Dios","18"="Moquegua","19"="Pasco","20"="Piura",
  "21"="Puno","22"="San Martín","23"="Tacna","24"="Tumbes","25"="Ucayali")

# ---- Tablas ----
tabla_flujo <- function(r) tibble::tribble(
  ~Etapa, ~Registros,
  "Filas de la base unida 2014–2024", "3 843 598",
  "Filas de los módulos de 2025 (15 archivos)", "1 849 052",
  "Productor-año 2014–2024, tras agregar", "349 117",
  "2014–2024, sin 2021 y solo pequeños/medianos productores", fn(r$n_base_1424),
  "Productores de 2025, tras agregar", fn(r$n_base_25),
  "Base consolidada 2014–2025", fn(r$n_total),
  "Casos completos (modelo)", fn(r$n))

tabla_desc <- function(r) {
  d0 <- r$desc[r$desc$credito == 0, ]; d1 <- r$desc[r$desc$credito == 1, ]
  tibble::tribble(
    ~Variable, ~v,
    "Edad (media, años)", "edad", "Mujer", "mujer", "Lengua materna originaria", "lengua",
    "Sin nivel o inicial", "sinniv", "Educación superior", "sup", "Usa riego", "riego",
    "Título de propiedad", "titulo", "Actividad pecuaria", "pecuaria",
    "Semilla certificada", "semilla", "Usa fertilizantes", "fertiliz",
    "Capacitación", "capacita", "Asistencia técnica", "asist",
    "Pertenece a asociación", "asoc", "Sierra", "sierra", "Selva", "selva") |>
    mutate(`Sin crédito` = map_chr(v, ~ f2(d0[[.x]], 1)),
           `Con crédito` = map_chr(v, ~ f2(d1[[.x]], 1))) |>
    select(-v) |>
    add_row(Variable = "Productores (n)", `Sin crédito` = fn(d0$n), `Con crédito` = fn(d1$n), .before = 1)
}

tabla_coef <- function(r) {
  r$coef |>
    mutate(orden = match(term, c("(Intercept)", orden_x))) |>
    arrange(orden) |>
    transmute(Variable = ifelse(term == "(Intercept)", "Constante", etq_x[term]),
              B = f2(b, 3), EE = f2(se, 3), Wald = f2(wald, 1),
              OR = map2_chr(or, term, ~ f2(.x, dec(.y))),
              `IC 95 %` = pmap_chr(list(lo, hi, term), ~ paste0(f2(..1, dec(..3)), "–", f2(..2, dec(..3)))),
              p = ifelse(p < 0.001, "<0.001", f2(p, 3))) |>
    mutate(OR = ifelse(Variable == "Constante", "—", OR), `IC 95 %` = ifelse(Variable == "Constante", "—", `IC 95 %`))
}

tabla_lr <- function(r) {
  nombres <- c(mujer = "Mujer", edad = "Edad", educ = "Nivel educativo", lengua_originaria = "Lengua originaria",
               tam_hogar = "Tamaño del hogar", parcelas = "Parcelas", riego = "Riego", titulo = "Título de propiedad",
               pecuaria = "Actividad pecuaria", semilla = "Semilla certificada", fertiliz = "Fertilizantes",
               capacita = "Capacitación", asist = "Asistencia técnica", asoc = "Asociación",
               region = "Región natural", anio_c = "Año")
  r$lr |> arrange(desc(lr)) |>
    transmute(Variable = nombres[variable], gl, `χ² RV` = f2(lr, 1),
              p = ifelse(p < 0.001, "<0.001", f2(p, 3)))
}

tabla_comparacion <- function(r) {
  a <- r$ajuste
  fila <- function(nom, x, d) tibble(Indicador = nom, Completo = f2(x[1], d), Reducido = f2(x[2], d))
  bind_rows(
    tibble(Indicador = "Variables (parámetros)", Completo = "16 (19)", Reducido = "6 (9)"),
    tibble(Indicador = "AIC", Completo = fn(round(a$aic[1])), Reducido = fn(round(a$aic[2]))),
    tibble(Indicador = "BIC", Completo = fn(round(a$bic[1])), Reducido = fn(round(a$bic[2]))),
    fila("R² de Nagelkerke", a$r2n, 3), fila("R² de McFadden", a$r2mf, 3),
    fila("AUC", a$auc, 3), fila("Exactitud (corte 0.15)", a$exact, 3))
}

tabla_robustez <- function(r) {
  celda <- function(tab, t) {
    x <- tab[tab$term == t, ]
    if (nrow(x) == 0) return("—")
    paste0(f2(x$or, dec(t)), ifelse(x$p >= 0.05, "†", ""))
  }
  r$or |> filter(term != "(Intercept)") |> arrange(desc(or)) |>
    transmute(Variable = etq[term],
              Base = map_chr(term, ~ celda(r$or, .x)),
              Pond. = map_chr(term, ~ celda(r$or_w, .x)),
              `14–19` = map_chr(term, ~ celda(r$or_1419, .x)),
              `22–25` = map_chr(term, ~ celda(r$or_2225, .x)),
              `2025` = map_chr(term, ~ celda(r$or_25, .x))) |>
    add_row(Variable = "n", Base = fn(r$n), Pond. = fn(r$n), `14–19` = fn(r$n_1419),
            `22–25` = fn(r$n_2225), `2025` = fn(r$n_25)) |>
    add_row(Variable = "AUC", Base = f2(r$auc, 3), Pond. = "—", `14–19` = f2(r$auc_1419, 3),
            `22–25` = f2(r$auc_2225, 3), `2025` = f2(r$auc_25, 3))
}

# ---- Ecuación del modelo estimado (LaTeX) ----
ecuacion_modelo <- function(r, por_linea = 4) {
  cf <- r$coef |> mutate(orden = match(term, c("(Intercept)", orden_x))) |> arrange(orden)
  b0 <- f2(cf$b[1], 3)
  terms <- map_chr(2:nrow(cf), function(i) {
    paste0(ifelse(cf$b[i] < 0, "- ", "+ "), f2(abs(cf$b[i]), 3), "X_{", i - 1, "}")
  })
  grupos <- split(terms, ceiling(seq_along(terms) / por_linea))
  lineas <- map_chr(grupos, ~ paste(.x, collapse = " "))
  paste0("$$\\begin{aligned}\\ln\\left(\\frac{\\hat{p}}{1-\\hat{p}}\\right) ={}& ", b0, " \\\\ &",
         paste(lineas, collapse = " \\\\ &"), "\\end{aligned}$$")
}

# ---- Figuras ----
fig_anio <- function(r) {
  r$por_anio |>
    ggplot(aes(anio, pct)) +
    geom_line(colour = tinta, linewidth = 0.45) +
    geom_point(colour = tinta, size = 1.3) +
    geom_text(aes(label = f2(pct, 1)), vjust = -0.9, size = 2.3, family = fuente) +
    scale_x_continuous(breaks = c(2014:2019, 2022:2025)) +
    scale_y_continuous(limits = c(8, 15.5)) +
    labs(x = NULL, y = "% con crédito") + tema_articulo()
}

fig_forest <- function(r) {
  r$or |> filter(term != "(Intercept)") |>
    mutate(etiqueta = fct_reorder(etq[term], or), lado = or > 1) |>
    ggplot(aes(or, etiqueta, colour = lado)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55", linewidth = 0.3) +
    geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.18, linewidth = 0.4) +
    scale_x_log10(breaks = c(0.5, 0.75, 1, 1.5, 2, 3), limits = c(0.45, 3.6)) +
    scale_colour_manual(values = c(`TRUE` = tinta, `FALSE` = acento), guide = "none") +
    labs(x = "Razón de odds", y = NULL) + tema_articulo()
}

fig_roc_cal <- function(r) {
  roc_d <- r$roc |> slice(round(seq(1, n(), length.out = 2000)))
  g1 <- ggplot(roc_d, aes(fpr, tpr)) +
    geom_abline(linetype = "dashed", colour = "grey60", linewidth = 0.3) +
    geom_line(colour = tinta, linewidth = 0.5) +
    annotate("text", x = 0.62, y = 0.12, size = 2.3, family = fuente, label = paste("AUC =", f2(r$auc, 3))) +
    coord_equal() + labs(x = "1 − especificidad", y = "Sensibilidad") + tema_articulo()
  g2 <- ggplot(r$cal, aes(100 * pred, 100 * obs)) +
    geom_abline(linetype = "dashed", colour = "grey60", linewidth = 0.3) +
    geom_point(colour = acento, size = 1.2) +
    coord_equal(xlim = c(0, 38), ylim = c(0, 38)) +
    labs(x = "Predicho (%)", y = "Observado (%)") + tema_articulo()
  g1 + g2
}

fig_cv <- function(r) {
  cvd <- r$cv$auc_dep |> mutate(nombre = dep_nom[departamento]) |> filter(n >= 1000)
  cvd |>
    mutate(nombre = fct_reorder(nombre, auc), puno = departamento == "21") |>
    ggplot(aes(auc, nombre, colour = puno)) +
    geom_segment(aes(x = 0.64, xend = auc, yend = nombre), linewidth = 0.25, colour = "grey75") +
    # líneas de referencia: deben verse al imprimir (negras, 0.6 pt, con etiqueta)
    geom_vline(xintercept = r$cv$auc_bl, linetype = "dashed", colour = tinta, linewidth = 0.6) +
    geom_vline(xintercept = r$cv$auc_al, linetype = "dotted", colour = acento, linewidth = 0.6) +
    annotate("label", x = r$cv$auc_bl, y = 1.2, hjust = 1.05, size = 2.1, family = fuente, label.size = 0,
             label = paste0("Global bloqueado = ", f2(r$cv$auc_bl, 3))) +
    annotate("label", x = r$cv$auc_al, y = 2.6, hjust = -0.05, size = 2.1, family = fuente, label.size = 0,
             colour = acento, label = paste0("Aleatorio = ", f2(r$cv$auc_al, 3))) +
    geom_point(size = 1.4) +
    scale_colour_manual(values = c(`TRUE` = acento, `FALSE` = tinta), guide = "none") +
    scale_x_continuous(limits = c(0.64, 0.82)) +
    labs(x = "AUC fuera de muestra", y = NULL) + tema_articulo() +
    theme(axis.text.y = element_text(size = 6.8))
}

fig_prob <- function(r) {
  niv <- c("No", "Sí", "Sin nivel/inicial", "Primaria", "Secundaria", "Superior", "Costa", "Sierra", "Selva")
  r$pr |>
    mutate(nivel = factor(nivel, levels = niv),
           nivel = fct_recode(nivel, "Sin nivel" = "Sin nivel/inicial"),
           grupo = factor(grupo, levels = c("Educación", "Asociación", "Título", "Región"))) |>
    ggplot(aes(nivel, 100 * p)) +
    geom_col(fill = gris, width = 0.65) +
    geom_text(aes(label = f2(100 * p, 1)), vjust = -0.35, size = 2.1, family = fuente) +
    facet_grid(~ grupo, scales = "free_x", space = "free_x") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
    labs(x = NULL, y = "Probabilidad (%)") + tema_articulo() +
    theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 6.3))
}
