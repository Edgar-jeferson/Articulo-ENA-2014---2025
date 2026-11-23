# Respuesta a las debilidades metodológicas de la revisión
#   B1. Demanda vs. oferta: modelo en dos etapas (¿solicitó? y ¿obtuvo, dado que solicitó?)
#   B2. Año como efectos fijos (un efecto por año) en lugar de tendencia lineal
#   B3. Edad no lineal (spline) en un modelo extendido
#   B5. Título: inscrito (principal) frente a inscrito o no (sensibilidad)
#   Departamentos: los 24 modelos por departamento, clasificados por región natural predominante
# Salida: resultados_mejoras.rds, tablas/tabla15-17_*.csv, figuras/fig12-13_*.png
suppressPackageStartupMessages({library(tidyverse); library(splines); library(pROC)})
source("graficos.R")
set.seed(2026)
d <- read_csv("datos/ena_muestra_regresion.csv.gz",
              col_types = cols(departamento = col_character(), conglomerado = col_character()),
              show_col_types = FALSE) |>
  mutate(educ = factor(educ, 0:3, c("Sin nivel/inicial","Primaria","Secundaria","Superior")),
         region = factor(region, 1:3, c("Costa","Sierra","Selva")),
         across(c(mujer, lengua_originaria, riego, titulo, titulo_cualquiera, pecuaria, semilla, fertiliz,
                  capacita, asist, asoc), ~ factor(.x, 0:1, c("No","Sí"))),
         anio_c = anio - 2014, anio_f = factor(anio), dep = dep_nom[departamento])
vars <- c("mujer","edad","educ","lengua_originaria","tam_hogar","parcelas","riego","titulo","pecuaria",
          "semilla","fertiliz","capacita","asist","asoc","region","anio_c")
f <- reformulate(vars, "credito")
tab_or <- function(m, etiqueta) {
  ci <- suppressMessages(confint.default(m))
  tibble(modelo = etiqueta, term = names(coef(m)), b = coef(m), or = exp(coef(m)),
         lo = exp(ci[, 1]), hi = exp(ci[, 2]), p = summary(m)$coefficients[, 4])
}
# efecto marginal promedio (pp) de pasar una indicadora de No a Sí, o +k en una continua
ame_mod <- function(m, datos, etiqueta) {
  fm <- formula(m)
  un <- function(v, a, b_, paso = 1) {
    da <- datos; db <- datos
    if (is.factor(datos[[v]])) { da[[v]] <- factor(a, levels(datos[[v]])); db[[v]] <- factor(b_, levels(datos[[v]])) }
    else db[[v]] <- datos[[v]] + paso
    100 * mean(predict(m, db, type = "response") - predict(m, da, type = "response"))
  }
  bin <- c("mujer","lengua_originaria","riego","titulo","pecuaria","semilla","fertiliz","capacita","asist","asoc")
  bind_rows(
    tibble(term = paste0(bin, "Sí"), ame = map_dbl(bin, \(v) un(v, "No", "Sí"))),
    tibble(term = c("educPrimaria","educSecundaria","educSuperior"),
           ame = map_dbl(c("Primaria","Secundaria","Superior"), \(n) un("educ", "Sin nivel/inicial", n))),
    tibble(term = c("regionSierra","regionSelva"), ame = map_dbl(c("Sierra","Selva"), \(n) un("region", "Costa", n))),
    tibble(term = "edad (+10 años)", ame = un("edad", paso = 10)),
    tibble(term = "tam_hogar", ame = un("tam_hogar")), tibble(term = "parcelas", ame = un("parcelas"))) |>
    mutate(modelo = etiqueta)
}

# ---------------------------------------------------------------- B1. dos etapas
m_cred <- glm(f, data = d, family = binomial)
m_sol  <- glm(update(f, solicito ~ .), data = d, family = binomial)
d_sol  <- filter(d, solicito == 1)
m_obt  <- glm(update(f, obtuvo ~ .), data = d_sol, family = binomial)
etapas <- bind_rows(tab_or(m_cred, "Obtuvo crédito (todos)"), tab_or(m_sol, "Solicitó (todos)"),
                    tab_or(m_obt, "Obtuvo | solicitó")) |> filter(term != "(Intercept)")
ame_etapas <- bind_rows(ame_mod(m_sol, d, "Solicitó (todos)"), ame_mod(m_obt, d_sol, "Obtuvo | solicitó"))
desc_etapas <- list(n = nrow(d), pct_sol = 100 * mean(d$solicito), n_sol = nrow(d_sol),
                    pct_obt = 100 * mean(d_sol$obtuvo), pct_cred = 100 * mean(d$credito),
                    auc_sol = as.numeric(auc(d$solicito, fitted(m_sol), quiet = TRUE)),
                    auc_obt = as.numeric(auc(d_sol$obtuvo, fitted(m_obt), quiet = TRUE)),
                    por_region = d |> group_by(region) |>
                      summarise(sol = 100 * mean(solicito), obt = 100 * mean(obtuvo, na.rm = TRUE),
                                cred = 100 * mean(credito), .groups = "drop"))
cat("Solicitó:", round(desc_etapas$pct_sol, 1), "% | obtuvo entre solicitantes:", round(desc_etapas$pct_obt, 1), "%\n")
print(desc_etapas$por_region)
print(etapas |> select(modelo, term, or) |> pivot_wider(names_from = modelo, values_from = or), n = 30)

# ---------------------------------------------------------------- B2 + B3. año como efectos fijos y edad no lineal
f_fe  <- reformulate(c(setdiff(vars, "anio_c"), "anio_f"), "credito")
f_ext <- reformulate(c(setdiff(vars, c("anio_c", "edad")), "anio_f", "ns(edad, df = 4)"), "credito")
m_fe  <- glm(f_fe, data = d, family = binomial)
m_ext <- glm(f_ext, data = d, family = binomial)
comp_spec <- bind_rows(tab_or(m_cred, "Base"), tab_or(m_fe, "Año como efectos fijos"),
                       tab_or(m_ext, "Efectos fijos de año + edad spline")) |>
  filter(!str_detect(term, "anio|edad|Intercept"))
dif_max <- comp_spec |> select(modelo, term, or) |> pivot_wider(names_from = modelo, values_from = or) |>
  mutate(d_fe = abs(`Año como efectos fijos` / Base - 1), d_ext = abs(`Efectos fijos de año + edad spline` / Base - 1))
ajuste_spec <- tibble(modelo = c("Base", "Año como efectos fijos", "Efectos fijos de año + edad spline"),
                      aic = c(AIC(m_cred), AIC(m_fe), AIC(m_ext)),
                      auc = map_dbl(list(m_cred, m_fe, m_ext), \(m) as.numeric(auc(d$credito, fitted(m), quiet = TRUE))))
efe_anio <- tibble(anio = levels(d$anio_f)[-1], or = exp(coef(m_fe)[paste0("anio_f", levels(d$anio_f)[-1])]))
curva_edad <- map_dfr(seq(20, 85, 1), function(e) { de <- d; de$edad <- e
  tibble(edad = e, p = 100 * mean(predict(m_ext, de, type = "response"))) })
edad_max <- curva_edad$edad[which.max(curva_edad$p)]
cat("Máximo cambio relativo de OR con efectos fijos de año:", round(100 * max(dif_max$d_fe), 1),
    "% | con edad spline:", round(100 * max(dif_max$d_ext), 1), "%\n"); print(ajuste_spec); print(efe_anio)
cat("Edad con mayor probabilidad:", edad_max, "\n")

# ---------------------------------------------------------------- B5. título inscrito vs. cualquiera
m_tit2 <- glm(update(f, . ~ . - titulo + titulo_cualquiera), data = d, family = binomial)
titulo_sens <- bind_rows(tab_or(m_cred, "Título inscrito") |> filter(term == "tituloSí"),
                         tab_or(m_tit2, "Título inscrito o no") |> filter(term == "titulo_cualquieraSí"))
print(titulo_sens)

# ---------------------------------------------------------------- departamentos (24) por región predominante
reg_dep <- d |> count(departamento, dep, region) |> group_by(departamento, dep) |>
  mutate(pct = 100 * n / sum(n)) |> slice_max(pct, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(departamento, dep, region_pred = region, pct_region = pct)
vars_d <- setdiff(vars, "region")
claves <- c("asocSí","educSuperior","fertilizSí","semillaSí","tituloSí","asistSí","mujerSí")
deps <- d |> filter(departamento != "07") |> group_split(departamento) |> map_dfr(function(dd) {
  m <- glm(reformulate(vars_d, "credito"), data = dd, family = binomial)
  tab_or(m, dd$dep[1]) |> filter(term %in% claves) |>
    mutate(departamento = dd$departamento[1], n = nrow(dd), pct_cred = 100 * mean(dd$credito),
           pct_sol = 100 * mean(dd$solicito), pct_obt = 100 * mean(dd$obtuvo, na.rm = TRUE))
}) |> left_join(select(reg_dep, -dep), by = "departamento") |> rename(dep = modelo)
tabla_dep <- deps |> mutate(celda = paste0(f2(or, 2), if_else(p < 0.05, "", "†"))) |>
  select(region_pred, pct_region, dep, n, pct_sol, pct_obt, pct_cred, term, celda) |>
  pivot_wider(names_from = term, values_from = celda) |>
  arrange(region_pred, desc(pct_cred))
resumen_region <- deps |> group_by(region_pred, term) |>
  summarise(n_dep = n(), or_mediana = median(or), or_min = min(or), or_max = max(or),
            sig = sum(p < 0.05), .groups = "drop")
print(tabla_dep, n = 30, width = Inf); print(resumen_region, n = 30)

# ---------------------------------------------------------------- guardar
res <- list(etapas = etapas, ame_etapas = ame_etapas, desc_etapas = desc_etapas, comp_spec = comp_spec,
            dif_max = dif_max, ajuste_spec = ajuste_spec, efe_anio = efe_anio, curva_edad = curva_edad,
            edad_max = edad_max, titulo_sens = titulo_sens, deps = deps, tabla_dep = tabla_dep,
            resumen_region = resumen_region, reg_dep = reg_dep,
            rf_param = c(num.trees = 300, min.node.size = 50))
saveRDS(res, "resultados_mejoras.rds")
write_csv(etapas, "tablas/tabla15_dos_etapas.csv")
write_csv(tabla_dep, "tablas/tabla16_departamentos_por_region.csv")
write_csv(comp_spec, "tablas/tabla17_especificaciones.csv")

g_dep <- deps |> filter(term %in% c("asocSí","educSuperior","fertilizSí","semillaSí","tituloSí")) |>
  mutate(et = factor(etq[term], etq[c("asocSí","educSuperior","fertilizSí","semillaSí","tituloSí")]),
         dep = fct_reorder(dep, pct_cred), sig = p < 0.05,
         lab = paste0(f2(or, 1), if_else(sig, "", "†"))) |>
  ggplot(aes(et, dep, fill = log(or))) + geom_tile(colour = "white") +
  geom_text(aes(label = lab), size = 1.9, family = fuente) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0, guide = "none",
                       limits = c(-log(5), log(5)), oob = scales::squish) +
  facet_grid(region_pred ~ ., scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) + tema_articulo() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), axis.text.y = element_text(size = 6),
        strip.text.y = element_text(angle = 0))
ggsave("figuras/fig12_mapa_calor_departamentos.png", g_dep, width = 3.4, height = 4.6, dpi = 300)
cat("Listo\n")
