# Más allá de las razones de odds: efectos marginales, edad no lineal y heterogeneidad territorial
#   1. Efectos marginales promedio (AME) en puntos porcentuales, con IC por simulación
#   2. Edad no lineal (spline natural) frente a edad lineal
#   3. ¿Los efectos cambian por región natural? Modelos por región + pruebas RV de interacción
#   4. ¿Y por departamento? Un logit por departamento (OR de asociación, título, educación, semilla...)
#   5. Logit multinivel con pendientes aleatorias por departamento
# Salida: resultados_heterogeneidad.rds, tablas/tabla10-13_*.csv, figuras/fig8-11_*.png
suppressPackageStartupMessages({library(tidyverse); library(lme4); library(splines); library(patchwork)})
source("graficos.R")
set.seed(2026)
d <- read_csv("datos/ena_muestra_regresion.csv.gz",
              col_types = cols(departamento = col_character(), conglomerado = col_character()),
              show_col_types = FALSE) |>
  mutate(educ = factor(educ, 0:3, c("Sin nivel/inicial","Primaria","Secundaria","Superior")),
         region = factor(region, 1:3, c("Costa","Sierra","Selva")),
         across(c(mujer, lengua_originaria, riego, titulo, pecuaria, semilla, fertiliz, capacita, asist, asoc),
                ~ factor(.x, 0:1, c("No","Sí"))),
         anio_c = anio - 2014, dep = dep_nom[departamento])
vars <- c("mujer","edad","educ","lengua_originaria","tam_hogar","parcelas","riego","titulo","pecuaria",
          "semilla","fertiliz","capacita","asist","asoc","region","anio_c")
f <- reformulate(vars, "credito")
m <- glm(f, data = d, family = binomial)

# ---------------------------------------------------------------- 1. efectos marginales promedio
# AME de una indicadora = media de P(y=1 | x=Sí) − P(y=1 | x=No), manteniendo lo demás como está.
# Continuas: cambio por +1 unidad (edad: +10 años). IC 95 %: 300 simulaciones de los coeficientes.
B <- MASS::mvrnorm(300, coef(m), vcov(m))
X0 <- model.matrix(f, d)
pred_b <- function(Xm, b) plogis(Xm %*% b)
contraste <- function(var, a, b_, paso = 1) {
  da <- d; db <- d
  if (is.factor(d[[var]])) { da[[var]] <- factor(a, levels(d[[var]])); db[[var]] <- factor(b_, levels(d[[var]])) }
  else { db[[var]] <- d[[var]] + paso }
  Xa <- model.matrix(f, da); Xb <- model.matrix(f, db)
  est <- mean(pred_b(Xb, coef(m)) - pred_b(Xa, coef(m)))
  sims <- apply(B, 1, function(b) mean(pred_b(Xb, b) - pred_b(Xa, b)))
  tibble(ame = 100 * est, lo = 100 * quantile(sims, .025), hi = 100 * quantile(sims, .975))
}
ame <- bind_rows(
  tibble(term = "mujerSí", contraste("mujer", "No", "Sí")),
  tibble(term = "edad", contraste("edad", paso = 10)) |> mutate(term = "edad (+10 años)"),
  tibble(term = "educPrimaria", contraste("educ", "Sin nivel/inicial", "Primaria")),
  tibble(term = "educSecundaria", contraste("educ", "Sin nivel/inicial", "Secundaria")),
  tibble(term = "educSuperior", contraste("educ", "Sin nivel/inicial", "Superior")),
  tibble(term = "lengua_originariaSí", contraste("lengua_originaria", "No", "Sí")),
  tibble(term = "tam_hogar", contraste("tam_hogar")),
  tibble(term = "parcelas", contraste("parcelas")),
  map_dfr(c("riego","titulo","pecuaria","semilla","fertiliz","capacita","asist","asoc"),
          \(v) tibble(term = paste0(v, "Sí"), contraste(v, "No", "Sí"))),
  tibble(term = "regionSierra", contraste("region", "Costa", "Sierra")),
  tibble(term = "regionSelva", contraste("region", "Costa", "Selva"))) |>
  left_join(tibble(term = names(coef(m)), or = exp(coef(m))), by = "term")
cat("Efectos marginales promedio (puntos porcentuales):\n"); print(ame, n = 30)

# ---------------------------------------------------------------- 2. edad no lineal
f_spl <- reformulate(c(setdiff(vars, "edad"), "ns(edad, df = 4)"), "credito")
m_spl <- glm(f_spl, data = d, family = binomial)
lr_edad <- anova(m, m_spl, test = "LRT")
edades <- 20:85
curva_edad <- map_dfr(edades, function(e) { de <- d; de$edad <- e
  tibble(edad = e, lineal = mean(predict(m, de, type = "response")),
         spline = mean(predict(m_spl, de, type = "response"))) })
cat("Edad lineal vs spline: chi2 =", round(lr_edad$Deviance[2], 1), " p =", signif(lr_edad$`Pr(>Chi)`[2], 3),
    " AIC", round(AIC(m)), "vs", round(AIC(m_spl)), "\n")

# ---------------------------------------------------------------- 3. heterogeneidad por región natural
vars_r <- setdiff(vars, "region")
f_r <- reformulate(vars_r, "credito")
por_region <- map_dfr(levels(d$region), function(rg) {
  mr <- glm(f_r, data = filter(d, region == rg), family = binomial)
  ci <- suppressMessages(confint.default(mr))
  tibble(region = rg, term = names(coef(mr)), or = exp(coef(mr)), lo = exp(ci[, 1]), hi = exp(ci[, 2]),
         n = nobs(mr), pct_credito = 100 * mean(mr$y))
}) |> filter(term != "(Intercept)")
# Prueba global: ¿todos los efectos son iguales en las tres regiones?
m_int <- glm(reformulate(c(paste0("region*(", paste(vars_r, collapse = "+"), ")")), "credito"),
             data = d, family = binomial)
lr_global <- anova(m, m_int, test = "LRT")
# Prueba variable por variable: añadir solo region × variable
int_var <- map_dfr(setdiff(vars_r, "anio_c"), function(v) {
  mi <- update(m, as.formula(paste(". ~ . + region:", v)))
  a <- anova(m, mi, test = "LRT")
  tibble(variable = v, chi2 = a$Deviance[2], gl = a$Df[2], p = a$`Pr(>Chi)`[2])
}) |> arrange(desc(chi2))
cat("Interacción global región × todas: chi2 =", round(lr_global$Deviance[2], 1), " gl =", lr_global$Df[2],
    " p =", signif(lr_global$`Pr(>Chi)`[2], 3), "\n"); print(int_var)

# ---------------------------------------------------------------- 4. un logit por departamento
vars_d <- setdiff(vars, "region")   # dentro de un departamento la región casi no varía
claves <- c("asocSí", "tituloSí", "educSuperior", "semillaSí", "fertilizSí", "asistSí", "capacitaSí", "mujerSí")
por_dep <- d |> filter(departamento != "07") |> group_split(departamento) |> map_dfr(function(dd) {
  mr <- glm(reformulate(vars_d, "credito"), data = dd, family = binomial)
  ci <- suppressMessages(confint.default(mr))
  tibble(departamento = dd$departamento[1], dep = dd$dep[1], term = names(coef(mr)), b = coef(mr),
         or = exp(coef(mr)), lo = exp(ci[, 1]), hi = exp(ci[, 2]), n = nobs(mr), pct_credito = 100 * mean(dd$credito))
}) |> filter(term %in% claves)
# ¿Es real la variación entre departamentos? Prueba de heterogeneidad Q (tipo metaanálisis) por variable
het_dep <- por_dep |> mutate(se = (log(hi) - log(lo)) / (2 * 1.96), w = 1 / se^2) |>
  group_by(term) |> summarise(or_comb = exp(sum(w * b) / sum(w)), Q = sum(w * (b - sum(w * b) / sum(w))^2),
                              gl = n() - 1, p = pchisq(Q, gl, lower.tail = FALSE),
                              I2 = pmax(0, (Q - gl) / Q) * 100, or_min = min(or), or_max = max(or),
                              dep_min = dep[which.min(or)], dep_max = dep[which.max(or)]) |>
  arrange(desc(I2))
cat("Heterogeneidad entre departamentos:\n"); print(het_dep, width = Inf)

# ---------------------------------------------------------------- 5. multinivel con pendientes aleatorias
ctrl <- glmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
f_ml0 <- update(f, . ~ . + (1 | departamento))
f_ml1 <- update(f, . ~ . + (1 + asoc + titulo + semilla | departamento))
ml0 <- glmer(f_ml0, data = d, family = binomial, control = ctrl, nAGQ = 0)
ml1 <- glmer(f_ml1, data = d, family = binomial, control = ctrl, nAGQ = 0)
lr_ml <- anova(ml0, ml1)
re <- coef(ml1)$departamento
or_ml_dep <- tibble(departamento = rownames(re), dep = dep_nom[rownames(re)],
                    asoc = exp(re[, "asocSí"]), titulo = exp(re[, "tituloSí"]), semilla = exp(re[, "semillaSí"]),
                    intercepto = re[, "(Intercept)"])
cat("Pendientes aleatorias vs solo intercepto: chi2 =", round(lr_ml$Chisq[2], 1), " gl =", lr_ml$Df[2],
    " p =", signif(lr_ml$`Pr(>Chisq)`[2], 3), "\n"); print(VarCorr(ml1)); print(arrange(or_ml_dep, asoc), n = 30)

# ---------------------------------------------------------------- guardar
res <- list(ame = ame, lr_edad = lr_edad, curva_edad = curva_edad, por_region = por_region,
            lr_global = lr_global, int_var = int_var, por_dep = por_dep, het_dep = het_dep,
            lr_ml = lr_ml, varcorr = as.data.frame(VarCorr(ml1)), or_ml_dep = or_ml_dep,
            acceso_dep = d |> group_by(departamento, dep) |> summarise(n = n(), pct = 100 * mean(credito), .groups = "drop"))
saveRDS(res, "resultados_heterogeneidad.rds")
write_csv(ame, "tablas/tabla10_efectos_marginales.csv")
write_csv(int_var, "tablas/tabla11_interaccion_region.csv")
write_csv(por_region, "tablas/tabla12_or_por_region.csv")
write_csv(het_dep, "tablas/tabla13_heterogeneidad_departamentos.csv")
write_csv(por_dep, "tablas/tabla13b_or_por_departamento.csv")
write_csv(or_ml_dep, "tablas/tabla14_multinivel_pendientes.csv")

# ---------------------------------------------------------------- figuras
destacar <- c("Lima", "Cusco", "Puno")
g_ame <- ame |> mutate(et = coalesce(etq[term], term), et = fct_reorder(et, ame)) |>
  ggplot(aes(ame, et, colour = ame > 0)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = tinta, linewidth = 0.4) +
  geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.18, linewidth = 0.4) +
  scale_colour_manual(values = c(`TRUE` = tinta, `FALSE` = acento), guide = "none") +
  labs(x = "Cambio en la probabilidad de crédito (puntos porcentuales)", y = NULL) + tema_articulo()
ggsave("figuras/fig8_efectos_marginales.png", g_ame, width = 3.5, height = 3.2, dpi = 300)

g_reg <- por_region |> filter(term %in% claves) |>
  mutate(et = fct_reorder(etq[term], or)) |>
  ggplot(aes(or, et, colour = region)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = tinta, linewidth = 0.4) +
  geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.15, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  scale_x_log10() + scale_colour_manual(values = c(Costa = "#2B6CB0", Sierra = acento, Selva = "#2F855A")) +
  labs(x = "Razón de odds (escala log)", y = NULL, colour = NULL) + tema_articulo() +
  theme(legend.position = "bottom")
ggsave("figuras/fig9_or_por_region.png", g_reg, width = 3.5, height = 3.4, dpi = 300)

g_dep <- por_dep |> filter(term %in% c("asocSí", "tituloSí", "educSuperior", "semillaSí")) |>
  mutate(et = etq[term], dep = fct_reorder(dep, pct_credito), clave = dep %in% destacar) |>
  ggplot(aes(or, dep, colour = clave)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = tinta, linewidth = 0.4) +
  geom_vline(data = het_dep |> filter(term %in% c("asocSí","tituloSí","educSuperior","semillaSí")) |>
               mutate(et = etq[term]), aes(xintercept = or_comb), colour = "grey40", linewidth = 0.4) +
  geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.12, linewidth = 0.35) +
  scale_x_log10() + scale_colour_manual(values = c(`TRUE` = acento, `FALSE` = tinta), guide = "none") +
  facet_wrap(~ et, nrow = 1, scales = "free_x") +
  labs(x = "Razón de odds dentro de cada departamento (escala log)", y = NULL) + tema_articulo() +
  theme(axis.text.y = element_text(size = 6))
ggsave("figuras/fig10_or_por_departamento.png", g_dep, width = 7.2, height = 4.2, dpi = 300)

g_edad <- curva_edad |> pivot_longer(-edad) |>
  ggplot(aes(edad, 100 * value, linetype = name)) + geom_line(colour = tinta, linewidth = 0.5) +
  scale_linetype_manual(values = c(lineal = "dashed", spline = "solid"),
                        labels = c(lineal = "Edad lineal", spline = "Spline natural (4 gl)")) +
  labs(x = "Edad del productor (años)", y = "Probabilidad media de crédito (%)", linetype = NULL) +
  tema_articulo() + theme(legend.position = c(0.75, 0.85))
ggsave("figuras/fig11_edad_no_lineal.png", g_edad, width = 3.5, height = 2.6, dpi = 300)
cat("Listo\n")
