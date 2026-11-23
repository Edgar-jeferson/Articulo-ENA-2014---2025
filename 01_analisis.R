# Análisis para el artículo: ENA 2014-2025, acceso a crédito
suppressPackageStartupMessages({library(tidyverse); library(easystats); library(survey); library(pROC)})
ena <- read_csv("datos/ena_2014_2025_productor.csv.gz",
                col_types = cols(departamento = col_character(), conglomerado = col_character()),
                show_col_types = FALSE)
ena_raw <- ena
si_no <- c("No", "Sí")
ena <- ena |>
  mutate(educ = factor(educ, 0:3, c("Sin nivel/inicial","Primaria","Secundaria","Superior")),
         region = factor(region, 1:3, c("Costa","Sierra","Selva")),
         across(c(mujer, lengua_originaria, riego, titulo, pecuaria, semilla, fertiliz, capacita, asist, asoc),
                ~ factor(.x, 0:1, si_no)),
         anio_c = anio - 2014)
vars <- c("credito","mujer","edad","educ","lengua_originaria","tam_hogar","parcelas","riego","titulo",
          "pecuaria","semilla","fertiliz","capacita","asist","asoc","region","anio_c")
datos <- ena |> drop_na(all_of(vars)) |> mutate(id_cong = paste(anio, conglomerado))
# muestra usada en la regresión logística (casos completos en las 17 variables)
write_csv(ena_raw |> drop_na(all_of(setdiff(vars, "anio_c"))), "datos/ena_muestra_regresion.csv.gz")
f <- credito ~ mujer + edad + educ + lengua_originaria + tam_hogar + parcelas + riego + titulo +
  pecuaria + semilla + fertiliz + capacita + asist + asoc + region + anio_c
f_sin_anio <- update(f, . ~ . - anio_c)
or_tab <- function(mod) model_parameters(mod, exponentiate = TRUE, ci_method = "wald") |>
  as.data.frame() |> transmute(term = Parameter, or = Coefficient, lo = CI_low, hi = CI_high, p = p)

m   <- glm(f, data = datos, family = binomial)
mw  <- svyglm(f, design = svydesign(ids = ~id_cong, weights = ~peso, data = datos), family = quasibinomial())
m1  <- glm(f, data = filter(datos, anio <= 2019), family = binomial)
m2  <- glm(f, data = filter(datos, anio >= 2022), family = binomial)
m25 <- glm(f_sin_anio, data = filter(datos, anio == 2025), family = binomial)

roc_m <- roc(datos$credito, fitted(m), quiet = TRUE)
pred <- as.integer(fitted(m) >= 0.15)
tab <- table(datos$credito, pred)
# probabilidades predichas estandarizadas
prob_std <- function(var, etq) map_dfr(levels(datos[[var]]), function(n) {
  nd <- datos; nd[[var]] <- factor(n, levels = levels(datos[[var]]))
  tibble(grupo = etq, nivel = n, p = mean(predict(m, newdata = nd, type = "response")))})
pr <- bind_rows(prob_std("educ","Educación"), prob_std("asoc","Asociación"),
                prob_std("titulo","Título"), prob_std("region","Región"))

cal <- tibble(p = fitted(m), y = datos$credito) |> mutate(dec = ntile(p, 10)) |>
  group_by(dec) |> summarise(pred = mean(p), obs = mean(y), n = n(), .groups = "drop")
res <- list(
  cal = cal,
  n_total = nrow(ena), n = nrow(datos), prev = mean(datos$credito),
  n_base_1424 = sum(ena$anio <= 2024), n_base_25 = sum(ena$anio == 2025),
  por_anio = datos |> group_by(anio) |> summarise(n = n(), pct = 100*mean(credito), .groups = "drop"),
  desc = datos |> group_by(credito) |>
    summarise(n = n(), edad = mean(edad), mujer = 100*mean(mujer=="Sí"), lengua = 100*mean(lengua_originaria=="Sí"),
              sup = 100*mean(educ=="Superior"), sinniv = 100*mean(educ=="Sin nivel/inicial"),
              riego = 100*mean(riego=="Sí"), titulo = 100*mean(titulo=="Sí"), pecuaria = 100*mean(pecuaria=="Sí"),
              semilla = 100*mean(semilla=="Sí"), fertiliz = 100*mean(fertiliz=="Sí"),
              capacita = 100*mean(capacita=="Sí"), asist = 100*mean(asist=="Sí"), asoc = 100*mean(asoc=="Sí"),
              sierra = 100*mean(region=="Sierra"), selva = 100*mean(region=="Selva"),
              hogar = median(tam_hogar), parcelas = median(parcelas), .groups = "drop"),
  region_pct = datos |> group_by(region) |> summarise(p = 100*mean(credito), .groups="drop"),
  or = or_tab(m), or_w = or_tab(mw), or_1419 = or_tab(m1), or_2225 = or_tab(m2), or_25 = or_tab(m25),
  n_1419 = nobs(m1), n_2225 = nobs(m2), n_25 = nobs(m25),
  chi2 = m$null.deviance - m$deviance, gl = m$df.null - m$df.residual,
  r2n = as.numeric(r2_nagelkerke(m)), r2t = as.numeric(r2_tjur(m)),
  auc = as.numeric(auc(roc_m)), auc_ci = as.numeric(ci.auc(roc_m)),
  roc = tibble(fpr = 1 - roc_m$specificities, tpr = roc_m$sensitivities),
  auc_1419 = as.numeric(auc(roc(m1$y, fitted(m1), quiet=TRUE))),
  auc_2225 = as.numeric(auc(roc(m2$y, fitted(m2), quiet=TRUE))),
  auc_25 = as.numeric(auc(roc(m25$y, fitted(m25), quiet=TRUE))),
  vif = check_collinearity(m) |> as.data.frame(),
  hl = performance_hosmer(m, n_bins = 10),
  tab = tab, sens = tab["1","1"]/sum(tab["1",]), espec = tab["0","0"]/sum(tab["0",]), exact = sum(diag(tab))/sum(tab),
  pr = pr,
  versiones = c(R = paste(R.version$major, R.version$minor, sep="."),
                sapply(c("tidyverse","easystats","parameters","performance","survey","pROC"), \(p) as.character(packageVersion(p))))
)
saveRDS(res, "resultados_articulo.rds")
print(res$or_w |> mutate(across(where(is.numeric), \(x) round(x,3))))
cat("AUC", res$auc, res$auc_ci, " periodo", res$auc_1419, res$auc_2225, res$auc_25, "\n")
print(res$hl)
print(cbind(res$or[,1:2], w=res$or_w$or, a=res$or_1419$or, b=res$or_2225$or) |> mutate(across(where(is.numeric), \(x) round(x,3))))
print(res$or_25 |> mutate(across(where(is.numeric), \(x) round(x,3))))
cat("n", res$n_1419, res$n_2225, res$n_25, " r2t", res$r2t, "\n")
print(res$desc |> mutate(across(where(is.numeric), \(x) round(x,1))) |> t())

# ---- Validación cruzada aleatoria vs. bloqueada por departamento (siguiendo Vilca Apaza et al.) ----
auc_vec <- function(y, p) as.numeric(auc(roc(y, p, quiet = TRUE)))
set.seed(2026)
datos$fold_al <- sample(rep(1:10, length.out = nrow(datos)))
oof_al <- numeric(nrow(datos))
for (k in 1:10) {
  tr <- datos$fold_al != k
  mk <- glm(f, data = datos[tr, ], family = binomial)
  oof_al[!tr] <- predict(mk, newdata = datos[!tr, ], type = "response")
}
deps <- sort(unique(datos$departamento))
oof_bl <- numeric(nrow(datos)); auc_dep <- list()
for (dp in deps) {
  te <- datos$departamento == dp
  mk <- glm(f, data = datos[!te, ], family = binomial)
  oof_bl[te] <- predict(mk, newdata = datos[te, ], type = "response")
  if (length(unique(datos$credito[te])) == 2)
    auc_dep[[dp]] <- tibble(departamento = dp, n = sum(te), auc = auc_vec(datos$credito[te], oof_bl[te]))
}
auc_fold_al <- map_dbl(1:10, \(k) auc_vec(datos$credito[datos$fold_al == k], oof_al[datos$fold_al == k]))
brier <- function(p) mean((datos$credito - p)^2)
cv <- list(
  auc_al = auc_vec(datos$credito, oof_al), auc_bl = auc_vec(datos$credito, oof_bl),
  auc_fold_al = auc_fold_al, auc_dep = bind_rows(auc_dep),
  brier_in = brier(fitted(m)), brier_al = brier(oof_al), brier_bl = brier(oof_bl),
  n_dep = length(deps)
)
res$cv <- cv
saveRDS(res, "resultados_articulo.rds")
cat("CV AUC aleatoria", cv$auc_al, " bloqueada", cv$auc_bl, " n_dep", cv$n_dep, "\n")
cat("AUC folds aleatorios: ", round(range(auc_fold_al),3), " sd ", sd(auc_fold_al), "\n")
print(cv$auc_dep |> arrange(auc), n = 30)
cat("sd dep", sd(cv$auc_dep$auc), " Brier", cv$brier_in, cv$brier_al, cv$brier_bl, "\n")

# ---- Selección y evaluación del modelo (estructura de Huata-Panca et al., 2025) ----
# Eliminación hacia atrás por razón de verosimilitudes: aporte de cada variable al ajuste
lr <- drop1(m, test = "LRT") |> as.data.frame() |> rownames_to_column("variable") |>
  filter(variable != "<none>") |> transmute(variable, gl = Df, lr = LRT, p = `Pr(>Chi)`)
# Coeficientes: B, error estándar, estadístico de Wald, p y OR
coef_tab <- model_parameters(m, ci_method = "wald") |> as.data.frame() |>
  transmute(term = Parameter, b = Coefficient, se = SE, wald = (Coefficient / SE)^2, p = p,
            or = exp(Coefficient), lo = exp(CI_low), hi = exp(CI_high))
# Modelo reducido: las seis variables con mayor aporte en la prueba de razón de verosimilitudes
f_red <- credito ~ fertiliz + semilla + region + asoc + edad + educ
m_red <- glm(f_red, data = datos, family = binomial)
ajuste <- function(mod) tibble(
  k = length(coef(mod)) - 1, aic = AIC(mod), bic = BIC(mod),
  r2n = as.numeric(r2_nagelkerke(mod)), r2mf = as.numeric(r2_mcfadden(mod)$R2),
  auc = as.numeric(auc(roc(mod$y, fitted(mod), quiet = TRUE))),
  exact = mean(as.integer(fitted(mod) >= 0.15) == mod$y),
  chi2 = mod$null.deviance - mod$deviance, gl = mod$df.null - mod$df.residual)
lr_red <- anova(m_red, m, test = "LRT")
res$lr <- lr; res$coef <- coef_tab
res$ajuste <- bind_rows(completo = ajuste(m), reducido = ajuste(m_red), .id = "modelo")
res$or_red <- or_tab(m_red)
res$lr_red <- c(chi2 = lr_red$Deviance[2], gl = lr_red$Df[2], p = lr_red$`Pr(>Chi)`[2])
res$eventos <- sum(datos$credito); res$epv <- sum(datos$credito) / (length(coef(m)) - 1)
res$r2mf <- as.numeric(r2_mcfadden(m)$R2)
saveRDS(res, "resultados_articulo.rds")
print(lr |> arrange(desc(lr)) |> mutate(across(where(is.numeric), \(x) signif(x, 4))))
print(res$ajuste); print(res$lr_red); cat("EPV", res$epv, " eventos", res$eventos, "\n")
