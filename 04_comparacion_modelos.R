# Comparación del logit con modelos alternativos (estadísticos y de machine learning)
# Misma muestra, mismas 16 variables y misma validación que 01_analisis.R:
#   - CV aleatoria de 10 particiones (set.seed(2026))
#   - CV bloqueada dejando fuera un departamento a la vez
# Además: logit multinivel (ICC) e importancia SHAP de XGBoost frente al logit.
# Salida: resultados_comparacion.rds y tablas/tabla7_*.csv, figuras/fig6_*.png
suppressPackageStartupMessages({
  library(tidyverse); library(pROC); library(glmnet); library(ranger); library(xgboost); library(lme4)
})
set.seed(2026)
t0 <- Sys.time()
d <- read_csv("datos/ena_muestra_regresion.csv.gz",
              col_types = cols(departamento = col_character(), conglomerado = col_character()),
              show_col_types = FALSE) |>
  mutate(educ = factor(educ, 0:3, c("Sin nivel/inicial","Primaria","Secundaria","Superior")),
         region = factor(region, 1:3, c("Costa","Sierra","Selva")),
         across(c(mujer, lengua_originaria, riego, titulo, pecuaria, semilla, fertiliz, capacita, asist, asoc),
                ~ factor(.x, 0:1, c("No","Sí"))),
         anio_c = anio - 2014, id_cong = paste(anio, conglomerado))
f <- credito ~ mujer + edad + educ + lengua_originaria + tam_hogar + parcelas + riego + titulo +
  pecuaria + semilla + fertiliz + capacita + asist + asoc + region + anio_c
X <- model.matrix(f, d)[, -1]
y <- d$credito
n <- nrow(d)

# ---------------------------------------------------------------- hiperparámetros
# Se eligen con CV agrupada por departamento (5 grupos) para no premiar el sobreajuste espacial.
deps <- sort(unique(d$departamento))
grupo5 <- setNames(sample(rep(1:5, length.out = length(deps))), deps)[d$departamento]
folds5 <- lapply(1:5, function(k) which(grupo5 == k))
dall <- xgb.DMatrix(X, label = y)
grid <- expand_grid(max_depth = c(3, 5, 7), min_child_weight = c(20, 100))
cvx <- pmap_dfr(grid, function(max_depth, min_child_weight) {
  p <- list(objective = "binary:logistic", eval_metric = "logloss", eta = 0.05, subsample = 0.8,
            colsample_bytree = 0.8, max_depth = max_depth, min_child_weight = min_child_weight, nthread = 0)
  cv <- xgb.cv(params = p, data = dall, nrounds = 2000, folds = folds5, early_stopping_rounds = 50,
               verbose = 0)
  tibble(max_depth, min_child_weight, nrounds = cv$early_stop$best_iteration,
         logloss = min(cv$evaluation_log$test_logloss_mean))
})
mejor <- slice_min(cvx, logloss, n = 1)
p_xgb <- list(objective = "binary:logistic", eval_metric = "logloss", eta = 0.05, subsample = 0.8,
              colsample_bytree = 0.8, max_depth = mejor$max_depth, min_child_weight = mejor$min_child_weight,
              nthread = 0)
cat("XGBoost elegido:\n"); print(mejor)
lam <- cv.glmnet(X, y, family = "binomial", foldid = grupo5, type.measure = "deviance")$lambda.min  # lambda.1se sobrepenalizaba (pendiente de calibración ~2)

# ---------------------------------------------------------------- modelos
ajustar_predecir <- function(tr, te) {
  dtr <- d[tr, ]; dte <- d[te, ]
  out <- list()
  for (lk in c(logit = "logit", probit = "probit", cloglog = "cloglog")) {
    m <- glm(f, data = dtr, family = binomial(link = lk))
    out[[lk]] <- predict(m, dte, type = "response")
  }
  la <- glmnet(X[tr, ], y[tr], family = "binomial", lambda = lam)
  out$lasso <- as.numeric(predict(la, X[te, , drop = FALSE], type = "response"))
  rf <- ranger(x = X[tr, ], y = factor(y[tr]), num.trees = 300, min.node.size = 50, probability = TRUE,
               num.threads = 0, seed = 2026)
  out$rf <- predict(rf, X[te, , drop = FALSE])$predictions[, "1"]
  xg <- xgb.train(params = p_xgb, data = xgb.DMatrix(X[tr, ], label = y[tr]), nrounds = mejor$nrounds,
                  verbose = 0)
  out$xgboost <- predict(xg, X[te, , drop = FALSE])
  as_tibble(out) |> mutate(fila = te)
}

validar <- function(folds, nombre) {
  map_dfr(seq_along(folds), function(k) {
    te <- folds[[k]]
    cat(nombre, k, "/", length(folds), format(Sys.time() - t0, digits = 3), "\n")
    ajustar_predecir(setdiff(seq_len(n), te), te) |> mutate(particion = k)
  }) |> arrange(fila)
}

folds_al <- split(seq_len(n), sample(rep(1:10, length.out = n)))
folds_dep <- split(seq_len(n), d$departamento)
if (file.exists("cache_oof.rds")) {
  list2env(readRDS("cache_oof.rds"), environment())
} else {
  oof_al <- validar(folds_al, "aleatoria")
  oof_dep <- validar(folds_dep, "departamento")
  saveRDS(list(oof_al = oof_al, oof_dep = oof_dep), "cache_oof.rds")
}

modelos <- c("logit", "probit", "cloglog", "lasso", "rf", "xgboost")
nombres <- c(logit = "Logit", probit = "Probit", cloglog = "Log-log compl.", lasso = "LASSO logístico",
             rf = "Random Forest", xgboost = "XGBoost")
metricas <- function(oof, esquema) {
  map_dfr(modelos, function(mo) {
    p <- pmin(pmax(oof[[mo]], 1e-6), 1 - 1e-6)
    r <- roc(y, p, quiet = TRUE)
    ci <- ci.auc(r, method = "delong")
    dl <- if (mo == "logit") NA_real_ else
      roc.test(roc(y, oof$logit, quiet = TRUE), r, method = "delong", paired = TRUE)$p.value
    auc_part <- oof |> mutate(y = y) |> group_by(particion) |>
      summarise(a = if (n_distinct(y) == 2) as.numeric(auc(y, .data[[mo]], quiet = TRUE)) else NA_real_) |>
      pull(a)
    tibble(esquema, modelo = nombres[mo], auc = as.numeric(auc(r)), auc_lo = ci[1], auc_hi = ci[3],
           p_delong = dl, brier = mean((p - y)^2), logloss = -mean(y * log(p) + (1 - y) * log(1 - p)),
           pend_calib = coef(glm(y ~ qlogis(p), family = binomial))[2],
           auc_part_min = min(auc_part, na.rm = TRUE), auc_part_max = max(auc_part, na.rm = TRUE))
  })
}
comp <- bind_rows(metricas(oof_al, "Aleatoria 10 particiones"), metricas(oof_dep, "Bloqueada por departamento"))
print(comp, width = Inf)

# ---------------------------------------------------------------- interpretación: SHAP vs logit
xg_full <- xgb.train(params = p_xgb, data = dall, nrounds = mejor$nrounds, verbose = 0)
shap <- predict(xg_full, X, predcontrib = TRUE)
shap <- shap[, colnames(shap) %in% colnames(X), drop = FALSE]
grupo_var <- function(nm) case_when(str_starts(nm, "educ") ~ "educ", str_starts(nm, "region") ~ "region",
                                    TRUE ~ str_remove(nm, "Sí$"))
imp_shap <- tibble(term = colnames(shap), v = colMeans(abs(shap))) |>
  mutate(variable = grupo_var(term)) |> group_by(variable) |> summarise(shap = sum(v)) |>
  mutate(rank_shap = rank(-shap))
m_full <- glm(f, data = d, family = binomial)
lr <- drop1(m_full, test = "LRT")
imp_logit <- tibble(variable = rownames(lr)[-1], lr = lr$LRT[-1]) |>
  mutate(variable = if_else(variable == "anio_c", "anio_c", variable), rank_lr = rank(-lr))
imp <- full_join(imp_logit, imp_shap, by = "variable") |> arrange(rank_lr)
rho <- cor(imp$rank_lr, imp$rank_shap, method = "spearman")
cat("Spearman ranking logit (RV) vs SHAP:", round(rho, 3), "\n"); print(imp)

# Dirección: el signo medio de SHAP en cada indicadora debe coincidir con el del coeficiente logit
dir <- tibble(term = colnames(shap),
              shap_si = map_dbl(colnames(shap), \(t) { x <- X[, t]
                if (all(x %in% 0:1)) mean(shap[x == 1, t]) - mean(shap[x == 0, t]) else cor(x, shap[, t]) })) |>
  left_join(tibble(term = names(coef(m_full)), beta = coef(m_full)), by = "term") |>
  mutate(coincide = sign(shap_si) == sign(beta))
cat("Signos coincidentes SHAP vs logit:", sum(dir$coincide), "de", nrow(dir), "\n")

# ---------------------------------------------------------------- logit multinivel
mm <- glmer(update(f, . ~ . + (1 | departamento)), data = d, family = binomial,
            control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
mm2 <- glmer(update(f, . ~ . + (1 | departamento) + (1 | id_cong)), data = d, family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
icc <- function(m) { v <- sapply(VarCorr(m), function(x) x[1]); c(v / (sum(v) + pi^2 / 3), total = sum(v) / (sum(v) + pi^2 / 3)) }
or_ml <- tibble(term = names(fixef(mm2)), or_multinivel = exp(fixef(mm2))) |>
  left_join(tibble(term = names(coef(m_full)), or_logit = exp(coef(m_full))), by = "term")
multinivel <- list(icc_dep = icc(mm), icc_dep_cong = icc(mm2), or = or_ml,
                   aic = c(logit = AIC(m_full), dep = AIC(mm), dep_cong = AIC(mm2)))
cat("ICC:\n"); print(multinivel$icc_dep); print(multinivel$icc_dep_cong); print(or_ml, n = 30)

# ---------------------------------------------------------------- guardar
res <- list(comp = comp, cv_xgb = cvx, xgb = mejor, lambda = lam, imp = imp, rho = rho, dir = dir,
            multinivel = multinivel, oof_dep = oof_dep, y = y, departamento = d$departamento,
            versiones = sapply(c("glmnet", "ranger", "xgboost", "lme4", "pROC"), \(p) as.character(packageVersion(p))))
saveRDS(res, "resultados_comparacion.rds")
dir.create("tablas", showWarnings = FALSE); dir.create("figuras", showWarnings = FALSE)
write_csv(comp, "tablas/tabla7_comparacion_modelos.csv")
write_csv(imp, "tablas/tabla8_importancia_logit_vs_shap.csv")
write_csv(or_ml, "tablas/tabla9_logit_vs_multinivel.csv")

g1 <- comp |> mutate(modelo = fct_reorder(modelo, auc)) |>
  ggplot(aes(auc, modelo, colour = esquema)) +
  geom_pointrange(aes(xmin = auc_lo, xmax = auc_hi), position = position_dodge(width = 0.5)) +
  labs(x = "AUC fuera de muestra (IC 95 % DeLong)", y = NULL, colour = "Validación") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave("figuras/fig6_comparacion_modelos.png", g1, width = 6.5, height = 3.8, dpi = 300)
g2 <- imp |> ggplot(aes(rank_lr, rank_shap, label = variable)) +
  geom_abline(linetype = 2, colour = "grey60") + geom_point() + geom_text(vjust = -0.6, size = 3) +
  scale_x_reverse() + scale_y_reverse() +
  labs(x = "Rango en el logit (prueba RV)", y = "Rango en XGBoost (SHAP medio)",
       subtitle = sprintf("Correlación de Spearman = %.2f", rho)) + theme_minimal(base_size = 11)
ggsave("figuras/fig7_logit_vs_shap.png", g2, width = 6, height = 5, dpi = 300)
cat("Listo en", format(Sys.time() - t0, digits = 3), "\n")
