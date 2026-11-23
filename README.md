# Determinantes del acceso al crédito de los productores agropecuarios del Perú (ENA 2014–2025)

Datos y código del artículo *Determinantes del acceso al crédito de los productores agropecuarios del Perú: evidencia de la Encuesta Nacional Agropecuaria 2014–2025*. Autores: Edgar Jeferson Cusihuaman Garate, Nestor Ademir Ruelas Yana, Keny Leonel Ccora Quispe y Yhonel Mamani Crúz (Facultad de Ingeniería Estadística e Informática, Universidad Nacional del Altiplano, Puno).

## Datos

Los microdatos originales de la Encuesta Nacional Agropecuaria (ENA) son públicos y se descargan del portal de microdatos del INEI (<http://iinei.inei.gob.pe/microdatos/>). Se usaron los años 2014–2019 y 2022–2025. En 2020 no se aplicó la encuesta y en 2021 las preguntas de crédito vienen vacías.

La unión de los módulos se hizo originalmente en SPSS. Para que ese paso sea reproducible, se replicó en Python (carpeta `reconstruccion/`): los scripts leen los archivos `.sav` del INEI y construyen la base consolidada con una diferencia menor a 0.1 % respecto a la versión de SPSS. La base unida de 2014–2024 tenía 3 843 598 filas y 1 679 variables, y los ocho módulos de 2025 sumaron 1 849 052 filas. Por su tamaño, esas bases no se incluyen aquí. Se incluye la base ya consolidada, con una fila por productor y año.

Limpieza aplicada: se conservan solo los productores que respondieron el módulo de crédito y se excluye a los grandes productores (`CODIGO = 2`, empresas sin datos del productor ni del hogar). Los productores sin parcelas registradas quedan con `parcelas` faltante.

| Archivo | Filas | Contenido |
|---|---|---|
| `datos/ena_2014_2025_productor.csv.gz` | 297 716 | Base consolidada 2014–2025, una fila por productor y año, 23 variables |
| `datos/ena_muestra_regresion.csv.gz` | 294 068 | Muestra de la regresión logística: casos completos en las 17 variables del modelo (34 233 con crédito) |

Los archivos están comprimidos en gzip. `readr::read_csv()` los lee directamente, sin descomprimirlos.

### Variables

| Variable | Descripción | Codificación |
|---|---|---|
| `anio` | Año de la encuesta | 2014–2019, 2022–2025 |
| `departamento` | Código INEI del departamento | 01–25 |
| `conglomerado` | Conglomerado o segmento de muestreo | código |
| `peso` | Factor de expansión | numérico |
| `credito` | Solicitó (P901) y obtuvo (P902) crédito en los últimos 12 meses | 1 = sí, 0 = no |
| `mujer` | Productora mujer | 1 = sí, 0 = no |
| `edad` | Edad del productor | años |
| `educ` | Nivel educativo | 0 = sin nivel/inicial, 1 = primaria, 2 = secundaria, 3 = superior |
| `lengua_originaria` | Lengua materna originaria | 1 = sí, 0 = no |
| `tam_hogar` | Tamaño del hogar | personas |
| `parcelas` | Número de parcelas | número |
| `riego` | Tiene riego | 1 = sí, 0 = no |
| `titulo` | Tiene título de propiedad inscrito en registros públicos | 1 = sí, 0 = no |
| `pecuaria` | Tiene actividad pecuaria | 1 = sí, 0 = no |
| `semilla` | Usa semilla certificada | 1 = sí, 0 = no |
| `fertiliz` | Usa fertilizantes | 1 = sí, 0 = no |
| `capacita` | Recibió capacitación | 1 = sí, 0 = no |
| `asist` | Recibió asistencia técnica | 1 = sí, 0 = no |
| `asoc` | Pertenece a una asociación de productores | 1 = sí, 0 = no |
| `region` | Región natural | 1 = costa, 2 = sierra, 3 = selva |
| `solicito` | Solicitó crédito en los últimos 12 meses (P901) | 1 = sí, 0 = no |
| `obtuvo` | Obtuvo el crédito, solo entre quienes solicitaron (P902) | 1 = sí, 0 = no, vacío si no solicitó |
| `titulo_cualquiera` | Tiene título de propiedad, inscrito o no (análisis de sensibilidad) | 1 = sí, 0 = no |

## Código

| Archivo | Qué hace |
|---|---|
| `01_analisis.R` | Lee la base consolidada, recodifica, exporta la muestra de la regresión, estima todos los modelos (logit, diseño muestral, por periodos, 2025, modelo reducido), la selección por razón de verosimilitudes y la validación cruzada aleatoria y por departamento. Guarda `resultados_articulo.rds`. |
| `graficos.R` | Funciones de cada figura y tabla del artículo. |
| `02_figuras_tablas.R` | Exporta las figuras a `figuras/` (PNG, 300 dpi) y las tablas a `tablas/` (CSV). |
| `03_tablas_latex.R` | Exporta las tablas y la ecuación del modelo en LaTeX a `latex/tablas/`. |
| `04_comparacion_modelos.R` | Compara el logit con probit, log-log complementario, LASSO, Random Forest y XGBoost (validación aleatoria y por departamento, DeLong, Brier, calibración), importancia SHAP y logit multinivel. Tarda unos 35 minutos. |
| `05_heterogeneidad.R` | Efectos marginales promedio, edad no lineal, interacciones con la región, un logit por departamento y multinivel con pendientes aleatorias. |
| `06_mejoras_metodologicas.R` | Modelo en dos etapas (solicitud y aprobación), efectos fijos de año, edad como spline, sensibilidad del título y los 24 departamentos agrupados por región natural. |
| `reconstruccion/01_extraer_credito.py`, `02_construir_base_credito.py` | Construyen la base consolidada desde los `.sav` del INEI (Python, `pyreadstat`). |
| `articulo_ENA_credito_IEEE.qmd` | Documento Quarto del artículo (versión original). |
| `articulo_ENA_credito_IEEE_v2.qmd` | Versión 2 del artículo, con las correcciones del docente. |
| `postproceso_docx.py` | Da formato IEEE (título, dos columnas, tablas) al Word que genera Quarto. |
| `articulo_final/` | Artículo v2 final en Word y PDF. |
| `latex/main.tex` | Versión LaTeX del artículo (clase IEEEtran). |

## Cómo reproducir

1. (Opcional) Reconstruir la base desde los microdatos: descargar la ENA del INEI, ajustar las rutas en `reconstruccion/` y ejecutar `python 01_extraer_credito.py` y `python 02_construir_base_credito.py` (requiere `pandas`, `pyreadstat`, `pyarrow`).
2. Instalar R (≥ 4.4) y los paquetes `tidyverse`, `easystats`, `survey`, `pROC`, `patchwork`, `systemfonts`, `ragg`, `knitr`, `glmnet`, `ranger`, `xgboost` y `lme4`.
3. Abrir la carpeta del repositorio como directorio de trabajo.
4. Ejecutar `01_analisis.R` (unos 2 minutos), `04_comparacion_modelos.R` (unos 35 minutos), `05_heterogeneidad.R` (unos 15 minutos), `06_mejoras_metodologicas.R` y luego `02_figuras_tablas.R`.
5. Para el artículo v2: `quarto render articulo_ENA_credito_IEEE_v2.qmd --to docx` y después `python postproceso_docx.py articulo_ENA_credito_IEEE_v2.docx articulo_final/Articulo_ENA_credito_IEEE_v2.docx`.

La validación cruzada aleatoria usa `set.seed(2026)`.
