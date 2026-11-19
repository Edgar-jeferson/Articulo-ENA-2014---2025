# Determinantes del acceso al crédito de los productores agropecuarios del Perú (ENA 2014–2025)

Datos y código del artículo *Determinantes del acceso al crédito de los productores agropecuarios del Perú: evidencia de la Encuesta Nacional Agropecuaria 2014–2025*. Autores: Edgar Jeferson Cusihuaman Garate, Nestor Ademir Ruelas Yana, Keny Leonel Ccora Quispe y Yhonel Mamani Crúz (Facultad de Ingeniería Estadística e Informática, Universidad Nacional del Altiplano, Puno).

## Datos

Los microdatos originales de la Encuesta Nacional Agropecuaria (ENA) son públicos y se descargan del portal de microdatos del INEI (<http://iinei.inei.gob.pe/microdatos/>). Se usaron los años 2014–2019 y 2022–2025. En 2020 no se aplicó la encuesta y en 2021 las preguntas de crédito vienen vacías.

La unión de los módulos se hizo en SPSS. La base unida de 2014–2024 tenía 3 843 598 filas y 1 679 variables, y los ocho módulos de 2025 sumaron 1 849 052 filas. Por su tamaño, esas bases no se incluyen aquí. Se incluye la base ya consolidada, con una fila por productor y año.

| Archivo | Filas | Contenido |
|---|---|---|
| `datos/ena_2014_2025_productor.csv.gz` | 297 559 | Base consolidada 2014–2025, una fila por productor y año, 20 variables |
| `datos/ena_muestra_regresion.csv.gz` | 293 910 | Muestra de la regresión logística: casos completos en las 17 variables del modelo (34 192 con crédito) |

Los archivos están comprimidos en gzip. `readr::read_csv()` los lee directamente, sin descomprimirlos.

### Variables

| Variable | Descripción | Codificación |
|---|---|---|
| `anio` | Año de la encuesta | 2014–2019, 2022–2025 |
| `departamento` | Código INEI del departamento | 01–25 |
| `conglomerado` | Conglomerado o segmento de muestreo | código |
| `peso` | Factor de expansión | numérico |
| `credito` | Obtuvo crédito en los últimos 12 meses | 1 = sí, 0 = no |
| `mujer` | Productora mujer | 1 = sí, 0 = no |
| `edad` | Edad del productor | años |
| `educ` | Nivel educativo | 0 = sin nivel/inicial, 1 = primaria, 2 = secundaria, 3 = superior |
| `lengua_originaria` | Lengua materna originaria | 1 = sí, 0 = no |
| `tam_hogar` | Tamaño del hogar | personas |
| `parcelas` | Número de parcelas | número |
| `riego` | Tiene riego | 1 = sí, 0 = no |
| `titulo` | Tiene título de propiedad | 1 = sí, 0 = no |
| `pecuaria` | Tiene actividad pecuaria | 1 = sí, 0 = no |
| `semilla` | Usa semilla certificada | 1 = sí, 0 = no |
| `fertiliz` | Usa fertilizantes | 1 = sí, 0 = no |
| `capacita` | Recibió capacitación | 1 = sí, 0 = no |
| `asist` | Recibió asistencia técnica | 1 = sí, 0 = no |
| `asoc` | Pertenece a una asociación de productores | 1 = sí, 0 = no |
| `region` | Región natural | 1 = costa, 2 = sierra, 3 = selva |

## Código

| Archivo | Qué hace |
|---|---|
| `01_analisis.R` | Lee la base consolidada, recodifica, exporta la muestra de la regresión, estima todos los modelos (logit, diseño muestral, por periodos, 2025, modelo reducido), la selección por razón de verosimilitudes y la validación cruzada aleatoria y por departamento. Guarda `resultados_articulo.rds`. |
| `graficos.R` | Funciones de cada figura y tabla del artículo. |
| `02_figuras_tablas.R` | Exporta las figuras a `figuras/` (PNG, 300 dpi) y las tablas a `tablas/` (CSV). |
| `03_tablas_latex.R` | Exporta las tablas y la ecuación del modelo en LaTeX a `latex/tablas/`. |
| `articulo_ENA_credito_IEEE.qmd` | Documento Quarto del artículo. Genera el PDF y el Word. |
| `latex/main.tex` | Versión LaTeX del artículo (clase IEEEtran). |

## Cómo reproducir

1. Instalar R (≥ 4.4) y los paquetes `tidyverse`, `easystats`, `survey`, `pROC`, `patchwork`, `systemfonts`, `ragg` y `knitr`.
2. Abrir la carpeta del repositorio como directorio de trabajo.
3. Ejecutar `01_analisis.R` (tarda unos 2 minutos), luego `02_figuras_tablas.R` y `03_tablas_latex.R`.
4. Para el artículo: `quarto render articulo_ENA_credito_IEEE.qmd`, o compilar `latex/main.tex` con pdfLaTeX.

La validación cruzada aleatoria usa `set.seed(2026)`.
