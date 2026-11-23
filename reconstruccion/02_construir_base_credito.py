"""
Paso 2: construye la base del artículo de crédito, una fila por productor y año (ENA 2014-2025).

Entradas:
  intermedio/base_credito_2014_2024.parquet  (01_extraer_credito.py)
  VALIDACION TECNICA/datos/crudos/ENA_2025/*.sav
Salidas (en datos/, mismo formato que espera 01_analisis.R del repo):
  ena_2014_2025_productor.csv.gz   20 variables
  ena_muestra_regresion.csv.gz     casos completos en las 17 variables del modelo
  reporte_reconstruccion.txt

Definiciones (1 = sí, 0 = no):
  credito   P901 == 1 y P902 == 1 (solicitó y obtuvo); 0 si P901 respondida y no obtuvo
  mujer     P1103 == 2 del productor (P1102 == 1)
  edad      P1104_A del productor
  educ      P1105: 1-2 -> 0 sin nivel/inicial, 3-4 -> 1 primaria, 5-6 -> 2 secundaria, 7-10 -> 3 superior
  lengua_originaria  P1106 en {1 quechua, 2 aymara, 3 otra nativa}
  tam_hogar número de miembros del hogar (filas P1100)
  parcelas  P103 (si falta, número de parcelas distintas P105_N)
  riego     alguna parcela/cultivo con agua que no es lluvia (P212 en 2-7) o bajo riego (P119 == 1)
  titulo    alguna parcela con título, inscrito o no (P111/P112 en {1, 2})
  pecuaria  P102_2 == 1 (2014-2022); crió animales P401A/P405A (2023-2025)
  semilla   algún cultivo con semilla certificada (P214 == 1 o P122 == 1)
  fertiliz  P238 == 1
  capacita  P701 == 1 ; asist P704 == 1 ; asoc P801 == 1
  region    REGION 1 costa, 2 sierra, 3 selva
"""
import glob
from pathlib import Path
import numpy as np
import pandas as pd
import pyreadstat

AQUI = Path(__file__).parent
BASE = AQUI / "intermedio" / "base_credito_2014_2024.parquet"
DIR25 = Path(r"D:/UNIVERSIDAD/8vo/CienciaDeDatosI/GRUPO/VALIDACION TECNICA/datos/crudos/ENA_2025")
OUT = AQUI / "datos"
LOG = []


def log(m=""):
    print(m); LOG.append(str(m))


def num(s):
    return pd.to_numeric(s, errors="coerce")


def txt(s):
    return s.astype("string").str.strip().str.replace(r"\.0$", "", regex=True).fillna("")


def id_ua(df):
    a = num(df["ANIO"]).astype(int)
    viejo = a.astype(str) + "|" + txt(df["CONGLOMERADO"]) + "|" + txt(df["NSELUA"]) + "|" + txt(df["UA"])
    nuevo = (a.astype(str) + "|" + txt(df["CCDD"]).str[:2] + txt(df["CCPP"]) + txt(df["CCDI"]) + "|"
             + txt(df["NSEGM"]) + "|" + txt(df["ID_PROD"]) + "|" + txt(df["UA"]))
    return pd.Series(np.where(a <= 2022, viejo, nuevo), index=df.index)


def leer25(nombre, cols):
    f = glob.glob(str(DIR25 / nombre))[0]
    _, m = pyreadstat.read_sav(f, metadataonly=True)
    llaves = ["ANIO", "CCDD", "CCPP", "CCDI", "NSEGM", "ID_PROD", "UA", "REGION", "FACTOR_PRODUCTOR", "CODIGO"]
    usar = [c for c in llaves + cols if c in m.column_names]
    try:
        d, _ = pyreadstat.read_sav(f, usecols=usar)
    except Exception:
        d, _ = pyreadstat.read_sav(f, usecols=usar, encoding="latin1")
    for c in llaves + cols + ["CONGLOMERADO", "NSELUA"]:
        if c not in d.columns:
            d[c] = pd.NA
    return d


def cargar_2025():
    mods = {"17_CAP900.sav": ["P901", "P902"], "15_CAP700.sav": ["P701", "P704"], "16_CAP800.sav": ["P801"],
            "19_CAP1100.sav": ["P1100", "P1102", "P1103", "P1104_A", "P1105", "P1106"],
            "01_CAP100A_01.sav": ["P103"], "01_CAP100A_03.sav": ["P112"], "01_CAP100A_04.sav": ["P119", "P122"],
            "03_CAP200AB.sav": ["P212", "P214"], "07_CAP200E.sav": ["P238"],
            "10_CAP400A_1.sav": ["P401A"], "10_CAP400A_2.sav": ["P405A"]}
    return pd.concat([leer25(f, c) for f, c in mods.items()], ignore_index=True)


def agregar(df):
    """Colapsa las filas de todos los módulos a una fila por productor (id_ua)."""
    df = df.copy()
    df["id"] = id_ua(df)
    df["ANIO"] = num(df["ANIO"]).astype(int)
    for c in ["P901", "P902", "P1102", "P1103", "P1104_A", "P1105", "P1106", "P103", "P111", "P112", "P119",
              "P122", "P212", "P214", "P238", "P102_2", "P401A", "P701", "P704", "P801", "REGION"]:
        if c in df.columns:
            df[c] = num(df[c])
        else:
            df[c] = np.nan
    for c in ["P105_N", "P405A", "P1100", "FACTOR"]:
        if c not in df.columns:
            df[c] = pd.NA
    g = df.groupby("id", sort=False)
    o = pd.DataFrame(index=g.size().index)
    o["anio"] = g["ANIO"].first()
    o["departamento"] = txt(g["CCDD"].first()).str[:2].str.zfill(2)
    o["conglomerado"] = np.where(o["anio"] <= 2022, g["CONGLOMERADO"].first().pipe(txt),
                                 (g["CCDD"].first().pipe(txt).str[:2] + g["CCPP"].first().pipe(txt)
                                  + g["CCDI"].first().pipe(txt) + "-" + g["NSEGM"].first().pipe(txt)))
    peso = num(g["FACTOR"].first()) if "FACTOR" in df else np.nan
    o["peso"] = peso.fillna(num(g["FACTOR_PRODUCTOR"].first())) if "FACTOR_PRODUCTOR" in df else peso

    p901 = g["P901"].max(); p902 = g["P902"].min()
    o["credito"] = np.where(p901.isna(), np.nan, ((p901 == 1) & (p902 == 1)).astype(float))
    # modelo en dos etapas: solicitud (demanda) y obtención entre quienes solicitaron (oferta)
    o["solicito"] = np.where(p901.isna(), np.nan, (p901 == 1).astype(float))
    o["obtuvo"] = np.where(p901 == 1, np.where(p902.isna(), np.nan, (p902 == 1).astype(float)), np.nan)

    prod = df[df["P1102"] == 1].groupby("id")[["P1103", "P1104_A", "P1105", "P1106"]].first()
    o = o.join(prod)
    o["mujer"] = (o["P1103"] == 2).astype(float).where(o["P1103"].notna())
    o["edad"] = o["P1104_A"]
    o["educ"] = pd.cut(o["P1105"], [0, 2, 4, 6, 10], labels=[0, 1, 2, 3]).astype(float)
    o["lengua_originaria"] = o["P1106"].isin([1, 2, 3]).astype(float).where(o["P1106"].notna())
    o["tam_hogar"] = df[df["P1100"].notna()].groupby("id")["P1100"].nunique()
    o["tam_hogar"] = o["tam_hogar"].where(o["edad"].notna())

    par = g["P103"].max()
    # sin parcelas registradas (productores pecuarios sin tierra) -> faltante, como en el artículo
    o["parcelas"] = par.fillna(df[df["P105_N"].notna()].groupby("id")["P105_N"].nunique())

    def alguna(cond):
        return df.assign(_c=cond).groupby("id")["_c"].max().astype(float)

    o["riego"] = alguna(df["P212"].between(2, 7) | (df["P119"] == 1))
    # título: solo inscrito en registros públicos (el que sirve como garantía; definición del artículo original)
    o["titulo"] = alguna((df["P111"] == 1) | (df["P112"] == 1))
    o["titulo_cualquiera"] = alguna(df["P111"].isin([1, 2]) | df["P112"].isin([1, 2]))  # inscrito o no (sensibilidad)
    o["pecuaria"] = alguna((df["P102_2"] == 1) | (df["P401A"] == 1) | df["P405A"].notna())
    o["semilla"] = alguna((df["P214"] == 1) | (df["P122"] == 1))
    o["fertiliz"] = alguna(df["P238"] == 1)
    o["capacita"] = alguna(df["P701"] == 1)
    o["asist"] = alguna(df["P704"] == 1)
    o["asoc"] = alguna(df["P801"] == 1)
    o["region"] = g["REGION"].max()
    o["codigo"] = num(g["CODIGO"].max()) if "CODIGO" in df else np.nan
    return o.reset_index(drop=True)


COLS = ["anio", "departamento", "conglomerado", "peso", "credito", "mujer", "edad", "educ", "lengua_originaria",
        "tam_hogar", "parcelas", "riego", "titulo", "pecuaria", "semilla", "fertiliz", "capacita", "asist",
        "asoc", "region", "solicito", "obtuvo", "titulo_cualquiera"]
MODELO = ["credito", "mujer", "edad", "educ", "lengua_originaria", "tam_hogar", "parcelas", "riego", "titulo",
          "pecuaria", "semilla", "fertiliz", "capacita", "asist", "asoc", "region", "anio"]


def main():
    b = pd.read_parquet(BASE)
    b = b[~num(b["ANIO"]).isin([2021])]  # 2021: P901/P902 vacías
    partes = [agregar(b[num(b["ANIO"]) == a]) for a in sorted(num(b["ANIO"]).unique())]
    partes.append(agregar(cargar_2025()))
    ena = pd.concat(partes, ignore_index=True)
    log(f"Unidades agropecuarias (todas): {len(ena):,}")
    ena = ena[ena["credito"].notna()]  # productores que respondieron el módulo de crédito
    log(f"Con módulo de crédito respondido: {len(ena):,}")
    log("Por año y tipo de productor (CODIGO 1 = pequeño/mediano, 2 = grande):")
    log(ena.groupby(["anio", "codigo"]).size().unstack().to_string())
    # Limpieza: solo pequeños y medianos productores (los grandes son empresas sin módulo de hogar)
    ena = ena[ena["codigo"] == 1][COLS]
    log(f"Base consolidada, solo pequeños y medianos productores: {len(ena):,}   [artículo: 297 559]")
    log(f"  2014-2024: {(ena['anio'] <= 2024).sum():,} [artículo: 259 110]   2025: {(ena['anio'] == 2025).sum():,} [artículo: 38 449]")
    muestra = ena.dropna(subset=MODELO)
    log(f"Casos completos en las 17 variables: {len(muestra):,}   [artículo: 293 910]")
    log(f"Con crédito en la muestra: {int(muestra['credito'].sum()):,}   [artículo: 34 192]")
    log("\nPor año: n, % crédito   [artículo: 2014 13.8 %, 2019 9.7 %, 2025 11.3 %]")
    log(muestra.groupby("anio").agg(n=("credito", "size"), pct=("credito", lambda s: round(100 * s.mean(), 1)))
        .to_string())
    log("\nMedias según crédito (muestra):")
    log(muestra.groupby("credito")[MODELO[1:-1]].mean().T.round(3).to_string())
    log("\n% faltantes en la base consolidada:")
    log((ena.isna().mean() * 100).round(1).to_string())
    OUT.mkdir(exist_ok=True)
    ena.to_csv(OUT / "ena_2014_2025_productor.csv.gz", index=False)
    muestra.to_csv(OUT / "ena_muestra_regresion.csv.gz", index=False)
    (AQUI / "reporte_reconstruccion.txt").write_text("\n".join(LOG), encoding="utf-8")


if __name__ == "__main__":
    main()
