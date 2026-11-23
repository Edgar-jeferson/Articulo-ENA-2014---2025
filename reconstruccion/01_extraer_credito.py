"""Paso 1: extrae de ENA_2014_2024.sav las columnas del artículo de crédito -> parquet."""
import pyreadstat, json
from pathlib import Path
SAV = Path(r"D:/UNIVERSIDAD/8vo/CienciaDeDatosI/GRUPO/VALIDACION TECNICA/datos/crudos/ENA_2014_2024.sav")
OUT = Path(__file__).parent / "intermedio"; OUT.mkdir(exist_ok=True)
COLS = ["ANIO","CCDD","CCPP","CCDI","CONGLOMERADO","NSELUA","NSEGM","ID_PROD","UA","RESFIN","REGION","CODIGO",
        "FACTOR","FACTOR_PRODUCTOR","P101A","P102_1","P102_2","P103","P14_TOTPARCELAS","P105_N","P110_1","P111","P112",
        "P119","P120","P121","P122","P212","P213","P214","P235","P235A","P236","P238",
        "P401A","P405A","P602_7","P701","P704","P801","P901","P902","P1100","P1102","P1103","P1104_A","P1105","P1106"]
_, m = pyreadstat.read_sav(SAV, metadataonly=True, encoding="latin1")
usar = [c for c in COLS if c in m.column_names]
print("faltan:", [c for c in COLS if c not in usar])
fx = lambda s: s.encode("latin1","replace").decode("utf-8","replace") if isinstance(s,str) else s
etq = {c: {str(k): fx(v) for k, v in m.variable_value_labels.get(c, {}).items()} for c in usar}
json.dump(etq, open(OUT/"etiquetas_2014_2024.json","w",encoding="utf-8"), ensure_ascii=False, indent=1)
df, _ = pyreadstat.read_sav(SAV, usecols=usar, encoding="latin1")
for c in df.columns:
    if df[c].dtype == object: df[c] = df[c].astype("string").str.strip()
df.to_parquet(OUT/"base_credito_2014_2024.parquet", index=False)
print(df.shape)
