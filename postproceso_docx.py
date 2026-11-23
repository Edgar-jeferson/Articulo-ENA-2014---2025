"""Da formato IEEE al Word que genera Quarto (mismo formato que el artículo original):
título, autores y afiliación centrados a una columna; Resumen y Palabras clave en negrita a 9 pt;
el resto del artículo a dos columnas.
Uso: python postproceso_docx.py entrada.docx salida.docx
"""
import re
import sys
import zipfile

TITULO = ("Determinantes del acceso al crédito agropecuario en el Perú mediante regresión logística "
          "y aprendizaje automático con la Encuesta Nacional Agropecuaria 2014–2025")
AUTORES = "Edgar Jeferson Cusihuaman Garate,  Nestor Ademir Ruelas Yana,  Keny Leonel Ccora Quispe,  Yhonel Mamani Crúz"
AFILIACION = "Facultad de Ingeniería Estadística e Informática, Universidad Nacional del Altiplano, Puno, Perú"


def parrafo(texto, tam, despues, cursiva=False):
    i = "<w:i/>" if cursiva else '<w:i w:val="0"/>'
    return (f'<w:p><w:pPr><w:spacing w:after="{despues}"/><w:ind w:firstLine="0"/><w:jc w:val="center"/></w:pPr>'
            f'<w:r><w:rPr><w:rFonts w:ascii="Times New Roman" w:hAnsi="Times New Roman"/>{i}<w:sz w:val="{tam}"/></w:rPr>'
            f'<w:t xml:space="preserve">{texto}</w:t></w:r></w:p>')


COLUMNA = 5040                 # ancho de columna en twips: (12240 − 900 − 900 − 360) / 2
COLUMNA_EMU = COLUMNA * 635    # mismo ancho en EMU (imágenes)

SECT_1COL = ('<w:sectPr><w:pgSz w:w="12240" w:h="15840"/>'
             '<w:pgMar w:left="900" w:right="900" w:top="1080" w:bottom="1440" w:header="720" w:footer="720" w:gutter="0"/>'
             '<w:cols w:num="1"/></w:sectPr>')
SECT_2COL = ('<w:sectPr><w:type w:val="continuous"/><w:pgSz w:w="12240" w:h="15840"/>'
             '<w:pgMar w:left="900" w:right="900" w:top="1080" w:bottom="1440" w:header="720" w:footer="720" w:gutter="0"/>'
             '<w:cols w:num="2" w:space="360"/></w:sectPr>')


def negrita_9pt(p):
    """Todas las corridas del párrafo en negrita y 9 pt (como el resumen del original)."""
    p = re.sub(r"<w:r>(?!<w:rPr>)", "<w:r><w:rPr></w:rPr>", p)
    p = re.sub(r"<w:rPr>(.*?)</w:rPr>",
               lambda m: "<w:rPr>" + re.sub(r"<w:b/>|<w:bCs/>|<w:sz [^>]*/>", "", m.group(1)) + '<w:b/><w:sz w:val="18"/></w:rPr>',
               p)
    return p


def main(entrada, salida):
    zin = zipfile.ZipFile(entrada)
    x = zin.read("word/document.xml").decode("utf-8")
    parrafos = list(re.finditer(r"<w:p[ >].*?</w:p>", x, flags=re.S))

    def buscar(txt):
        return next(m for m in parrafos if txt in re.sub(r"<[^>]+>", "", m.group(0)))

    p_tit, p_res, p_pal = buscar("TITULO_IEEE"), buscar("Resumen"), buscar("Palabras clave")
    # el salto de sección va en un párrafo vacío aparte: si va en "Palabras clave", Word estira esa línea
    nuevo_pal = (negrita_9pt(p_pal.group(0)) +
                 '<w:p><w:pPr><w:spacing w:before="0" w:after="0" w:line="120" w:lineRule="exact"/>' + SECT_1COL + "</w:pPr></w:p>")

    reemplazos = [(p_tit.span(), parrafo(TITULO, 44, 280) + parrafo(AUTORES, 22, 60) + parrafo(AFILIACION, 19, 280, True)),
                  (p_res.span(), negrita_9pt(p_res.group(0))),
                  (p_pal.span(), nuevo_pal)]
    for (a, b), txt in sorted(reemplazos, reverse=True):
        x = x[:a] + txt + x[b:]
    # sección final a dos columnas
    x = re.sub(r"<w:sectPr(?:(?!<w:sectPr).)*?</w:sectPr>(\s*</w:body>)", SECT_2COL + r"\1", x, flags=re.S)
    # Quarto dimensiona tablas e imágenes para una columna de 7920 twips; la columna IEEE mide 5040
    f = COLUMNA / 7920
    x = re.sub(r'(<w:gridCol w:w=")(\d+)', lambda m: m.group(1) + str(int(int(m.group(2)) * f)), x)
    x = re.sub(r'(<w:tcW w:type="dxa" w:w=")(\d+)', lambda m: m.group(1) + str(int(int(m.group(2)) * f)), x)
    x = re.sub(r'(<w:tcW w:w=")(\d+)(" w:type="dxa")', lambda m: m.group(1) + str(int(int(m.group(2)) * f)) + m.group(3), x)
    # ajuste automático al contenido (dentro del ancho de la columna) en lugar de anchos fijos
    x = x.replace('<w:tblLayout w:type="fixed" />', '<w:tblLayout w:type="autofit" />')
    x = x.replace('<w:tblW w:type="auto" w:w="0" />', '<w:tblW w:type="pct" w:w="5000" />')
    # márgenes internos de celda pequeños (2 pt) para que los números no se partan en columnas angostas
    x = x.replace("<w:tblLook ", '<w:tblCellMar><w:left w:w="40" w:type="dxa"/><w:right w:w="40" w:type="dxa"/></w:tblCellMar><w:tblLook ')
    # imágenes más anchas que la columna: se reducen manteniendo la proporción
    def ajustar(m):
        cx, cy = int(m.group(2)), int(m.group(4))
        if cx <= COLUMNA_EMU:
            return m.group(0)
        k = COLUMNA_EMU / cx
        return f"{m.group(1)}{int(cx * k)}{m.group(3)}{int(cy * k)}{m.group(5)}"
    x = re.sub(r'(<wp:extent cx=")(\d+)(" cy=")(\d+)(")', ajustar, x)
    x = re.sub(r'(<a:ext cx=")(\d+)(" cy=")(\d+)(")', ajustar, x)

    estilos = zin.read("word/styles.xml").decode("utf-8")
    # celdas de tabla (estilo Compact) a 7.5 pt, como en el original, y sin la sangría de primera línea
    # que heredan del texto normal (en columnas angostas partía los números)
    estilos = re.sub(r'(<w:style [^>]*w:styleId="Compact"[^>]*>)(.*?)(</w:style>)',
                     lambda m: m.group(1) + re.sub(r"<w:rPr>.*?</w:rPr>", "", m.group(2), flags=re.S)
                     .replace('<w:pPr><w:spacing w:after="36" w:before="36" /></w:pPr>',
                              '<w:pPr><w:spacing w:after="20" w:before="20" /><w:ind w:firstLine="0" /></w:pPr>') +
                     '<w:rPr><w:sz w:val="15"/><w:szCs w:val="15"/></w:rPr>' + m.group(3), estilos, flags=re.S)

    with zipfile.ZipFile(salida, "w", zipfile.ZIP_DEFLATED) as zout:
        for it in zin.infolist():
            if it.filename.endswith("/"):
                continue
            if it.filename == "word/document.xml":
                data = x.encode("utf-8")
            elif it.filename == "word/styles.xml":
                data = estilos.encode("utf-8")
            else:
                data = zin.read(it.filename)
            zout.writestr(it, data)
    print("Guardado", salida)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
