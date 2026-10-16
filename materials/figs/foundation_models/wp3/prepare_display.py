"""Prepare vector display copies of the two user-selected WP3 PDF figures."""
from pathlib import Path
from copy import deepcopy
from pypdf import PdfReader, PdfWriter, PageObject, Transformation
from pypdf.generic import RectangleObject, NameObject
import pypdfium2 as pdfium

ROOT=Path(__file__).resolve().parent

def save(page,name):
    writer=PdfWriter(); writer.add_page(page)
    with (ROOT/f"{name}.pdf").open("wb") as f: writer.write(f)
    doc=pdfium.PdfDocument(ROOT/f"{name}.pdf")
    doc[0].render(scale=1.6).to_pil().save(ROOT/f"{name}.png")

source=PdfReader(ROOT/"fig-wp3_runtime_full-1.pdf").pages[0]
canvas=PageObject.create_blank_page(width=864,height=335)
# Original top RMSSE row, runtime axis label alone, and complete model legend.
for rect,dy in [((0,304,864,504),-169),((350,102,560,118),0),((0,0,864,96),0)]:
    region=deepcopy(source)
    # Remove text outside this view, not merely its visible ink. Retain state
    # operations so font/colour changes are identical for the remaining labels.
    stream=region.get_contents()
    visible=False
    clean=[]
    for operands,operator in stream.operations:
        if operator == b"BT": visible=False
        if operator == b"Tm":
            x,y=float(operands[4]),float(operands[5])
            visible=rect[0] <= x <= rect[2] and rect[1] <= y <= rect[3]
        if operator in (b"Tj",b"TJ",b"'",b'"') and not visible:
            continue
        clean.append((operands,operator))
    stream.operations=clean
    region[NameObject("/Contents")]=stream
    region.cropbox=RectangleObject(rect)
    canvas.merge_transformed_page(region,Transformation().translate(0,dy))
save(canvas,"lao_runtime_rmsse")

fp=deepcopy(PdfReader(ROOT/"fig-wp3_runtime_rmsse-1.pdf").pages[0])
fp.mediabox=RectangleObject((0,0,900,720))
fp.cropbox=RectangleObject((0,0,900,720))
save(fp,"fp_runtime_rmsse_display")
print("Original vector results preserved; Lao sPIN panels excluded; FP legend revealed.")
