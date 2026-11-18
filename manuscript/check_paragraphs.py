"""Conta as linhas de cada parágrafo de corpo no PDF renderizado (A4, Times 12, 1,5).

Uso: python3 check_paragraphs.py index.docx index.pdf
Converte o .docx em .pdf com LibreOffice antes de executar.
"""
import re
import subprocess
import sys

import docx


def norm(s):
    return re.sub(r"\s+", " ", s).strip()


def main(docx_path, pdf_path):
    txt = subprocess.run(["pdftotext", pdf_path, "-"], capture_output=True, text=True).stdout
    lines = [norm(l) for l in txt.split("\n")]
    lines = [l for l in lines if l]
    flat, line_of = "", []
    for i, l in enumerate(lines):
        start = len(flat)
        flat += l + " "
        line_of.extend([i] * (len(flat) - start))
    d = docx.Document(docx_path)
    out, pos = [], 0
    for p in d.paragraphs:
        t = norm(p.text)
        if p.style.name not in ("Body Text", "First Paragraph") or len(t) < 200 or t.startswith("Nota."):
            continue
        head, tail = t[:35], t[-35:]
        a = flat.find(head, pos)
        if a < 0:
            out.append((t[:60], None)); continue
        b = flat.find(tail, a)
        if b < 0:
            out.append((t[:60], None)); continue
        b = b + len(tail) - 1
        n = line_of[b] - line_of[a] + 1
        out.append((t[:60], n)); pos = b
    bad = [(h, n) for h, n in out if n is None or n < 7 or n > 11]
    for h, n in out:
        flag = "" if n is not None and 7 <= n <= 11 else "  <-- FORA"
        print(f"{n!s:>4}  {h}{flag}")
    print(f"\n{len(out)} parágrafos; fora do intervalo 7-11: {len(bad)}")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(*sys.argv[1:3]))
