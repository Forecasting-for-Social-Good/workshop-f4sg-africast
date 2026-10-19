#!/usr/bin/env python3
"""Render the AFRICAST 2026 YouTube thumbnails (1280x720), one per day.

Crops each presenter's headshot from images/ with ImageMagick, writes an
HTML page per day and screenshots it with headless Chrome.

    python3 _social/youtube/make_thumbnails.py

Needs ImageMagick (`convert`), Google Chrome and a network connection
(Fira Sans is loaded from Google Fonts).
"""

import subprocess
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
IMAGES = HERE.parent.parent / "images"
CHROME = "google-chrome"

# Presenter: (full name, first name, source headshot, square crop WxH+X+Y)
PEOPLE = {
    "mitchell": ("Mitchell O'Hara-Wild", "Mitchell", "mitch.jpg", "320x320+38+12"),
    "abiodun": ("Abiodun Egwuenu", "Abiodun", "Headshot_Abiodun.jpg", "820x820+34+20"),
    "mustafa": ("Mustafa Aslan", "Mustafa", "mustafa.jpeg", "500x500+149+70"),
    "bahman": ("Bahman Rostami-Tabar", "Bahman", "bahman.jpg", "560x560+63+70"),
    "harsha": ("Harsha Halgamuwe Hewage", "Harsha", "harsha.jpg", "640x640+170+30"),
}

# Day: (title, presenters in session order)
DAYS = {
    1: ("Time series basics &amp; graphics", ["mitchell", "abiodun"]),
    2: ("Features &amp; forecasting", ["mitchell"]),
    3: ("Regression &amp; ARIMA", ["mitchell", "mustafa"]),
    4: ("Forecast accuracy &amp; ETS", ["bahman"]),
    5: ("Diagnostics &amp; foundation models", ["bahman", "harsha"]),
}

# Seasonal series with an 80% and 95% forecast fan, drawn behind the photos
FORECAST_SVG = """<svg aria-hidden="true" viewBox="0 -220 1280 320" style="position:absolute;left:0;bottom:96px;width:1280px;height:320px">
<polygon points="900,-36 920,-84 940,-111 960,-111 980,-88 1000,-65 1020,-64 1040,-92 1060,-132 1080,-159 1100,-159 1120,-136 1140,-113 1160,-112 1180,-140 1200,-180 1220,-207 1240,-207 1260,-184 1280,-161 1280,80 1260,45 1240,11 1220,-1 1200,15 1180,43 1160,59 1140,47 1120,13 1100,-22 1080,-34 1060,-18 1040,10 1020,26 1000,14 980,-20 960,-54 940,-66 920,-51" fill="#3B75AC" fill-opacity="0.16"/>
<polygon points="900,-36 920,-76 940,-101 960,-98 980,-73 1000,-47 1020,-44 1040,-69 1060,-106 1080,-131 1100,-128 1120,-103 1140,-77 1160,-74 1180,-99 1200,-136 1220,-161 1240,-158 1260,-133 1280,-107 1280,26 1260,-6 1240,-38 1220,-47 1200,-28 1180,2 1160,21 1140,11 1120,-20 1100,-52 1080,-62 1060,-43 1040,-12 1020,6 1000,-3 980,-35 960,-67 940,-76 920,-58" fill="#3B75AC" fill-opacity="0.28"/>
<polyline points="0,-4 20,22 40,33 60,8 80,-28 100,-44 120,-30 140,-3 160,25 180,23 200,6 220,-32 240,-55 260,-50 280,-20 300,20 320,25 340,3 360,-31 380,-63 400,-55 420,-21 440,9 460,17 480,-4 500,-51 520,-64 540,-57 560,-31 580,-7 600,4 620,-24 640,-45 660,-67 680,-66 700,-42 720,-3 740,-2 760,-19 780,-54 800,-81 820,-76 840,-45 860,-19 880,-17 900,-36" fill="none" stroke="#434A5C" stroke-opacity="0.45" stroke-width="4" stroke-linejoin="round"/>
<polyline points="900,-36 920,-67 940,-89 960,-83 980,-54 1000,-25 1020,-19 1040,-41 1060,-75 1080,-96 1100,-90 1120,-62 1140,-33 1160,-27 1180,-48 1200,-82 1220,-104 1240,-98 1260,-69 1280,-41" fill="none" stroke="#2E5F8E" stroke-opacity="0.7" stroke-width="4" stroke-linejoin="round"/>
</svg>"""


def crop_headshots(dest):
    for key, (_, _, src, crop) in PEOPLE.items():
        subprocess.run(
            ["convert", str(IMAGES / src), "-crop", crop, "+repage",
             "-resize", "600x600", "-quality", "90", str(dest / f"{key}.jpg")],
            check=True,
        )


def page(n, title, people, faces):
    solo = len(people) == 1
    size = 360 if solo else 250
    top = 160 if solo else 150
    photos = "".join(
        f"""<div style="display:flex;flex-direction:column;align-items:center;gap:12px;margin-top:{i * 80}px">
<img src="{(faces / f'{k}.jpg').as_uri()}" alt="{PEOPLE[k][0]}" style="width:{size}px;height:{size}px;border-radius:50%;object-fit:cover;border:10px solid #FBFCF7;display:block">
<span style="font-size:30px;font-weight:600;color:#1F2A36">{PEOPLE[k][1]}</span></div>"""
        for i, k in enumerate(people)
    )
    return f"""<!doctype html><html lang="en"><head><meta charset="utf-8">
<link href="https://fonts.googleapis.com/css2?family=Fira+Sans:wght@400;600;700&display=swap" rel="stylesheet">
<style>body{{margin:0}}</style></head><body>
<div style="width:1280px;height:720px;position:relative;overflow:hidden;background:#EEF4DF;font-family:'Fira Sans',sans-serif;color:#1F2A36">
{FORECAST_SVG}
<img src="{(IMAGES / 'savanna-wide.png').as_uri()}" alt="" style="position:absolute;left:0;bottom:0;width:1280px;height:134px;object-fit:cover;object-position:left bottom">
<div style="position:absolute;left:56px;top:36px;display:flex;align-items:center;gap:6px">
<img src="{(IMAGES / 'africast-mark.png').as_uri()}" alt="" style="height:128px;width:auto">
<span style="font-size:92px;font-weight:700;color:#3B75AC;letter-spacing:0.01em;margin-top:30px">AFRICAST</span></div>
<span style="position:absolute;right:56px;top:56px;background:#434A5C;color:#FFFFFF;font-size:52px;font-weight:700;padding:6px 34px;border-radius:999px;letter-spacing:0.04em">DAY {n}</span>
<div style="position:absolute;left:64px;top:216px;width:590px;font-size:80px;font-weight:700;line-height:1.02;color:#1F2A36">{title}</div>
<div style="position:absolute;right:56px;top:{top}px;display:flex;gap:24px;align-items:flex-start">{photos}</div>
</div></body></html>"""


def main():
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        crop_headshots(tmp)
        for n, (title, people) in DAYS.items():
            html = tmp / f"day{n}.html"
            html.write_text(page(n, title, people, tmp))
            out = HERE / f"africast-2026-day{n}.png"
            subprocess.run(
                [CHROME, "--headless=new", "--disable-gpu", "--hide-scrollbars",
                 f"--user-data-dir={tmp / 'chrome'}", "--allow-file-access-from-files",
                 "--window-size=1280,720", f"--screenshot={out}", html.as_uri()],
                check=True, timeout=120, stderr=subprocess.DEVNULL,
            )
            print(out)


if __name__ == "__main__":
    main()
