"""Rebuild savanna.png as a wide, seamlessly tiling skyline.

The source is a single colour (#0E2224) with three alpha tones painted
back to front: far hills (95), mid ground (184) and near objects (255).
Sprites are lifted from the source, the terrain is redrawn as periodic
curves, and everything is placed with wrap-around so the strip tiles.

usage: python3 images/savanna-wide.py images/savanna.png images/savanna-wide.png [WIDTH]
"""
import sys
import numpy as np
from PIL import Image
from scipy import ndimage as ndi

src, out = sys.argv[1], sys.argv[2]
W = int(sys.argv[3]) if len(sys.argv) > 3 else 3840
A = np.asarray(Image.open(src).convert("RGBA"))[..., 3].astype(float) / 255
H = A.shape[0]
FAR, MID, NEAR = 95 / 255, 184 / 255, 1.0
TONES = np.array([0, FAR, MID, NEAR])
rng = np.random.default_rng(11)

# --- source analysis -------------------------------------------------------

def run_top(solid):
    """Top of the solid run touching the bottom edge, per column."""
    top = np.full(solid.shape[1], solid.shape[0])
    for x in range(solid.shape[1]):
        col = solid[:, x]
        y = len(col)
        while y > 0 and col[y - 1]:
            y -= 1
        top[x] = y
    return top

# Mid ground line: grey opening removes trunks and legs, keeps the ground.
g0 = run_top(A > 0.55)
src_ground = ndi.minimum_filter1d(ndi.maximum_filter1d(g0, 41, mode="wrap"), 41, mode="wrap")

# Background tone under each pixel: local minimum, snapped down to a tone.
local_min = ndi.minimum_filter(A, size=3, mode="nearest")
bg_tone = TONES[np.searchsorted(TONES, local_min + 0.03) - 1]

def sprite(x0, y0, x1, y1, tone, cut=None, grounded=False):
    a = A[y0:y1, x0:x1]
    bg = np.minimum(bg_tone[y0:y1, x0:x1], TONES[TONES < tone].max())
    c = np.clip((a - bg) / (tone - bg), 0, 1)
    # only the sprite's own tone, plus a 1px antialiased rim
    lower = TONES[TONES < tone].max()
    c *= ndi.binary_dilation(a > lower + 0.6 * (tone - lower), iterations=1)
    c[c < 0.05] = 0
    if cut:
        for cx0, cy0, cx1, cy1 in cut:
            c[cy0 - y0:cy1 - y0, cx0 - x0:cx1 - x0] = 0
    base = None
    if grounded:
        g = src_ground[x0:x1] - y0
        rows = np.arange(c.shape[0])[:, None]
        c[rows >= g[None, :] - 1] = 0
        base = int(np.median(g))
        c = c[:base - 1]
    # keep only the main blob(s), dropping stray terrain specks
    lab, n = ndi.label(ndi.binary_dilation(c > 0.3, iterations=2))
    if n:
        sizes = ndi.sum(c > 0.3, lab, range(1, n + 1))
        keep = np.isin(lab, 1 + np.flatnonzero(sizes >= max(4, 0.05 * sizes.max())))
        c = c * keep
    return dict(c=c, tone=tone, y0=y0, base=base)

S = {
    "tree": sprite(150, 16, 362, H, NEAR, cut=[(230, 112, 266, H)]),
    "cub": sprite(232, 112, 267, H, NEAR),
    "lion": sprite(54, 102, 120, H, NEAR),
    "lioness": sprite(151, 117, 211, H, NEAR),
    "bush": sprite(822, 108, 874, H, NEAR),
    "acacia": sprite(744, 18, 866, 110, MID, cut=[(744, 80, 774, 110)], grounded=True),
    "hyena": sprite(745, 82, 773, 110, MID, grounded=True),
    "grazer": sprite(704, 82, 739, 110, MID, grounded=True),
    "shrub": sprite(737, 90, 747, 110, MID, grounded=True),
    "gnu": sprite(17, 84, 51, 110, MID, grounded=True),
    "tuft": sprite(165, 86, 192, 110, MID, grounded=True),
    "flock_a": sprite(86, 10, 124, 46, FAR),
    "flock_b": sprite(124, 10, 162, 46, FAR),
}

# --- terrain ---------------------------------------------------------------

xs = np.arange(W)

def periodic_noise(n_waves, amp, min_cycles, max_cycles):
    out = np.zeros(W)
    for _ in range(n_waves):
        k = rng.integers(min_cycles, max_cycles + 1)
        out += rng.uniform(0.3, 1) * np.sin(2 * np.pi * k * xs / W + rng.uniform(0, 2 * np.pi))
    return amp * out / np.abs(out).max()

def wrapdist(x, c):
    d = x - c
    return (d + W / 2) % W - W / 2

# Far hills: a ridge of soft, asymmetric peaks over a rolling base.
hills = 74 + periodic_noise(4, 5, 2, 9)
for c in np.linspace(0, W, 11, endpoint=False) + rng.uniform(-120, 120, 11):
    height = rng.uniform(22, 48)
    d = wrapdist(xs, c)
    w = np.where(d < 0, rng.uniform(70, 150), rng.uniform(90, 190))
    bump = height * np.exp(-0.5 * (d / w) ** 2) ** 1.6
    hills = np.minimum(hills, 74 - bump + periodic_noise(3, 1.5, 30, 60) * (bump > 2))
hills += periodic_noise(3, 1.2, 40, 90)

ground = 103 + periodic_noise(4, 4.5, 3, 11)

yy = np.arange(H)[:, None] + 0.5
def cover(line):
    return np.clip(yy - line[None, :], 0, 1)

img = np.zeros((H, W))
img = img * (1 - cover(hills)) + FAR * cover(hills)

def paint(img, name, x, flip=False, scale=1.0, dy=0):
    s = S[name]
    c = s["c"][:, ::-1] if flip else s["c"]
    if scale != 1.0:
        h, w = c.shape
        c = np.asarray(Image.fromarray((c * 255).astype(np.uint8)).resize(
            (round(w * scale), round(h * scale)), Image.LANCZOS)) / 255
    h, w = c.shape
    if s["base"] is not None:
        # stand on the new ground; legs and trunks run down into it
        gx = ground[(x + np.arange(w)) % W]
        top = int(round(np.median(gx))) - h + 1 + dy
        full = np.zeros((H - top, w))
        full[:h] = c
        full[h:] = c[-1:]
        c, top = full, top
    else:
        top = H - h if s["y0"] + s["c"].shape[0] >= H else s["y0"] + dy
    cols = (x + np.arange(c.shape[1])) % W
    y0 = max(top, 0)
    c = c[y0 - top:H - top]
    region = img[y0:y0 + c.shape[0], :][:, cols]
    img[y0:y0 + c.shape[0], cols] = region * (1 - c) + s["tone"] * c
    return img

# far: birds
for name, x, y, flip in [("flock_a", 420, -2, False), ("flock_b", 1640, 4, True),
                         ("flock_a", 2510, 0, True), ("flock_b", 3420, -4, False)]:
    img = paint(img, name, x, flip, dy=y)

# mid: the ground band, then what stands on it
img = img * (1 - cover(ground)) + MID * cover(ground)
for name, x, flip, scale in [
    ("acacia", 640, False, 1.0), ("hyena", 610, True, 1.0), ("shrub", 735, False, 1.0),
    ("gnu", 40, False, 1.0), ("tuft", 470, False, 1.0),
    ("tuft", 1015, True, 0.8), ("grazer", 1060, True, 1.0), ("gnu", 1110, False, 1.0),
    ("acacia", 1890, True, 0.85), ("hyena", 1840, False, 1.0), ("tuft", 2050, False, 1.0),
    ("shrub", 1600, True, 1.0),
    ("acacia", 2780, False, 1.1), ("grazer", 2700, False, 1.0), ("shrub", 2930, True, 1.0),
    ("gnu", 3470, True, 1.0), ("hyena", 3515, True, 1.0), ("tuft", 3720, True, 0.9),
    ("tuft", 2340, True, 0.9), ("grazer", 3160, True, 1.0),
]:
    img = paint(img, name, x, flip, scale)

# near: the big trees and the pride
for name, x, flip, scale in [
    ("tree", 180, False, 1.0), ("lion", 30, False, 1.0), ("cub", 300, True, 1.0),
    ("lioness", 520, True, 1.0), ("bush", 870, False, 1.0),
    ("tree", 1260, True, 0.92), ("lioness", 1180, False, 1.0), ("lion", 1560, True, 1.0),
    ("bush", 1990, True, 0.8),
    ("lion", 2150, False, 1.0), ("tree", 2420, False, 0.96), ("cub", 2460, False, 1.0),
    ("lioness", 2690, False, 1.0),
    ("bush", 3050, False, 1.0), ("tree", 3280, True, 1.0), ("lion", 3560, True, 1.0),
    ("cub", 3390, True, 1.0),
]:
    img = paint(img, name, x, flip, scale)

rgba = np.zeros((H, W, 4), np.uint8)
rgba[..., :3] = (14, 34, 36)
rgba[..., 3] = np.clip(np.round(img * 255), 0, 255)
Image.fromarray(rgba).save(out, optimize=True)
