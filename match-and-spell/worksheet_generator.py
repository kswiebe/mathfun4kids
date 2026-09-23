#!/usr/bin/env python3
from __future__ import annotations

import argparse
import html
import io
import math
import random
from pathlib import Path

import cairosvg
from pypdf import PdfReader, PdfWriter

PAGE_W = 1000
PAGE_H = 1414
BG = "#fbfbfb"
NAVY = "#102a63"
RED = "#ef2d43"
BLUE = "#dff5f8"
PEACH = "#fde8d5"
PINK = "#ffb8bd"
GRAY = "#777"

LEFT_X = 34
LEFT_W = 330
RIGHT_X = 850
RIGHT_W = 120

DOT_L_X = LEFT_X + LEFT_W + 8
DOT_R_X = RIGHT_X - 8
CENTER_X0 = DOT_L_X + 24
CENTER_X1 = DOT_R_X - 24

HEADER_H = 120
TOP_MARGIN = 35
BOTTOM_TEXT_H = 165
BOTTOM_MARGIN = 34
ROW_GAP = 10

WORDS = [
    "MAUS", "HUND", "MOND", "BAUM",
    "APFEL", "BLUME", "KATZE", "PFERD", "SONNE",
    "STERN", "TIGER", "VOGEL", "WOLKE", "FARBE",
    "BIRNE", "LAMPE", "TASSE", "WIESE",
    "BANANE", "GARTEN", "SCHULE", "FISCHE",
    "ELEFANT", "BALLON", "KINDER", "MALBUCH",
]

ICONS = [
    "apple", "fish", "star", "flower", "balloon",
    "heart", "moon", "tree", "car", "cat",
    "dog", "butterfly", "pencil", "pear", "duck",
    "sun", "cloud", "book",
]

MAX_WORD_LEN = len(ICONS)

def esc(s: str) -> str:
    return html.escape(s, quote=True)

def text(x, y, s, size, weight=500, anchor="start", fill=NAVY):
    return (
        f'<text x="{x}" y="{y}" font-family="Arial, Helvetica, sans-serif" '
        f'font-size="{size}" font-weight="{weight}" text-anchor="{anchor}" '
        f'fill="{fill}">{esc(s)}</text>'
    )

def star_points(cx, cy, r1, r2, n=5):
    pts = []
    for i in range(n * 2):
        a = -math.pi / 2 + i * math.pi / n
        r = r1 if i % 2 == 0 else r2
        pts.append(f"{cx + r * math.cos(a):.1f},{cy + r * math.sin(a):.1f}")
    return " ".join(pts)

def icon_svg(kind, cx, cy, scale=1.0):
    s = scale
    if kind == "apple":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <circle cx="-12" cy="4" r="22" fill="#ef4444" stroke="#222" stroke-width="3"/>
          <circle cx="12" cy="4" r="22" fill="#ef4444" stroke="#222" stroke-width="3"/>
          <rect x="-2" y="-31" width="5" height="18" rx="2" fill="#7c4a21"/>
          <ellipse cx="11" cy="-24" rx="13" ry="7" transform="rotate(-25 11 -24)"
                   fill="#48a23f" stroke="#222" stroke-width="2"/>
        </g>'''
    if kind == "fish":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <ellipse cx="0" cy="0" rx="30" ry="19" fill="#ff9737" stroke="#222" stroke-width="3"/>
          <polygon points="-27,0 -49,-18 -49,18" fill="#ff7a21" stroke="#222" stroke-width="3"/>
          <circle cx="16" cy="-5" r="3.5" fill="#222"/>
          <path d="M 6 8 Q 14 14 22 8" fill="none" stroke="#222" stroke-width="2"/>
        </g>'''
    if kind == "star":
        return (
            f'<polygon points="{star_points(cx, cy, 31*s, 14*s)}" '
            f'fill="#ffd43b" stroke="#222" stroke-width="3"/>'
        )
    if kind == "flower":
        petals = []
        for a in range(0, 360, 60):
            rad = math.radians(a)
            px, py = cx + 20*s*math.cos(rad), cy + 20*s*math.sin(rad)
            petals.append(
                f'<circle cx="{px:.1f}" cy="{py:.1f}" r="{13*s:.1f}" '
                f'fill="#ef76b5" stroke="#222" stroke-width="2"/>'
            )
        return (
            "<g>" + "".join(petals) +
            f'<circle cx="{cx}" cy="{cy}" r="{12*s}" fill="#ffd43b" '
            f'stroke="#222" stroke-width="2"/></g>'
        )
    if kind == "balloon":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <ellipse cx="0" cy="-8" rx="22" ry="28" fill="#6aa9ff" stroke="#222" stroke-width="3"/>
          <polygon points="-5,18 5,18 0,27" fill="#6aa9ff" stroke="#222" stroke-width="2"/>
          <path d="M 0 27 Q 8 43 0 58" fill="none" stroke="#555" stroke-width="2"/>
        </g>'''
    if kind == "heart":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <path d="M0,28 C-45,0 -30,-33 -10,-28 C0,-25 5,-17 8,-10
                   C11,-17 16,-25 26,-28 C46,-33 61,0 8,28 Z"
                fill="#f05b78" stroke="#222" stroke-width="3"/>
        </g>'''
    if kind == "moon":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <circle cx="0" cy="0" r="30" fill="#ffd54a" stroke="#222" stroke-width="3"/>
          <circle cx="14" cy="-7" r="29" fill="{BG}"/>
        </g>'''
    if kind == "tree":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <rect x="-8" y="10" width="16" height="38" fill="#8b5a2b" stroke="#222" stroke-width="2"/>
          <circle cx="-17" cy="0" r="22" fill="#55b94f" stroke="#222" stroke-width="2"/>
          <circle cx="10" cy="-10" r="24" fill="#55b94f" stroke="#222" stroke-width="2"/>
          <circle cx="24" cy="8" r="20" fill="#55b94f" stroke="#222" stroke-width="2"/>
        </g>'''
    if kind == "car":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <rect x="-34" y="-8" width="68" height="28" rx="8" fill="#5da9e9" stroke="#222" stroke-width="3"/>
          <path d="M-22,-8 L-10,-27 H15 L28,-8 Z" fill="#8fd3ff" stroke="#222" stroke-width="3"/>
          <circle cx="-21" cy="22" r="9" fill="#333"/>
          <circle cx="21" cy="22" r="9" fill="#333"/>
        </g>'''
    if kind == "cat":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <circle cx="0" cy="-6" r="25" fill="#a7a7a7" stroke="#222" stroke-width="3"/>
          <polygon points="-20,-24 -30,-45 -7,-32" fill="#a7a7a7" stroke="#222" stroke-width="3"/>
          <polygon points="20,-24 30,-45 7,-32" fill="#a7a7a7" stroke="#222" stroke-width="3"/>
          <circle cx="-8" cy="-10" r="3" fill="#222"/><circle cx="8" cy="-10" r="3" fill="#222"/>
          <path d="M-6,3 Q0,9 6,3" fill="none" stroke="#222" stroke-width="2"/>
        </g>'''
    if kind == "dog":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <circle cx="0" cy="-4" r="25" fill="#b97845" stroke="#222" stroke-width="3"/>
          <ellipse cx="-23" cy="-9" rx="11" ry="22" fill="#8f5d39" stroke="#222" stroke-width="3"/>
          <ellipse cx="23" cy="-9" rx="11" ry="22" fill="#8f5d39" stroke="#222" stroke-width="3"/>
          <circle cx="-8" cy="-8" r="3" fill="#222"/><circle cx="8" cy="-8" r="3" fill="#222"/>
          <circle cx="0" cy="2" r="5" fill="#222"/>
        </g>'''
    if kind == "butterfly":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <ellipse cx="-17" cy="-10" rx="17" ry="23" fill="#f58ac8" stroke="#222" stroke-width="2"/>
          <ellipse cx="17" cy="-10" rx="17" ry="23" fill="#8fc7ff" stroke="#222" stroke-width="2"/>
          <ellipse cx="-13" cy="17" rx="13" ry="17" fill="#ffa54d" stroke="#222" stroke-width="2"/>
          <ellipse cx="13" cy="17" rx="13" ry="17" fill="#7edb91" stroke="#222" stroke-width="2"/>
          <rect x="-3" y="-24" width="6" height="48" rx="3" fill="#333"/>
          <path d="M-2,-23 Q-10,-36 -17,-37 M2,-23 Q10,-36 17,-37"
                fill="none" stroke="#222" stroke-width="2"/>
        </g>'''
    if kind == "pencil":
        return f'''
        <g transform="translate({cx},{cy}) rotate(-35) scale({s})">
          <rect x="-34" y="-8" width="57" height="16" fill="#ffd343" stroke="#222" stroke-width="2"/>
          <polygon points="23,-8 43,0 23,8" fill="#e9c39b" stroke="#222" stroke-width="2"/>
          <polygon points="38,-2 43,0 38,2" fill="#333"/>
          <rect x="-40" y="-8" width="6" height="16" fill="#f58b9b" stroke="#222" stroke-width="2"/>
        </g>'''
    if kind == "pear":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <path d="M0,-31 C-9,-20 -7,-8 -20,5 C-40,27 -23,45 0,46
                   C23,45 40,27 20,5 C7,-8 9,-20 0,-31 Z"
                fill="#9ed84d" stroke="#222" stroke-width="3"/>
          <rect x="-2" y="-46" width="5" height="16" rx="2" fill="#7c4a21"/>
          <ellipse cx="10" cy="-39" rx="12" ry="6" transform="rotate(-25 10 -39)"
                   fill="#48a23f" stroke="#222" stroke-width="2"/>
        </g>'''
    if kind == "duck":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <ellipse cx="0" cy="10" rx="31" ry="21" fill="#ffd84a" stroke="#222" stroke-width="3"/>
          <circle cx="20" cy="-12" r="17" fill="#ffd84a" stroke="#222" stroke-width="3"/>
          <polygon points="34,-10 50,-4 34,2" fill="#ff9f32" stroke="#222" stroke-width="2"/>
          <circle cx="24" cy="-16" r="3" fill="#222"/>
        </g>'''
    if kind == "sun":
        rays = []
        for a in range(0, 360, 45):
            rad = math.radians(a)
            x1, y1 = 35*math.cos(rad), 35*math.sin(rad)
            x2, y2 = 49*math.cos(rad), 49*math.sin(rad)
            rays.append(
                f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" '
                f'stroke="#f6b800" stroke-width="5" stroke-linecap="round"/>'
            )
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          {''.join(rays)}
          <circle cx="0" cy="0" r="29" fill="#ffd43b" stroke="#222" stroke-width="3"/>
        </g>'''
    if kind == "cloud":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <circle cx="-22" cy="3" r="18" fill="#dcecff" stroke="#222" stroke-width="2"/>
          <circle cx="0" cy="-9" r="24" fill="#dcecff" stroke="#222" stroke-width="2"/>
          <circle cx="23" cy="3" r="19" fill="#dcecff" stroke="#222" stroke-width="2"/>
          <rect x="-38" y="2" width="76" height="22" rx="11" fill="#dcecff" stroke="#222" stroke-width="2"/>
        </g>'''
    if kind == "book":
        return f'''
        <g transform="translate({cx},{cy}) scale({s})">
          <path d="M-38,-28 Q-10,-35 0,-21 V31 Q-14,18 -38,23 Z"
                fill="#72c47a" stroke="#222" stroke-width="3"/>
          <path d="M38,-28 Q10,-35 0,-21 V31 Q14,18 38,23 Z"
                fill="#79aef2" stroke="#222" stroke-width="3"/>
          <line x1="0" y1="-21" x2="0" y2="31" stroke="#222" stroke-width="2"/>
        </g>'''
    raise ValueError(kind)

def panel_top():
    return TOP_MARGIN + HEADER_H + 10

def panel_height(n: int) -> float:
    usable = PAGE_H - panel_top() - BOTTOM_TEXT_H - BOTTOM_MARGIN - (n - 1) * ROW_GAP
    return usable / n

def row_center(row: int, n: int) -> float:
    ph = panel_height(n)
    return panel_top() + row * (ph + ROW_GAP) + ph / 2

def object_positions(count, x, y, w, h):
    if count == 1:
        return [(x + w / 2, y + h / 2)]
    cols = math.ceil(math.sqrt(count * w / h))
    cols = max(2, cols)
    rows = math.ceil(count / cols)
    usable_w = w * 0.82
    usable_h = h * 0.76
    pts = []
    for i in range(count):
        r = i // cols
        c = i % cols
        items_this_row = min(cols, count - r * cols)
        row_start = (cols - items_this_row) / 2
        fx = (row_start + c + 0.5) / cols
        fy = (r + 0.5) / rows
        pts.append((
            x + (w - usable_w) / 2 + fx * usable_w,
            y + (h - usable_h) / 2 + fy * usable_h
        ))
    return pts

def icon_scale_for_count(count, n_rows):
    base = 0.95 - max(0, n_rows - 4) * 0.05
    if count <= 2:
        return max(0.42, base)
    if count <= 4:
        return max(0.36, base * 0.82)
    if count <= 6:
        return max(0.31, base * 0.70)
    if count <= 9:
        return max(0.27, base * 0.58)
    return max(0.22, base * 0.48)

def point_segment_distance(px, py, ax, ay, bx, by):
    vx, vy = bx - ax, by - ay
    wx, wy = px - ax, py - ay
    vv = vx * vx + vy * vy
    if vv == 0:
        return math.hypot(px - ax, py - ay)
    t = max(0.0, min(1.0, (wx * vx + wy * vy) / vv))
    qx, qy = ax + t * vx, ay + t * vy
    return math.hypot(px - qx, py - qy)

def correct_segments(count_order, number_order):
    n = len(count_order)
    left_row_for = {value: i for i, value in enumerate(count_order)}
    right_row_for = {value: i for i, value in enumerate(number_order)}
    return {
        value: (
            DOT_L_X, row_center(left_row_for[value], n),
            DOT_R_X, row_center(right_row_for[value], n),
        )
        for value in range(1, n + 1)
    }

def choose_target_points(segs, rng):
    chosen = {}
    n = len(segs)
    for value in range(1, n + 1):
        ax, ay, bx, by = segs[value]
        candidates = [0.22 + i * 0.035 for i in range(17)]
        rng.shuffle(candidates)
        best = None
        best_score = -1.0
        for t in candidates:
            x = ax + t * (bx - ax)
            y = ay + t * (by - ay)
            if not (CENTER_X0 + 15 < x < CENTER_X1 - 15):
                continue
            other_line_dist = min(
                [point_segment_distance(x, y, *segs[m]) for m in range(1, n + 1) if m != value] or [999]
            )
            target_dist = min([math.hypot(x - cx, y - cy) for cx, cy in chosen.values()] or [999])
            score = min(other_line_dist, target_dist)
            if score > best_score:
                best_score = score
                best = (x, y)
        chosen[value] = best
    return chosen

def place_distractors(segs, targets, rng, count):
    letters = list("BCDFGHJKLMNPQRSUVWXYZ")
    pts = []
    attempts = 0
    top = panel_top() + 25
    bottom = PAGE_H - BOTTOM_TEXT_H - BOTTOM_MARGIN - 20
    while len(pts) < count and attempts < 30000:
        attempts += 1
        x = rng.uniform(CENTER_X0 + 15, CENTER_X1 - 15)
        y = rng.uniform(top, bottom)
        if min(point_segment_distance(x, y, *seg) for seg in segs.values()) < 36:
            continue
        if min([math.hypot(x - tx, y - ty) for tx, ty in targets.values()] or [999]) < 62:
            continue
        if min([math.hypot(x - dx, y - dy) for dx, dy, _ in pts] or [999]) < 56:
            continue
        pts.append((x, y, rng.choice(letters)))
    return pts

def shuffled_nontrivial(n, rng):
    values = list(range(1, n + 1))
    left = values[:]
    right = values[:]
    rng.shuffle(left)
    rng.shuffle(right)
    max_fixed = max(1, n // 4)
    attempts = 0
    while sum(a == b for a, b in zip(left, right)) > max_fixed and attempts < 100:
        rng.shuffle(right)
        attempts += 1
    return left, right

def build_word_schedule(words, count, rng):
    schedule = []
    previous_last = None
    while len(schedule) < count:
        cycle = words[:]
        rng.shuffle(cycle)
        if previous_last is not None and len(cycle) > 1 and cycle[0] == previous_last:
            cycle[0], cycle[1] = cycle[1], cycle[0]
        schedule.extend(cycle)
        previous_last = cycle[-1]
    return schedule[:count]

def make_page(word, rng, answer_key=False):
    word = word.upper().strip()
    if not word.isalpha():
        raise ValueError("Solution word must contain letters only.")
    n = len(word)
    if n < 2:
        raise ValueError("Solution word must contain at least two letters.")
    if n > MAX_WORD_LEN:
        raise ValueError(f"Word too long ({n}). Maximum supported length is {MAX_WORD_LEN}.")
    count_order, number_order = shuffled_nontrivial(n, rng)
    icon_order = ICONS[:]
    rng.shuffle(icon_order)
    icon_order = icon_order[:n]
    segs = correct_segments(count_order, number_order)
    targets = choose_target_points(segs, rng)
    distractors = place_distractors(segs, targets, rng, max(5, round(n * 1.5)))
    ph = panel_height(n)
    out = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{PAGE_W}" height="{PAGE_H}" viewBox="0 0 {PAGE_W} {PAGE_H}">',
        f'<rect width="{PAGE_W}" height="{PAGE_H}" fill="{BG}"/>',
        text(34, 60, "Welches Bild passt zu welcher Zahl? Verbinde", 38, 700),
        text(34, 105, "die Paare mit Linien. Nimm ein Lineal.", 38, 700),
    ]
    for row in range(n):
        y = panel_top() + row * (ph + ROW_GAP)
        fill = BLUE if row % 2 == 0 else PEACH
        out.append(f'<rect x="{LEFT_X}" y="{y:.1f}" width="{LEFT_W}" height="{ph:.1f}" fill="{fill}"/>')
        out.append(f'<circle cx="{DOT_L_X}" cy="{row_center(row, n):.1f}" r="10" fill="{RED}"/>')
        qty = count_order[row]
        kind = icon_order[row]
        scale = icon_scale_for_count(qty, n)
        for cx, cy in object_positions(qty, LEFT_X + 8, y + 5, LEFT_W - 16, ph - 10):
            out.append(icon_svg(kind, cx, cy, scale))
        out.append(f'<rect x="{RIGHT_X}" y="{y:.1f}" width="{RIGHT_W}" height="{ph:.1f}" fill="{fill}"/>')
        out.append(f'<circle cx="{DOT_R_X}" cy="{row_center(row, n):.1f}" r="10" fill="{RED}"/>')
        out.append(f'<rect x="{RIGHT_X + 27}" y="{y + ph/2 - 35:.1f}" width="66" height="70" fill="#fff"/>')
        out.append(text(RIGHT_X + 60, y + ph/2 + 15, str(number_order[row]), 40 if n < 10 else 34, 600, "middle"))
    for value in range(1, n + 1):
        x, y = targets[value]
        out.append(text(x, y + 14, word[value - 1], 40 if n < 10 else 34, 500, "middle"))
    for x, y, ch in distractors:
        out.append(text(x, y + 13, ch, 36 if n < 10 else 32, 500, "middle"))
    if answer_key:
        for value in range(1, n + 1):
            ax, ay, bx, by = segs[value]
            out.append(
                f'<line x1="{ax:.1f}" y1="{ay:.1f}" x2="{bx:.1f}" y2="{by:.1f}" '
                f'stroke="#555" stroke-width="3" opacity="0.65"/>'
            )
    panels_end = panel_top() + n * ph + (n - 1) * ROW_GAP
    instr_y = panels_end + 34
    out.append(text(34, instr_y + 30, "Die Buchstaben, die du durchgestrichen hast,", 30, 500))
    out.append(text(34, instr_y + 68, f"ergeben von 1 bis {n} ein Lösungswort. Trage ein.", 30, 500))
    out.append(text(34, instr_y + 140, "Lösung:", 35, 600))
    answer_x0 = 205
    available = PAGE_W - answer_x0 - 35
    gap = 8
    box_w = min(72, (available - gap * (n - 1)) / n)
    box_h = 68
    for i in range(n):
        x = answer_x0 + i * (box_w + gap)
        out.append(f'<rect x="{x:.1f}" y="{instr_y + 100:.1f}" width="{box_w:.1f}" height="{box_h}" fill="{PINK}"/>')
        out.append(text(x + box_w / 2, instr_y + 100 + box_h - 8, str(i + 1), 13 if n < 10 else 11, 400, "middle", GRAY))
    out.append("</svg>")
    return "\n".join(out), count_order, number_order, icon_order

def svg_pages_to_single_pdf(svg_pages, pdf_path: Path):
    writer = PdfWriter()
    for svg in svg_pages:
        pdf_bytes = cairosvg.svg2pdf(bytestring=svg.encode("utf-8"))
        reader = PdfReader(io.BytesIO(pdf_bytes))
        writer.add_page(reader.pages[0])
    with pdf_path.open("wb") as f:
        writer.write(f)

def main():
    p = argparse.ArgumentParser(description="Generate German number/letter matching worksheets.")
    p.add_argument("--out", default="worksheets", help="Directory for SVG files")
    p.add_argument("--count", type=int, default=6, help="Number of worksheets")
    p.add_argument("--seed", type=int, default=None, help="Seed for reproducibility")
    p.add_argument("--word", default=None, help="Specific solution word")
    p.add_argument("--answer-key", action="store_true", help="Also create answer-key versions")
    p.add_argument("--pdf", default=None, help="Optional combined multi-page PDF output path")
    p.add_argument("--no-svg", action="store_true", help="Do not write individual SVG files")
    args = p.parse_args()

    rng = random.Random(args.seed)
    outdir = Path(args.out)
    if not args.no_svg:
        outdir.mkdir(parents=True, exist_ok=True)

    if args.word:
        word_schedule = [args.word.upper()] * args.count
    else:
        word_schedule = build_word_schedule(WORDS, args.count, rng)

    pdf_pages = []
    pdf_key_pages = []

    for idx, word in enumerate(word_schedule, start=1):
        page_seed = rng.randrange(1 << 30)
        page_rng = random.Random(page_seed)
        svg, counts, numbers, icons = make_page(word, page_rng, answer_key=False)

        if not args.no_svg:
            path = outdir / f"arbeitsblatt_{idx:02d}_{word}.svg"
            path.write_text(svg, encoding="utf-8")

        pdf_pages.append(svg)
        print(f"{idx:02d}: solution={word} | left={counts} | right={numbers} | icons={icons}")

        if args.answer_key:
            key_rng = random.Random(page_seed)
            key_svg, _, _, _ = make_page(word, key_rng, answer_key=True)
            if not args.no_svg:
                key_path = outdir / f"arbeitsblatt_{idx:02d}_{word}_loesung.svg"
                key_path.write_text(key_svg, encoding="utf-8")
            pdf_key_pages.append(key_svg)

    if args.pdf:
        pdf_path = Path(args.pdf)
        pdf_path.parent.mkdir(parents=True, exist_ok=True)
        svg_pages_to_single_pdf(pdf_pages, pdf_path)
        print(f"PDF written to {pdf_path}")

        if args.answer_key:
            key_pdf_path = pdf_path.with_name(pdf_path.stem + "_loesung" + pdf_path.suffix)
            svg_pages_to_single_pdf(pdf_key_pages, key_pdf_path)
            print(f"Answer-key PDF written to {key_pdf_path}")

if __name__ == "__main__":
    main()
