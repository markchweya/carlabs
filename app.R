# app.R — CARLAB (Vision Pro / Hologram Edition with Wheel Text, nav fixed)
# FIX: lightweight priority dropdown was clipped/hidden because parent glass-card had overflow:hidden.
# Minimal CSS override only for the preferences (and filters) cards + higher dropdown z-index.

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)

# ---------------- Data ----------------
mtcars_data <- mtcars
mtcars_data$car <- rownames(mtcars)
rownames(mtcars_data) <- NULL

mtcars_data <- mtcars_data %>%
  mutate(
    transmission = ifelse(am == 0, "Automatic", "Manual"),
    cylinders    = factor(cyl)
  )

pref_levels <- c("Low", "Medium", "High", "Very high")

pref_to_weight <- function(x) {
  switch(
    x,
    "Low"       = 1,
    "Medium"    = 2,
    "High"      = 3,
    "Very high" = 4,
    1
  )
}

# helper: brand class
brand_class_for <- function(name) {
  brand <- tolower(sub(" .*", "", name))
  paste0("brand-", brand)
}

# ---------------- UI ----------------
ui <- fluidPage(
  theme = bs_theme(
    bootswatch   = "flatly",
    primary      = "#00e5ff",
    base_font    = font_google("Space Grotesk"),
    heading_font = font_google("Space Grotesk")
  ),
  
  tags$head(
    tags$style(HTML('
      html, body { height: 100%; }
      body {
        margin: 0;
        background: radial-gradient(circle at top,
                  #0b1220 0,
                  #020617 45%,
                  #000000 100%);
        color: #f9fafb;
        overflow-x: hidden;
        overflow-y: hidden;
      }

      body::before {
        content: "";
        position: fixed;
        inset: -40%;
        background: conic-gradient(
          from 180deg,
          rgba(0,229,255,0.52),
          rgba(99,102,241,0.62),
          rgba(236,72,153,0.52),
          rgba(34,197,94,0.52),
          rgba(0,229,255,0.52)
        );
        opacity: 0.18;
        filter: blur(80px);
        z-index: -3;
        animation: spinGlow 30s linear infinite;
      }
      @keyframes spinGlow {
        from { transform: rotate(0deg); }
        to   { transform: rotate(360deg); }
      }

      .app-shell {
        padding: 12px 24px 20px 24px;
        min-height: 100vh;
        box-sizing: border-box;
        position: relative;
      }

      .app-shell::before {
        content: "";
        position: fixed;
        inset: 0;
        pointer-events: none;
        background-image:
          radial-gradient(circle at 10% 20%, rgba(248,250,252,0.12) 0, transparent 40%),
          radial-gradient(circle at 80% 80%, rgba(226,232,240,0.08) 0, transparent 45%);
        opacity: 0.25;
        mix-blend-mode: screen;
        z-index: -2;
      }

      /* ---------- NAV ---------- */
      .top-nav {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 4px;
        padding: 6px 14px;
        border-radius: 999px;
        background: radial-gradient(circle at top left,
                    rgba(15,23,42,0.96),
                    rgba(15,23,42,0.82));
        border: 1px solid rgba(148,163,184,0.35);
        box-shadow:
          0 16px 40px rgba(0,0,0,0.85),
          0 0 0 0.5px rgba(148,163,184,0.45);
        backdrop-filter: blur(22px);
        position: relative;
        overflow: hidden;
      }
      .top-nav::after {
        content: "";
        position: absolute;
        inset: -40%;
        background: linear-gradient(135deg,
                    rgba(56,189,248,0.14),
                    rgba(139,92,246,0.18),
                    rgba(56,189,248,0.14));
        opacity: 0;
        mix-blend-mode: screen;
        animation: navSheen 11s ease-in-out infinite;
        pointer-events: none;
      }
      @keyframes navSheen {
        0%, 70%   { opacity: 0; transform: translateX(-30%); }
        80%       { opacity: 0.7; transform: translateX(0); }
        100%      { opacity: 0; transform: translateX(30%); }
      }

      .brand-title {
        font-weight: 600;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        font-size: 11px;
        color: #e5e7eb;
      }
      .nav-buttons {
        display: flex;
        gap: 10px;
      }

      .nav-pill {
        position: relative;
        border-radius: 999px !important;
        border: 1px solid rgba(148,163,184,0.55) !important;
        padding: 6px 16px !important;
        font-size: 11px !important;
        color: #e5e7eb !important;
        background: radial-gradient(circle at top left,
                  rgba(15,23,42,0.96),
                  rgba(15,23,42,0.78)) !important;
        backdrop-filter: blur(18px);
        box-shadow:
          0 10px 26px rgba(0,0,0,0.75),
          0 0 0 0.3px rgba(148,163,184,0.7);
        overflow: hidden;
        transition: transform 0.18s ease, box-shadow 0.18s ease,
                    border-color 0.18s ease, background 0.18s ease;
        cursor: pointer;
      }
      .nav-pill::before,
      .nav-pill::after {
        content: "";
        position: absolute;
        inset: 0;
        border-radius: inherit;
        pointer-events: none;
      }
      .nav-pill::before {
        inset: -60%;
        background: radial-gradient(circle at 0 0,
                    rgba(56,189,248,0.55),
                    transparent 55%);
        opacity: 0;
        mix-blend-mode: screen;
        transition: opacity 0.25s ease, transform 0.25s ease;
      }
      .nav-pill::after {
        border: 1px solid rgba(56,189,248,0.0);
        box-shadow: 0 0 0 0 rgba(56,189,248,0.6);
        transition: box-shadow 0.28s ease, border-color 0.28s ease;
      }
      .nav-pill:hover {
        transform: translateY(-1px) scale(1.02);
        box-shadow:
          0 18px 40px rgba(0,0,0,0.9),
          0 0 0 0.7px rgba(56,189,248,0.85);
        border-color: rgba(56,189,248,0.75) !important;
        background: radial-gradient(circle at top left,
                  rgba(15,23,42,1),
                  rgba(15,23,42,0.9)) !important;
      }
      .nav-pill:hover::before {
        opacity: 0.7;
        transform: translateX(18%);
      }
      .nav-pill:hover::after {
        border-color: rgba(56,189,248,0.9);
        box-shadow: 0 0 22px 1px rgba(56,189,248,0.7);
      }

      /* ---------- GLASS CARDS ---------- */
      .glass-card {
        border-radius: 22px;
        padding: 14px 16px;
        background: radial-gradient(circle at top left,
                  rgba(15,23,42,0.97),
                  rgba(15,23,42,0.86));
        border: 1px solid rgba(148,163,184,0.32);
        box-shadow:
          0 18px 40px rgba(0,0,0,0.85),
          0 0 0 0.4px rgba(148,163,184,0.55);
        backdrop-filter: blur(22px);
        transition: transform 0.25s ease, box-shadow 0.25s ease,
                    border-color 0.25s ease, background 0.25s ease;
        position: relative;
        overflow: hidden;
      }
      .glass-card::before,
      .glass-card::after {
        content: "";
        position: absolute;
        inset: 0;
        border-radius: inherit;
        pointer-events: none;
      }
      .glass-card::before {
        inset: -40%;
        background:
          radial-gradient(circle at 0 0,
            rgba(56,189,248,0.28), transparent 55%),
          radial-gradient(circle at 100% 0,
            rgba(139,92,246,0.24), transparent 55%);
        opacity: 0;
        mix-blend-mode: screen;
        transition: opacity 0.4s ease;
      }
      .glass-card::after {
        inset: 1px;
        border-radius: inherit;
        border: 1px solid rgba(255,255,255,0.02);
        box-shadow: inset 0 0 18px rgba(15,23,42,0.9);
      }
      .glass-card:hover {
        transform: translateY(-3px);
        box-shadow:
          0 26px 60px rgba(0,0,0,0.96),
          0 0 0 0.6px rgba(56,189,248,0.8);
        border-color: rgba(56,189,248,0.75);
      }
      .glass-card:hover::before {
        opacity: 0.9;
      }

      .glass-slim {
        padding: 8px 10px;
        border-radius: 18px;
        border-color: rgba(148,163,184,0.18);
        box-shadow: 0 12px 26px rgba(0,0,0,0.78);
      }

      .section-title {
        font-size: 10px;
        text-transform: uppercase;
        letter-spacing: 0.16em;
        color: #9ca3af;
        margin-bottom: 4px;
      }

      .hero-title {
        font-size: 32px;
        font-weight: 600;
        letter-spacing: 0.06em;
        position: relative;
        z-index: 1;
      }
      .hero-title::after {
        content: "";
        position: absolute;
        inset: -40%;
        background: linear-gradient(135deg,
                    rgba(56,189,248,0.16),
                    rgba(129,140,248,0.2),
                    rgba(56,189,248,0.16));
        mix-blend-mode: screen;
        opacity: 0;
        pointer-events: none;
        animation: titleBeam 18s linear infinite;
      }
      @keyframes titleBeam {
        0%,70% { opacity: 0; transform: translateX(-25%); }
        80%    { opacity: 0.8; transform: translateX(0%); }
        100%   { opacity: 0; transform: translateX(25%); }
      }

      .hero-highlight {
        background: linear-gradient(120deg,#22c55e,#22d3ee,#a855f7);
        -webkit-background-clip: text;
        color: transparent;
      }
      .hero-sub {
        color: #9ca3af;
        margin-top: 6px;
        max-width: 460px;
        font-size: 12px;
      }
      .hero-cta {
        margin-top: 16px;
        display: flex;
        gap: 10px;
        align-items: center;
      }

      .hero-btn,
      .hero-ghost,
      .holo-btn {
        position: relative;
        border-radius: 999px;
        border: 1px solid rgba(148,163,184,0.7);
        padding: 8px 18px;
        font-size: 12px;
        font-weight: 500;
        cursor: pointer;
        color: #f9fafb;
        background: radial-gradient(circle at top left,
                    rgba(15,23,42,0.96),
                    rgba(15,23,42,0.82));
        box-shadow:
          0 16px 38px rgba(0,0,0,0.9),
          0 0 0 0.6px rgba(148,163,184,0.7);
        backdrop-filter: blur(18px);
        transition: transform 0.22s ease, box-shadow 0.22s ease,
                    border-color 0.22s ease, background 0.22s ease;
        overflow: hidden;
      }
      .hero-btn {
        background: linear-gradient(120deg,#22d3ee,#38bdf8);
        color: #020617;
        box-shadow:
          0 20px 46px rgba(8,47,73,0.95),
          0 0 0 0.7px rgba(56,189,248,0.85);
        border-color: rgba(56,189,248,0.85);
      }
      .hero-btn::before,
      .hero-ghost::before,
      .holo-btn::before {
        content: "";
        position: absolute;
        inset: -40%;
        background: linear-gradient(135deg,
                    rgba(56,189,248,0.4),
                    rgba(139,92,246,0.4),
                    rgba(56,189,248,0.4));
        opacity: 0;
        mix-blend-mode: screen;
        transition: opacity 0.3s ease, transform 0.3s ease;
        pointer-events: none;
      }
      .hero-btn:hover,
      .hero-ghost:hover,
      .holo-btn:hover {
        transform: translateY(-2px) scale(1.03);
        box-shadow:
          0 28px 64px rgba(0,0,0,0.98),
          0 0 0 0.9px rgba(56,189,248,0.9);
        border-color: rgba(56,189,248,0.9);
      }
      .hero-btn:hover::before,
      .hero-ghost:hover::before,
      .holo-btn:hover::before {
        opacity: 0.9;
        transform: translateX(12%);
      }

      .hero-ghost {
        border-radius: 999px;
        border: 1px solid rgba(148,163,184,0.7);
        background: rgba(15,23,42,0.7);
        color: #e5e7eb;
      }

      .home-shell {
        min-height: calc(100vh - 90px);
        display: flex;
        align-items: center;
        position: relative;
        overflow: hidden;
      }

      .home-shell::before {
        content: "";
        position: absolute;
        inset: 8% 18% 8% 34%;
        background-image:
          linear-gradient(rgba(15,23,42,0.0) 1px, transparent 1px),
          linear-gradient(90deg, rgba(15,23,42,0.0) 1px, transparent 1px);
        background-size: 28px 28px;
        opacity: 0.22;
        mix-blend-mode: screen;
        transform: perspective(900px) rotateX(60deg) translateY(18%);
        filter: blur(0.3px);
        animation: gridDrift 30s linear infinite;
        pointer-events: none;
      }
      @keyframes gridDrift {
        from { transform: perspective(900px) rotateX(60deg) translate3d(18px,18%,0); }
        to   { transform: perspective(900px) rotateX(60deg) translate3d(-18px,18%,0); }
      }

      /* ---- HERO ORB ---- */
      .hero-orb {
        width: 280px;
        height: 280px;
        border-radius: 999px;
        position: relative;
        display: flex;
        align-items: center;
        justify-content: center;
        animation: floatOrb 10s ease-in-out infinite alternate;
      }
      .hero-orb-ring {
        position: relative;
        width: 240px;
        height: 240px;
        border-radius: 999px;
        background: conic-gradient(
          from 220deg,
          #22c55e,
          #22d3ee,
          #38bdf8,
          #a855f7,
          #22c55e
        );
        animation: spinRing 14s linear infinite;
        box-shadow:
          0 0 40px rgba(34,211,238,0.8),
          0 0 70px rgba(56,189,248,0.7);
        mask: radial-gradient(farthest-side,
              transparent 60%, black 62%);
      }
      .hero-orb-core {
        position: absolute;
        width: 190px;
        height: 190px;
        border-radius: 999px;
        background: radial-gradient(circle at 30% 10%,
                  rgba(15,23,42,0.7),
                  rgba(15,23,42,0.98));
        box-shadow: inset 0 0 26px rgba(15,23,42,0.95);
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        padding: 22px;
      }

      #wheel_text {
        font-size: 11px;
        letter-spacing: 0.18em;
        text-transform: uppercase;
        color: #e5e7eb;
        opacity: 0.85;
        line-height: 1.5;
        transform: translateY(4px);
        max-width: 160px;
        margin: 0 auto;
      }
      .wheel-text-transition {
        transition: opacity 0.25s ease, transform 0.25s ease;
      }
      .wheel-text-out {
        opacity: 0;
        transform: translateY(-8px);
      }
      .wheel-text-in {
        opacity: 1;
        transform: translateY(4px);
      }

      @keyframes floatOrb {
        from { transform: translateY(0px) translateX(0px); }
        to   { transform: translateY(-12px) translateX(6px); }
      }
      @keyframes spinRing {
        from { transform: rotate(0deg); }
        to   { transform: rotate(360deg); }
      }

      .fade-panel {
        animation: fadeSlideIn 0.45s ease-out;
      }
      @keyframes fadeSlideIn {
        from { opacity: 0; transform: translateY(10px); }
        to   { opacity: 1; transform: translateY(0px); }
      }

      /* ---------- COMPARE LAYOUT ---------- */
      .compare-shell {
        max-width: 1180px;
        margin: 16px auto 12px auto;
        height: calc(100vh - 110px);
        position: relative;
      }
      .compare-shell::before {
        content: "";
        position: absolute;
        inset: 10% 15%;
        background: radial-gradient(circle at top,
                    rgba(56,189,248,0.22),
                    transparent 60%);
        z-index: -1;
        filter: blur(40px);
        opacity: 0.85;
        animation: compareGlow 18s ease-in-out infinite alternate;
        pointer-events: none;
      }
      @keyframes compareGlow {
        from { transform: translateY(0) translateX(0); }
        to   { transform: translateY(-12px) translateX(10px); }
      }

      .compare-grid {
        display: grid;
        grid-template-columns: minmax(0, 1.1fr) minmax(0, 1.4fr) 320px;
        grid-template-rows: min-content 1fr;
        grid-template-areas:
          "pref   filter cars"
          "best   filter cars";
        grid-column-gap: 18px;
        grid-row-gap: 14px;
        align-items: start;
        height: 100%;
      }
      .compare-pref    { grid-area: pref; }
      .compare-filters { grid-area: filter; }
      .compare-mid     { grid-area: best; margin-top: 4px; }
      .compare-right   { grid-area: cars; }

      /* FIX: allow selectize dropdowns to escape the card + stack above lower card */
      .compare-pref {
        overflow: visible !important;
        position: relative;
        z-index: 6;
      }
      .compare-filters {
        overflow: visible !important;
        position: relative;
        z-index: 5;
      }
      .selectize-dropdown {
        z-index: 9999 !important;
      }

      .compare-mid {
        position: relative;
        overflow: hidden;
        display: flex;
        align-items: stretch;
        min-height: 0;
      }
      .compare-mid-inner {
        position: relative;
        width: 100%;
      }

      .compare-title {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.14em;
        color: #9ca3af;
        margin-bottom: 4px;
      }
      .best-name {
        font-size: 22px;
        font-weight: 600;
        letter-spacing: 0.04em;
        margin-bottom: 4px;
      }
      .best-specs {
        margin-top: 6px;
        font-size: 12px;
      }

      .compare-right {
        max-height: 100%;
        overflow-y: auto;
        padding-top: 8px;
        padding-bottom: 8px;
        scrollbar-width: none;
        -ms-overflow-style: none;
        background: radial-gradient(circle at top,
                    rgba(15,23,42,0.98),
                    rgba(15,23,42,0.86));
        border-radius: 22px;
      }
      .compare-right::-webkit-scrollbar {
        width: 0;
        height: 0;
      }

      .compare-pref .form-group,
      .compare-filters .form-group {
        margin-bottom: 10px;
      }

      @media (max-width: 1100px) {
        .compare-grid {
          grid-template-columns: minmax(0, 1fr);
          grid-template-rows: auto auto auto auto;
          grid-template-areas:
            "pref"
            "best"
            "filter"
            "cars";
        }
        .compare-right {
          max-height: 320px;
        }
      }

      /* ---------- SELECTS ---------- */
      .selectize-input {
        background: rgba(15,23,42,0.78) !important;
        border-radius: 16px !important;
        border: 1px solid rgba(148,163,184,0.6) !important;
        color: #e5e7eb !important;
        box-shadow: 0 8px 20px rgba(0,0,0,0.65);
        padding-top: 6px !important;
        padding-bottom: 6px !important;
        backdrop-filter: blur(14px);
        transition: all 0.18s ease-out;
        min-height: 34px !important;
      }
      .selectize-input > input { color: #e5e7eb !important; }
      .selectize-input.input-active,
      .selectize-input.focus {
        border-color: rgba(56,189,248,0.95) !important;
        box-shadow:
          0 0 0 1px rgba(56,189,248,0.8),
          0 16px 36px rgba(8,47,73,0.9);
        transform: translateY(-1px);
      }
      .selectize-dropdown {
        background: rgba(15,23,42,0.92) !important;
        border-radius: 18px !important;
        border: 1px solid rgba(51,65,85,0.9) !important;
        box-shadow: 0 20px 45px rgba(0,0,0,0.9);
        margin-top: 6px;
        backdrop-filter: blur(18px);
        animation: dropdownFade 0.18s ease-out;
      }
      .selectize-dropdown-content .option {
        padding: 7px 11px;
        color: #e5e7eb;
        font-size: 12px;
      }
      .selectize-dropdown-content .option.active {
        background: linear-gradient(120deg,#22d3ee,#38bdf8);
        color: #020617;
      }
      @keyframes dropdownFade {
        from { opacity: 0; transform: translateY(-4px) scale(0.98); }
        to   { opacity: 1; transform: translateY(0)    scale(1);    }
      }

      /* ---------- CAR CARDS ---------- */
      .car-card {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 7px 12px;
        margin-bottom: 8px;
        border-radius: 18px;
        background: radial-gradient(circle at top left,
                  rgba(15,23,42,0.98),
                  rgba(15,23,42,0.9));
        box-shadow: 0 10px 22px rgba(0,0,0,0.85);
        border: 1px solid rgba(30,64,175,0.7);
        transition: transform 0.18s ease, box-shadow 0.18s ease, border-color 0.18s ease;
        cursor: pointer;
      }
      .car-card:hover {
        transform: translateY(-2px) scale(1.01);
        box-shadow: 0 18px 34px rgba(0,0,0,0.95);
        border-color: rgba(56,189,248,0.9);
      }
      .car-left {
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .car-avatar {
        position: relative;
        width: 50px;
        height: 24px;
        border-radius: 14px;
        background: linear-gradient(135deg,#22d3ee,#38bdf8);
        box-shadow:
          0 7px 14px rgba(8,47,73,0.9),
          inset 0 0 8px rgba(15,23,42,0.8);
      }
      .car-avatar::before {
        content: "";
        position: absolute;
        left: 7px;
        top: -8px;
        width: 22px;
        height: 13px;
        border-radius: 9px 9px 4px 4px;
        background: linear-gradient(135deg,#1f2937,#020617);
        box-shadow: 0 4px 8px rgba(0,0,0,0.9);
      }
      .car-avatar::after {
        content: "";
        position: absolute;
        left: 4px;
        right: 4px;
        bottom: -7px;
        height: 12px;
        background:
          radial-gradient(circle at 20% 50%,#020617 0,#020617 60%,transparent 61%),
          radial-gradient(circle at 80% 50%,#020617 0,#020617 60%,transparent 61%);
      }
      .car-avatar.cyl4 { background: linear-gradient(135deg,#22c55e,#16a34a); }
      .car-avatar.cyl6 { background: linear-gradient(135deg,#22d3ee,#38bdf8); }
      .car-avatar.cyl8 { background: linear-gradient(135deg,#f97316,#ef4444); }
      .car-avatar.brand-ferrari { background: linear-gradient(135deg,#ef4444,#991b1b); }
      .car-avatar.brand-ford    { background: linear-gradient(135deg,#1d4ed8,#93c5fd); }
      .car-avatar.brand-toyota  { background: linear-gradient(135deg,#f9fafb,#9ca3af); }
      .car-avatar.brand-mazda   { background: linear-gradient(135deg,#06b6d4,#0e7490); }
      .car-avatar.brand-merc    { background: linear-gradient(135deg,#e5e7eb,#6b7280); }
      .car-avatar.brand-datsun  { background: linear-gradient(135deg,#facc15,#eab308); }
      .car-avatar.brand-hornet  { background: linear-gradient(135deg,#22c55e,#a3e635); }

      .car-name { font-weight: 500; font-size: 13px; }
      .car-sub  { font-size: 10px; color: #9ca3af; }
      .car-specs {
        font-size: 10px;
        color: #9ca3af;
        display: flex;
        flex-wrap: wrap;
        gap: 4px 8px;
        max-width: 320px;
        justify-content: flex-end;
      }
      .car-spec-pill {
        padding: 2px 7px;
        border-radius: 999px;
        background: rgba(15,23,42,0.9);
        border: 1px solid rgba(55,65,81,0.9);
      }
      .fit-pill {
        padding: 3px 9px;
        border-radius: 999px;
        background: linear-gradient(120deg,#22d3ee,#38bdf8);
        color: #020617;
        font-size: 10px;
        font-weight: 600;
        box-shadow: 0 9px 18px rgba(8,47,73,0.9);
      }

      /* ---------- MODAL CAR ---------- */
      .zoom-car-modal {
        display: flex;
        gap: 22px;
        align-items: center;
      }
      .zoom-car-3d {
        position: relative;
        width: 170px;
        height: 80px;
        border-radius: 26px;
        background: linear-gradient(135deg,#22d3ee,#38bdf8);
        box-shadow:
          0 14px 28px rgba(8,47,73,0.95),
          inset 0 0 14px rgba(15,23,42,0.9);
      }
      .zoom-car-3d::before {
        content: "";
        position: absolute;
        left: 22px;
        top: -20px;
        width: 70px;
        height: 38px;
        border-radius: 22px 22px 10px 10px;
        background: linear-gradient(135deg,#111827,#020617);
        box-shadow: 0 10px 18px rgba(0,0,0,0.95);
      }
      .zoom-car-3d::after {
        content: "";
        position: absolute;
        left: 16px;
        right: 16px;
        bottom: -14px;
        height: 22px;
        background:
          radial-gradient(circle at 18% 50%,#020617 0,#020617 65%,transparent 66%),
          radial-gradient(circle at 82% 50%,#020617 0,#020617 65%,transparent 66%);
      }
      .zoom-car-3d.cyl4 { background: linear-gradient(135deg,#22c55e,#16a34a); }
      .zoom-car-3d.cyl6 { background: linear-gradient(135deg,#22d3ee,#38bdf8); }
      .zoom-car-3d.cyl8 { background: linear-gradient(135deg,#f97316,#ef4444); }
      .zoom-car-3d.brand-ferrari { background: linear-gradient(135deg,#ef4444,#991b1b); }
      .zoom-car-3d.brand-ford    { background: linear-gradient(135deg,#1d4ed8,#93c5fd); }
      .zoom-car-3d.brand-toyota  { background: linear-gradient(135deg,#f9fafb,#9ca3af); }
      .zoom-car-3d.brand-mazda   { background: linear-gradient(135deg,#06b6d4,#0e7490); }
      .zoom-car-3d.brand-merc    { background: linear-gradient(135deg,#e5e7eb,#6b7280); }
      .zoom-car-3d.brand-datsun  { background: linear-gradient(135deg,#facc15,#eab308); }
      .zoom-car-3d.brand-hornet  { background: linear-gradient(135deg,#22c55e,#a3e635); }

      /* ---------- INSIGHTS ---------- */
      .glass-card table {
        background-color: transparent !important;
        color: #e5e7eb !important;
        font-size: 11px;
      }
      .glass-card th,
      .glass-card td {
        background-color: transparent !important;
        color: #e5e7eb !important;
        border-color: rgba(55,65,81,0.9) !important;
      }

      .insights-shell {
        max-width: 1180px;
        margin: 16px auto;
        height: calc(100vh - 110px);
        display: flex;
        flex-direction: column;
        gap: 10px;
      }
      .insights-top-row {
        flex: 2 1 0;
        display: flex;
        gap: 10px;
        min-height: 0;
      }
      .insights-config {
        flex: 0 0 320px;
      }
      .insights-plot {
        flex: 1 1 auto;
        display: flex;
        flex-direction: column;
        min-height: 0;
      }
      .insights-plot .shiny-plot-output {
        height: 100% !important;
      }
      .insights-bottom-row {
        flex: 1 1 0;
        display: flex;
        gap: 10px;
        min-height: 0;
      }
      .insights-variance,
      .insights-loadings,
      .insights-clusters {
        flex: 1 1 0;
        overflow: hidden;
      }
      .insights-variance table,
      .insights-loadings table,
      .insights-clusters table {
        font-size: 10px;
      }
      .insights-raw-btn-wrap {
        margin-top: 8px;
        display: flex;
        justify-content: flex-end;
      }

      .modal-content {
        border-radius: 24px;
        border: 1px solid rgba(148,163,184,0.35);
        background: radial-gradient(circle at top left,
                    rgba(15,23,42,0.98),
                    rgba(15,23,42,0.9));
        box-shadow:
          0 26px 64px rgba(0,0,0,0.98),
          0 0 0 0.6px rgba(56,189,248,0.7);
        backdrop-filter: blur(24px);
      }
      .modal-header,
      .modal-footer { border: none; }
      .modal-body { max-height: 60vh; overflow-y: auto; }

      .parallax-layer {
        will-change: transform;
        transition: transform 0.35s ease-out;
      }

      @media (max-width: 900px) {
        .app-shell { padding: 10px 12px 24px 12px; }
        .hero-title { font-size: 26px; }
        .home-shell { min-height: auto; }
      }
    ')),
    
    # Parallax JS (pure DOM, safe)
    tags$script(HTML("
      document.addEventListener('mousemove', function(e){
        var x = (e.clientX / window.innerWidth - 0.5);
        var y = (e.clientY / window.innerHeight - 0.5);
        document.querySelectorAll('.parallax-layer').forEach(function(el){
          var depth = parseFloat(el.getAttribute('data-depth') || '0.25');
          var tx = -x * depth * 24;
          var ty = -y * depth * 18;
          el.style.transform = 'translate3d(' + tx + 'px,' + ty + 'px,0)';
        });
      });
    ")),
    
    # Wheel text script (no jQuery / Shiny dependency)
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var el = document.getElementById('wheel_text');
        if (!el) return;
        var phrases = [
          'Smart car ranking engine',
          'Performance map insights',
          'Custom fit scoring',
          'Clustered driving styles',
          'Raw data explorer'
        ];
        var idx = 0;
        el.textContent = phrases[idx];
        el.classList.add('wheel-text-transition','wheel-text-in');
        setInterval(function(){
          idx = (idx + 1) % phrases.length;
          el.classList.remove('wheel-text-in');
          el.classList.add('wheel-text-out');
          setTimeout(function(){
            el.textContent = phrases[idx];
            el.classList.remove('wheel-text-out');
            el.classList.add('wheel-text-in');
          }, 260);
        }, 3000);
      });
    "))
  ),
  
  div(
    class = "app-shell",
    
    # Top nav
    div(
      class = "top-nav parallax-layer",
      `data-depth` = "0.12",
      div(span("CARLAB", class = "brand-title")),
      div(
        class = "nav-buttons",
        actionButton("nav_home",    "Home",    class = "nav-pill"),
        actionButton("nav_compare", "Compare", class = "nav-pill"),
        actionButton("nav_insight", "Insights", class = "nav-pill")
      )
    ),
    
    tabsetPanel(
      id       = "main_tabs",
      type     = "hidden",
      selected = "home",
      
      # ---------- HOME ----------
      tabPanel(
        title = "Home", value = "home",
        div(
          class = "fade-panel home-shell",
          fluidRow(
            column(
              7,
              div(
                class = "glass-card parallax-layer",
                `data-depth` = "0.26",
                span("Performance. Style. Trade-offs.", class = "section-title"),
                div(
                  class = "hero-title",
                  span("Design your ", class = "hero-highlight"),
                  "next car decision."
                ),
                div(
                  class = "hero-sub",
                  "Move a few sliders, let the engine do the math. ",
                  "We blend power, efficiency, weight and acceleration ",
                  "into a single score so you can compare cars in seconds."
                ),
                div(
                  class = "hero-cta",
                  actionButton("btn_start",   "Start comparing",       class = "hero-btn"),
                  actionButton("btn_insight", "See multivariate view", class = "hero-ghost")
                )
              )
            ),
            column(
              5,
              div(
                class = "hero-orb parallax-layer",
                `data-depth` = "0.4",
                div(class = "hero-orb-ring"),
                div(
                  class = "hero-orb-core",
                  span(id = "wheel_text", "Smart car ranking engine")
                )
              )
            )
          )
        )
      ),
      
      # ---------- COMPARE ----------
      tabPanel(
        title = "Compare", value = "compare",
        div(
          class = "fade-panel",
          div(
            class = "compare-shell",
            div(
              class = "compare-grid",
              
              div(
                class = "glass-card compare-pref",
                span("Your preferences", class = "section-title"),
                selectInput("pref_mpg",  "Fuel economy priority",  pref_levels, "High"),
                selectInput("pref_hp",   "Engine power priority",  pref_levels, "Medium"),
                selectInput("pref_wt",   "Lightweight priority",   pref_levels, "Medium"),
                selectInput("pref_qsec", "Fast acceleration priority", pref_levels, "High")
              ),
              
              div(
                class = "glass-card compare-filters",
                span("Filters", class = "section-title"),
                selectInput(
                  "f_cyl", "Cylinders",
                  choices  = c("All", levels(mtcars_data$cylinders)),
                  selected = "All"
                ),
                selectInput(
                  "f_trans", "Transmission",
                  choices  = c("All", "Automatic", "Manual"),
                  selected = "All"
                ),
                sliderInput(
                  "f_mpg_min", "Minimum mpg",
                  min   = floor(min(mtcars_data$mpg)),
                  max   = ceiling(max(mtcars_data$mpg)),
                  value = 15, step = 1
                )
              ),
              
              div(
                class = "glass-card compare-mid",
                div(
                  class = "compare-mid-inner",
                  span("Best match for you", class = "compare-title"),
                  uiOutput("best_car_card")
                )
              ),
              
              div(
                class = "glass-card compare-right",
                span("Ranked results (tap a car)", class = "compare-title"),
                uiOutput("ranked_cards")
              )
            )
          )
        )
      ),
      
      # ---------- INSIGHTS ----------
      tabPanel(
        title = "Insights", value = "insight",
        div(
          class = "fade-panel insights-shell",
          div(
            class = "insights-top-row",
            div(
              class = "glass-card insights-config",
              span("PCA configuration", class = "section-title"),
              checkboxGroupInput(
                "pca_vars",
                "Variables in PCA",
                choices  = c("mpg","hp","wt","qsec","drat"),
                selected = c("mpg","hp","wt","qsec","drat")
              ),
              sliderInput(
                "pca_k",
                "Clusters (k-means on PC1–PC2)",
                min = 2, max = 6, value = 3, step = 1
              ),
              checkboxInput(
                "pca_labels",
                "Show car names",
                value = FALSE
              )
            ),
            div(
              class = "glass-card insights-plot",
              span("Performance map (tap a car to zoom)", class = "section-title"),
              plotOutput("pca_plot", height = "320px", click = "pca_click")
            )
          ),
          div(
            class = "insights-bottom-row",
            div(
              class = "glass-card glass-slim insights-variance",
              span("Explained variance profile", class = "section-title"),
              tableOutput("pca_variance")
            ),
            div(
              class = "glass-card glass-slim insights-loadings",
              span("Feature influence by component", class = "section-title"),
              tableOutput("pca_loadings")
            ),
            div(
              class = "glass-card glass-slim insights-clusters",
              span("Cluster profiles", class = "section-title"),
              tableOutput("segment_table"),
              div(
                class = "insights-raw-btn-wrap",
                actionButton("btn_raw_data", "View raw data", class = "holo-btn")
              )
            )
          )
        )
      )
    )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  # nav
  observeEvent(input$nav_home,    { updateTabsetPanel(session, "main_tabs", selected = "home")    })
  observeEvent(input$nav_compare, { updateTabsetPanel(session, "main_tabs", selected = "compare") })
  observeEvent(input$nav_insight, { updateTabsetPanel(session, "main_tabs", selected = "insight") })
  
  # hero buttons
  observeEvent(input$btn_start,   { updateTabsetPanel(session, "main_tabs", selected = "compare") })
  observeEvent(input$btn_insight, { updateTabsetPanel(session, "main_tabs", selected = "insight") })
  
  # zoom modal helper
  show_car_modal <- function(car_row) {
    if (nrow(car_row) == 0) return(NULL)
    car_row <- car_row[1, ]
    cyl_class   <- paste0("cyl", as.character(car_row$cylinders))
    brand_class <- brand_class_for(car_row$car)
    fit_txt     <- if ("FitScore" %in% names(car_row) && !is.na(car_row$FitScore[1]))
      sprintf("%.3f", car_row$FitScore[1]) else "N/A"
    
    showModal(
      modalDialog(
        easyClose = TRUE,
        footer    = modalButton("Close"),
        title     = car_row$car,
        size      = "l",
        div(
          class = "zoom-car-modal",
          div(class = paste("zoom-car-3d", cyl_class, brand_class)),
          div(
            tags$p(tags$b("Fit score: "), fit_txt),
            tags$ul(
              tags$li(paste("mpg:", round(car_row$mpg,1))),
              tags$li(paste("hp:", round(car_row$hp,0))),
              tags$li(paste("weight (1000 lbs):", round(car_row$wt,2))),
              tags$li(paste("0–60 style time (qsec):", round(car_row$qsec,2))),
              tags$li(paste("cylinders:", as.character(car_row$cylinders))),
              tags$li(paste("transmission:", car_row$transmission))
            )
          )
        )
      )
    )
  }
  
  # scoring
  scored_cars <- reactive({
    df <- mtcars_data
    if (!is.null(input$f_cyl)   && input$f_cyl   != "All") df <- df %>% filter(cylinders == input$f_cyl)
    if (!is.null(input$f_trans) && input$f_trans != "All") df <- df %>% filter(transmission == input$f_trans)
    df <- df %>% filter(mpg >= input$f_mpg_min)
    if (nrow(df) == 0) return(df %>% mutate(FitScore = numeric(0)))
    
    z_mpg  <- scale(df$mpg)
    z_hp   <- scale(df$hp)
    z_wt   <- scale(-df$wt)
    z_qsec <- scale(-df$qsec)
    
    w <- c(
      mpg  = pref_to_weight(input$pref_mpg),
      hp   = pref_to_weight(input$pref_hp),
      wt   = pref_to_weight(input$pref_wt),
      qsec = pref_to_weight(input$pref_qsec)
    )
    w <- w / sum(w)
    
    df$FitScore <- w["mpg"]  * as.numeric(z_mpg) +
      w["hp"]   * as.numeric(z_hp)  +
      w["wt"]   * as.numeric(z_wt)  +
      w["qsec"] * as.numeric(z_qsec)
    df %>% arrange(desc(FitScore))
  })
  
  output$best_car_card <- renderUI({
    df <- scored_cars()
    if (nrow(df) == 0) {
      return(div(
        h4("No cars found"),
        tags$small("Try relaxing filters or lowering minimum mpg.")
      ))
    }
    best <- df[1, ]
    div(
      class = "best-card",
      div(class = "best-name", best$car),
      tags$ul(
        class = "best-specs",
        tags$li(HTML(paste0("<b>mpg:</b> ", round(best$mpg, 1)))),
        tags$li(HTML(paste0("<b>hp:</b> ", round(best$hp, 0)))),
        tags$li(HTML(paste0("<b>weight:</b> ", round(best$wt, 2)))),
        tags$li(HTML(paste0("<b>0–60 time (qsec):</b> ", round(best$qsec, 2)))),
        tags$li(HTML(paste0("<b>cyl:</b> ", as.character(best$cylinders)))),
        tags$li(HTML(paste0("<b>trans:</b> ", best$transmission)))
      )
    )
  })
  
  output$ranked_cards <- renderUI({
    df <- scored_cars()
    if (nrow(df) == 0) return(div(tags$small("No cars match these settings.")))
    df <- df %>% mutate(FitScore = round(FitScore, 3))
    df <- head(df, 10)
    
    cards <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      cyl_class   <- paste0("cyl", as.character(r$cylinders))
      brand_class <- brand_class_for(r$car)
      div(
        class   = "car-card",
        onclick = sprintf("Shiny.setInputValue('zoom_car', '%s', {priority: 'event'})", r$car),
        div(
          class = "car-left",
          div(class = paste("car-avatar", cyl_class, brand_class)),
          div(
            div(class = "car-name", r$car),
            div(class = "car-sub",
                paste0(r$cylinders, " cyl \u00b7 ", r$transmission))
          )
        ),
        div(
          class = "car-specs",
          span(class = "car-spec-pill", paste0("mpg ", round(r$mpg, 1))),
          span(class = "car-spec-pill", paste0("hp ", round(r$hp, 0))),
          span(class = "car-spec-pill", paste0("wt ", round(r$wt, 2))),
          span(class = "car-spec-pill", paste0("0–60s ", round(r$qsec, 1))),
          span(class = "fit-pill",      paste0("Fit ", sprintf("%.2f", r$FitScore)))
        )
      )
    })
    do.call(tagList, cards)
  })
  
  observeEvent(input$zoom_car, {
    df <- scored_cars()
    car_row <- df[df$car == input$zoom_car, ]
    show_car_modal(car_row)
  })
  
  # PCA
  pca_res <- reactive({
    req(input$pca_vars)
    x <- mtcars_data[, input$pca_vars, drop = FALSE]
    prcomp(x, scale. = TRUE)
  })
  
  pca_scores <- reactive({
    sc <- as.data.frame(pca_res()$x)
    sc$car <- mtcars_data$car
    sc$cyl <- factor(mtcars_data$cyl)
    sc
  })
  
  pca_clusters <- reactive({
    sc <- pca_scores()
    set.seed(123)
    kmeans(sc[, c("PC1", "PC2")], centers = input$pca_k)
  })
  
  output$pca_plot <- renderPlot({
    sc <- pca_scores()
    cl <- pca_clusters()
    sc$cluster <- factor(cl$cluster)
    
    g <- ggplot(sc, aes(x = PC1, y = PC2)) +
      geom_point(aes(color = cluster), alpha = 0) +
      geom_text(aes(label = "\U0001F697", color = cluster), size = 6, show.legend = FALSE) +
      scale_color_brewer(palette = "Set2") +
      labs(
        x = "PC1 (performance / size)",
        y = "PC2 (efficiency / gearing)",
        color = "Cluster"
      ) +
      theme_minimal(base_family = "Space Grotesk") +
      theme(
        plot.background  = element_rect(fill = "#020617", color = NA),
        panel.background = element_rect(fill = "#020617", color = NA),
        text             = element_text(color = "white", size = 10),
        axis.text        = element_text(color = "white", size = 9),
        legend.background = element_rect(fill = "#020617", color = NA),
        legend.key       = element_rect(fill = "#020617", color = NA),
        panel.grid.major = element_line(color = "#374151"),
        panel.grid.minor = element_line(color = "#1f2933")
      )
    
    if (isTRUE(input$pca_labels)) {
      g <- g + geom_text(
        data  = sc,
        aes(x = PC1, y = PC2, label = car),
        inherit.aes = FALSE,
        vjust = 2.1,
        size  = 3,
        color = "white",
        show.legend = FALSE
      )
    }
    g
  })
  
  observeEvent(input$pca_click, {
    click <- input$pca_click
    if (is.null(click)) return()
    sc <- pca_scores()
    d <- (sc$PC1 - click$x)^2 + (sc$PC2 - click$y)^2
    idx <- which.min(d)
    if (!length(idx)) return()
    if (sqrt(min(d)) > 0.7) return()
    car_name <- sc$car[idx]
    car_row  <- mtcars_data[mtcars_data$car == car_name, ]
    show_car_modal(car_row)
  })
  
  output$pca_variance <- renderTable({
    sdev <- pca_res()$sdev
    eig  <- sdev^2
    prop <- eig / sum(eig)
    cum  <- cumsum(prop)
    data.frame(
      PC         = paste0("PC", seq_along(eig)),
      Eigenvalue = round(eig, 3),
      Proportion = round(prop, 3),
      Cumulative = round(cum, 3)
    )
  }, striped = TRUE, hover = TRUE)
  
  output$pca_loadings <- renderTable({
    round(pca_res()$rotation, 3)
  }, rownames = TRUE, striped = TRUE, hover = TRUE)
  
  output$segment_table <- renderTable({
    df <- mtcars_data
    cl <- pca_clusters()
    df$cluster <- factor(cl$cluster)
    df %>%
      group_by(cluster) %>%
      summarise(
        Avg_mpg = round(mean(mpg), 1),
        Avg_hp  = round(mean(hp), 0),
        Avg_wt  = round(mean(wt), 2),
        Avg_qsec= round(mean(qsec), 2),
        Cars    = n()
      )
  }, striped = TRUE, hover = TRUE)
  
  output$pca_data <- renderDT({
    datatable(
      mtcars_data %>% select(car, everything()),
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE)
    )
  })
  
  observeEvent(input$btn_raw_data, {
    showModal(
      modalDialog(
        title = "Raw data (mtcars)",
        size  = "l",
        easyClose = TRUE,
        DTOutput("pca_data"),
        footer = modalButton("Close")
      )
    )
  })
}

shinyApp(ui, server)
