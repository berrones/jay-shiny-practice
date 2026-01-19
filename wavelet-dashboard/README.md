# Wavelet Dashboard (Shiny App)

An interactive, beginner-friendly **Shiny tutorial app** for learning the **Discrete Wavelet Transform (DWT)** using the **`wavelets`** R package.  
The app is intentionally minimal, visual, and exploratory—designed for first exposure to wavelets rather than exhaustive computation.

---

## What this app teaches

This app walks users through three simple ideas:

1. **What a wavelet is**  
   A wavelet is a small, localized wave used to describe patterns in data at different scales.

2. **Signal → DWT**  
   A synthetic signal is generated, noise is added, and a DWT is computed.

3. **Detail coefficients**  
   Users see how wavelets capture changes at different scales, and how this depends on the wavelet choice.

---

## Features

- Step-by-step tutorial navigation (Prev / Next)
- Interactive signal generation
- Two beginner-friendly wavelets only:
  - **Haar** (`haar`)
  - **Daubechies-4** (`d4`)
- Clean, colorful UI with large centered explanations
- Visual comparison of **fine vs coarse** detail coefficients
- No advanced denoising, thresholding, or MODWT (by design)

---

## Dependencies

Only two R packages are required:

```r
install.packages(c("shiny", "wavelets"))

How to run

Clone or download the repository, then from the app directory:

shiny::runApp()


or open app.R in RStudio and click Run App.

Target audience

Students encountering wavelets for the first time

Applied math, statistics, signal processing, or bioinformatics learners

Anyone who wants intuition before theory

No prior wavelet knowledge is assumed.


Design philosophy

This app prioritizes:

Visual intuition over formulas

Small parameter spaces over flexibility

Immediate feedback over completeness

Advanced topics (thresholding, MODWT, boundary effects, packet transforms) are intentionally excluded to keep the learning curve shallow.


License

MIT
