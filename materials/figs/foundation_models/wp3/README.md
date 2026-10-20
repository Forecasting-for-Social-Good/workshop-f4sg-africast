# Selected WP3 figures

The user selected these exact manuscript figures on 14 October 2026:

- `fig-wp3_runtime_full-1.pdf`: overall Lao multi-domain accuracy versus runtime.
- `fig-wp3_runtime_rmsse-1.pdf`: the larger overall family-planning comparison.

Both source PDFs are preserved byte-for-byte. The source directory is `phd_project_wp3/writings/manuscript/00-main_files/figure-pdf/`. `source_manifest.json` records their SHA-256 hashes.

## Display copies

**Main lecture:** `lao_runtime_rmsse.pdf` retains the original top row of RMSSE panels, the original runtime label, and the model legend. The sPIN panels are removed. The marks, coordinates, colours, labels and axes are not redrawn. This is an aggregate comparison across Lao clinical domains, not separate domain runtimes.

**Appendix:** `fp_runtime_rmsse_display.pdf` preserves the original FP figure's content. Its page boundary is widened from 864 to 900 points to reveal rightmost legend text that was clipped in the supplied source.

Run `python prepare_display.py` with pypdf, pypdfium2 and Pillow to regenerate the PDF display copies and PNG previews. The notebook uses copies of the two PNGs in `exercises/day5-foundation_models/figures/wp3/`. The slides use `lao_runtime_rmsse.svg`, converted from the display PDF with `pdftocairo -svg lao_runtime_rmsse.pdf lao_runtime_rmsse.svg`.

## Interpretation

Runtime is the complete rolling-origin evaluation workload, including fitting, tuning or adaptation where applicable, with shared costs allocated as in the study. It is not latency for one forecast. TimeGPT is remotely hosted; its elapsed time is not local CPU compute. Source plot model selections and horizon-specific vertical scales are retained.

The Lao figure omits LightGBM and XGBoost variants, as in the source. Chronos-2-FT-Full uses pooled full-weight adaptation for 1,000 steps, not the tourism lab's ten-step LoRA example. The Lao assessment is not transfer to an unseen country: Lao family-planning data informed model selection.

## Reference

Harsha Chamara Hewage, Bahman Rostami-Tabar, Aris Syntetos and Federico Liberatore. *Foundation models in low-resource settings: Evaluating AI-driven forecasting in global health supply chains*.

Title and authors supplied by the user. No year, journal, DOI or publication status has been inferred.
