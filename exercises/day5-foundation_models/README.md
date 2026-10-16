# Tourism foundation-model lab

Independent companion to session 10, **Time series foundation models**. The lecture is demonstration-led; this notebook is for the workshop exercise period or later study.

## Start with the saved outputs

Open `tourism_lab.ipynb` with this folder as the working directory. Leave all `RUN_*` switches set to `False` for the first pass. This path needs only numpy, pandas, matplotlib and Jupyter:

```sh
python -m pip install numpy pandas matplotlib notebook
jupyter notebook tourism_lab.ipynb
```

The notebook follows the slide demonstration sequence, including validation before fine-tuning, and includes the Chronos-2 architecture, a slide-to-lab roadmap, interpretation exercises and expandable answers. Saved forecasts are specific to Snowy Mountains holiday trips. When changing the series, generate fresh forecasts; the notebook checks the cached series identifier.

## Live runs

Use a separate Python 3.12 environment, install `requirements.txt`, and enable only the required notebook switches. Fine-tuning requires both `RUN_FINETUNING` and the chosen model switch (`RUN_CHRONOS` or `RUN_TIMEGPT`). Optional validation has its own `RUN_VALIDATION` switch. Restart the kernel and run from the top after changing these choices. Chronos-2 downloads `amazon/chronos-2` on its first online load. It can run on CPU; a GPU is optional.

For TimeGPT, configure `NIXTLA_API_KEY` in the environment or your local `.env`. The blank `.env.example` shows the variable name. The key is never included in distributed files. Requests send the public tourism history to Nixtla and consume API quota.

The saved run returned HTTP 429. There are no TimeGPT tourism outputs or claims of fine-tuning improvement. Failed requests are recorded in `results/timegpt_status.json`.

## Data and evaluation

`data/tourism.csv` is an unchanged copy of the workshop's `exercises/data/tourism.csv`, retained here so this lab folder is portable. It contains 24,320 observations across 304 quarterly series. The selected Snowy Mountains/Holiday series has 80 observations from 1998 Q1 to 2017 Q4, measured in thousands of overnight trips.

- Initial training: through 2013 Q4.
- Validation: three four-quarter origins covering 2014–2016.
- Final history and adaptation data: through 2016 Q4.
- Final test: 2017 Q1–Q4.
- Seasonal naive: repeat the latest four quarters.
- Chronos-2 point forecast: median; interval: 0.1–0.9 quantiles.
- MAE is the main point-error measure; RMSE is also reported on the same forecasts. Both use thousands of trips. RMSE emphasises large misses; it does not imply that the saved medians are means.
- LoRA: ten steps, context 64, minimum past 8, learning rate 0.00001, batch size 4, seed 42.

The settings were fixed before the final test, rather than selected on it. This single-series example does not establish a general model ranking. Public-data overlap with pretraining is unknown, and four test quarters cannot establish calibration.

## Reproduce saved results

From this folder, with model dependencies installed:

```sh
python scripts/tourism_forecasts.py prepare
python scripts/tourism_forecasts.py chronos
python scripts/tourism_forecasts.py timegpt --env-file /path/to/local/.env
python scripts/tourism_forecasts.py figures
```

The helper's Chronos run deliberately uses cached weights and offline mode. Download the checkpoint first using the notebook's `from_pretrained` call while online. Figures and metrics can be refreshed from saved CSVs without calling models. The helper records versions, the source-data hash and adaptation settings.

To update slide figures after a new run, copy the generated `figures/*.pdf` to `materials/figs/foundation_models/`, refresh its `results_table.md`, and render `materials/06_foundation_models.qmd`. If TimeGPT becomes available, replace its execution-status note with the generated tourism figure. Review the resulting PDF before presenting.

## Closing research comparison

The notebook ends with the same overall Lao multi-domain RMSSE-versus-runtime figure as the lecture. The larger FP comparison is optional, matching the slide appendix. These are the user's selected original WP3 figures, with the Lao sPIN panels removed. They are separate research results, not tourism model outputs. PNG display copies are in `figures/wp3/`. No domain-specific runtime is inferred.

Original PDFs, display-preparation code and provenance are in `materials/figs/foundation_models/wp3/` in the workshop repository. Reading this comparison needs no additional packages, credentials or model runs.

## References

- [FPP Python Chapter 15](https://otexts.com/fpppy/15-foundation-models.html): lecture section order.
- Marco Peixeiro, *Time Series Forecasting Using Foundation Models*, Chapters 1 and 2: explanatory flow from models and representations to pretraining and adaptation.
- [Tourism data documentation](https://tsibble.tidyverts.org/reference/tourism.html). Dates above reflect the actual workshop CSV snapshot.
- [Chronos](https://github.com/amazon-science/chronos-forecasting) and [Nixtla](https://www.nixtla.io/docs): model interfaces.
- Harsha Chamara Hewage, Bahman Rostami-Tabar, Aris Syntetos and Federico Liberatore. *Foundation models in low-resource settings: Evaluating AI-driven forecasting in global health supply chains*.
