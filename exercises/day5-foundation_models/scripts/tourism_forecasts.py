"""Generate the tourism lab's cached forecasts and figures; no models run on import."""
from pathlib import Path
import argparse
import hashlib
import importlib.metadata as metadata
import json
import logging
import os
import time

ROOT = Path(__file__).resolve().parents[1]
os.environ.setdefault("MPLCONFIGDIR", str(ROOT / "tmp" / "matplotlib"))
os.environ.setdefault("HF_HUB_DISABLE_TELEMETRY", "1")
os.environ.setdefault("TOKENIZERS_PARALLELISM", "false")
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

RESULTS = ROOT / "results"
FIGS = ROOT / "figures"
for folder in [RESULTS, FIGS]:
    folder.mkdir(parents=True, exist_ok=True)
H = 4
COLORS = {"Seasonal naive": "#777777", "Chronos-2": "#0072B2",
          "Chronos-2 LoRA": "#D55E00", "TimeGPT": "#009E73",
          "TimeGPT adapted": "#CC79A7"}

def data():
    raw = pd.read_csv(ROOT / "data/tourism.csv", parse_dates=["Quarter"])
    series = raw.loc[(raw.Region == "Snowy Mountains") & (raw.Purpose == "Holiday")].copy()
    df = series.rename(columns={"Quarter": "ds", "Trips": "y"})
    df["unique_id"] = "Snowy Mountains / Holiday"
    df = df[["unique_id", "ds", "y"]].sort_values("ds").reset_index(drop=True)
    assert len(df) == 80 and not df.duplicated(["unique_id", "ds"]).any()
    assert df.y.notna().all()
    assert df.ds.equals(pd.Series(pd.date_range("1998-01-01", periods=80, freq="QS")))
    return df, df.iloc[:-16].copy(), df.iloc[:-H].copy(), df.iloc[-H:].copy()

def save(fig, name):
    fig.tight_layout()
    fig.savefig(FIGS / f"{name}.png", dpi=180, bbox_inches="tight")
    fig.savefig(FIGS / f"{name}.pdf", bbox_inches="tight")
    plt.close(fig)

def style(ax):
    ax.spines[["top", "right"]].set_visible(False)
    ax.grid(alpha=.18)
    ax.set(xlabel="Year", ylabel="Overnight trips (thousands)")
    ax.xaxis.set_major_locator(mdates.YearLocator(2))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))

def aligned(raw, point, lower, upper, label):
    df, _, _, _ = data()
    raw = raw.rename(columns={point:"prediction", lower:"lower80", upper:"upper80"})
    out = raw[["unique_id", "ds", "prediction", "lower80", "upper80"]].copy()
    out["ds"] = pd.to_datetime(out.ds)
    out = out.merge(df, on=["unique_id", "ds"], validate="one_to_one")
    assert len(out) == H
    out["model"] = label
    return out

def baseline(context, future):
    out = future.copy()
    out["prediction"] = context.y.tail(H).to_numpy()
    out["model"] = "Seasonal naive"
    return out

def prepare():
    df, train, history, test = data()
    df.to_csv(ROOT / "data/tourism_model_input.csv", index=False)
    baseline(history, test).to_csv(RESULTS / "seasonal_naive.csv", index=False)
    rows = []
    for end in range(len(train), len(history), H):
        fc = baseline(df.iloc[:end], df.iloc[end:end+H])
        fc["cutoff"] = df.ds.iloc[end-1]
        rows.append(fc)
    pd.concat(rows).to_csv(RESULTS / "seasonal_naive_validation.csv", index=False)
    info = {"raw_rows":24320, "raw_series":304, "series":"Snowy Mountains / Holiday",
            "frequency":"Quarterly; quarter-start timestamps", "unit":"thousand overnight trips",
            "rows":80, "start":"1998 Q1", "end":"2017 Q4", "horizon":H,
            "training":"1998 Q1 to 2013 Q4", "validation":"2014 Q1 to 2016 Q4",
            "test":"2017 Q1 to 2017 Q4", "adaptation_history":"1998 Q1 to 2016 Q4",
            "source":"exercises/data/tourism.csv in the workshop repository",
            "sha256":hashlib.sha256((ROOT / "data/tourism.csv").read_bytes()).hexdigest()}
    (RESULTS / "data_provenance.json").write_text(json.dumps(info, indent=2))
    print(json.dumps(info), flush=True)

def chronos():
    os.environ["HF_HUB_OFFLINE"] = "1"
    import torch
    from chronos import Chronos2Pipeline
    torch.manual_seed(42)
    torch.set_num_threads(4)
    df, train, history, test = data()
    pipe = Chronos2Pipeline.from_pretrained("amazon/chronos-2", device_map="cpu", local_files_only=True)
    def predict(model, context, label):
        raw = model.predict_df(context, prediction_length=H, quantile_levels=[.1,.5,.9],
            id_column="unique_id", timestamp_column="ds", target="y")
        return aligned(raw, "0.5", "0.1", "0.9", label)
    predict(pipe, history, "Chronos-2").to_csv(RESULTS / "chronos2.csv", index=False)
    rows=[]
    for end in range(len(train), len(history), H):
        fc=predict(pipe, df.iloc[:end], "Chronos-2")
        fc["cutoff"]=df.ds.iloc[end-1]
        rows.append(fc)
    pd.concat(rows).to_csv(RESULTS / "chronos2_validation.csv", index=False)
    start=time.perf_counter()
    # Settings fixed before looking at 2017. Both adaptation examples use pre-test history.
    adapted=pipe.fit(inputs=[history.y.to_numpy(dtype=np.float32)], prediction_length=H,
        finetune_mode="lora", context_length=64, learning_rate=1e-5, num_steps=10,
        batch_size=4, min_past=8, output_dir=ROOT / "tmp/chronos2_lora",
        report_to="none", dataloader_num_workers=0, disable_tqdm=True,
        optim="adamw_torch", save_safetensors=True, seed=42)
    predict(adapted, history, "Chronos-2 LoRA").to_csv(RESULTS / "chronos2_finetuned.csv", index=False)
    (RESULTS / "chronos2_settings.json").write_text(json.dumps({"checkpoint":"amazon/chronos-2",
        "finetune_mode":"lora", "steps":10, "context_length":64, "min_past":8,
        "learning_rate":1e-5, "batch_size":4, "seed":42, "device":"cpu",
        "adaptation_and_inference_seconds":time.perf_counter()-start}, indent=2))
    print("Chronos-2 zero-shot, validation and LoRA outputs saved.", flush=True)

def timegpt(env_file=None):
    from dotenv import load_dotenv
    from nixtla import NixtlaClient
    logging.getLogger("nixtla").setLevel(logging.CRITICAL)
    load_dotenv(env_file or ROOT / ".env")
    if not os.getenv("NIXTLA_API_KEY"):
        raise RuntimeError("Configure NIXTLA_API_KEY locally.")
    df, train, history, test = data()
    client=NixtlaClient(max_retries=1)
    try:
        for stem, label, steps in [("timegpt","TimeGPT",0),("timegpt_finetuned","TimeGPT adapted",10)]:
            options={"finetune_steps":steps}
            if steps: options.update(finetune_depth=1, finetune_loss="mae")
            raw=client.forecast(df=history,h=H,freq="QS",level=[80],**options)
            aligned(raw,"TimeGPT","TimeGPT-lo-80","TimeGPT-hi-80",label).to_csv(RESULTS/f"{stem}.csv",index=False)
        status={"status":"success", "finetune_steps":10, "finetune_depth":1,
                "finetune_loss":"mae", "history_end":"2016 Q4"}
    except Exception as exc:
        # Save a status, never credentials or the raw server response.
        status={"status":"unavailable", "error_type":type(exc).__name__,
                "http_429":"429" in str(exc), "note":"No generated output is claimed for failed calls."}
    (RESULTS/"timegpt_status.json").write_text(json.dumps(status,indent=2))
    print(json.dumps(status),flush=True)

def figures():
    plt.rcParams.update({"font.family":"DejaVu Sans", "font.size":12,
                        "figure.facecolor":"#fafafa", "axes.facecolor":"#fafafa"})
    df,train,history,test=data()
    fig,ax=plt.subplots(figsize=(10,3.7))
    for part,label,color in [(train,"Training: to 2013", "#444444"),
                             (df.iloc[-16:-4],"Validation: 2014-16", "#0072B2"),
                             (test,"Test: 2017", "#D55E00")]:
        ax.plot(part.ds,part.y,label=label,color=color,lw=1.8)
    style(ax); ax.legend(frameon=False,ncol=3,fontsize=10)
    save(fig,"tourism_history")
    for stems,name,intervals in [(["seasonal_naive","chronos2"],"tourism_zero_shot",True),
                               (["chronos2","chronos2_finetuned"],"tourism_finetuning",False),
                               (["timegpt"],"tourism_timegpt",True),
                               (["timegpt","timegpt_finetuned"],"tourism_timegpt_finetuning",False)]:
        if not all((RESULTS/f"{s}.csv").exists() for s in stems): continue
        fig,ax=plt.subplots(figsize=(10,3.7))
        ax.plot(history.ds.tail(16),history.y.tail(16),color="#444444",lw=1.8,label="History")
        ax.plot(test.ds,test.y,"o-",color="black",label="Observed 2017")
        ax.axvline(history.ds.iloc[-1],ls="--",lw=1,color="#888888")
        for stem in stems:
            fc=pd.read_csv(RESULTS/f"{stem}.csv",parse_dates=["ds"])
            label=fc.model.iloc[0]
            ax.plot(fc.ds,fc.prediction,"o-",lw=1.8,color=COLORS[label],label=label)
            if intervals and "lower80" in fc:
                ax.fill_between(fc.ds,fc.lower80,fc.upper80,alpha=.15,color=COLORS[label],label="80% interval")
        style(ax); ax.xaxis.set_major_locator(mdates.YearLocator())
        ax.legend(frameon=False,ncol=3,fontsize=9)
        save(fig,name)
    for suffix in ["", "_validation"]:
        rows=[]
        for stem in ["seasonal_naive","chronos2","chronos2_finetuned","timegpt","timegpt_finetuned"]:
            path=RESULTS/f"{stem}{suffix}.csv"
            if not path.exists(): continue
            fc=pd.read_csv(path)
            row={"model":fc.model.iloc[0],"n":len(fc),"MAE":(fc.y-fc.prediction).abs().mean(),
                 "RMSE":((fc.y-fc.prediction)**2).mean()**0.5}
            if "lower80" in fc:
                row.update(coverage80=fc.y.between(fc.lower80,fc.upper80).mean(),
                           width80=(fc.upper80-fc.lower80).mean())
            rows.append(row)
        pd.DataFrame(rows).to_csv(RESULTS/("validation_metrics.csv" if suffix else "test_metrics.csv"),index=False)
    versions={}
    for package in ["numpy","pandas","torch","chronos-forecasting","nixtla","peft"]:
        try: versions[package]=metadata.version(package)
        except metadata.PackageNotFoundError: pass
    (RESULTS/"environment.json").write_text(json.dumps(versions,indent=2))
    print(pd.read_csv(RESULTS/"test_metrics.csv").to_string(index=False),flush=True)

if __name__=="__main__":
    parser=argparse.ArgumentParser()
    parser.add_argument("action",choices=["prepare","chronos","timegpt","figures"])
    parser.add_argument("--env-file")
    args=parser.parse_args()
    if args.action=="prepare": prepare()
    elif args.action=="chronos": chronos()
    elif args.action=="timegpt": timegpt(args.env_file)
    figures()
