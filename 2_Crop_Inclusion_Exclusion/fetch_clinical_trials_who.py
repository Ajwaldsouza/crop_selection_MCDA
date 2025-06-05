#!/usr/bin/env python3
import os
import sys
import zipfile
import requests
import pandas as pd
from tqdm import tqdm

# --- CONFIGURATION ---
ICTRP_ZIP_URL = (
    "https://trialsearch.who.int/Trial2.0/DownloadCSV?filename=ICTRP_Dataset.zip"
)
ZIP_FILE    = "ICTRP_Dataset.zip"
CSV_FILE    = "ICTRP_Dataset.csv"
SPECIES_CSV = "species_list.csv"
OUTPUT_CSV  = "species_trial_counts.csv"

# Browser‑style header to help avoid 403
HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/115.0.0.0 Safari/537.36"
    )
}

def download_ictpr_zip():
    print(f"Downloading ICTRP dataset from {ICTRP_ZIP_URL} ...")
    resp = requests.get(ICTRP_ZIP_URL, stream=True, headers=HEADERS)
    try:
        resp.raise_for_status()
    except requests.exceptions.HTTPError as e:
        print(f"ERROR: could not download (HTTP {resp.status_code}).")
        print("→ Please manually download the ICTRP ZIP from the WHO OneDrive")
        print("  (requires Microsoft login; expires after 10 days)")
        print(f"  and save it here as: {ZIP_FILE}")
        sys.exit(1)
    with open(ZIP_FILE, "wb") as f:
        for chunk in resp.iter_content(1024 * 1024):
            f.write(chunk)
    print("Download complete.")

def extract_csv_from_zip():
    print(f"Extracting {ZIP_FILE} …")
    with zipfile.ZipFile(ZIP_FILE, "r") as z:
        # find the first CSV inside
        for fn in z.namelist():
            if fn.lower().endswith(".csv"):
                z.extract(fn, ".")
                os.rename(fn, CSV_FILE)
                print(f"Extracted to {CSV_FILE}.")
                return
    print("ERROR: no CSV found inside ZIP.")
    sys.exit(1)

def main():
    # 1) Ensure ZIP is present
    if not os.path.exists(ZIP_FILE):
        download_ictpr_zip()
    # 2) Ensure CSV is extracted
    if not os.path.exists(CSV_FILE):
        extract_csv_from_zip()

    # 3) Load species list
    if not os.path.exists(SPECIES_CSV):
        print(f"ERROR: species list not found: {SPECIES_CSV}")
        sys.exit(1)
    species_df = pd.read_csv(SPECIES_CSV, header=None, names=["species"])
    print(f"Loaded {len(species_df)} species from {SPECIES_CSV}.")

    # 4) Load ICTRP data
    print(f"Loading ICTRP data from {CSV_FILE} (this may take a few minutes)...")
    ictp_df = pd.read_csv(CSV_FILE, dtype=str, low_memory=False)
    print(f"Loaded {len(ictp_df)} trial records.")

    # 5) Pick searchable columns
    search_cols = [
        col for col in ["Scientific title", "Public title", "Conditions", "Interventions"]
        if col in ictp_df.columns
    ]
    if not search_cols:
        print("ERROR: none of the expected columns found in ICTRP data.")
        print("Columns available:", ictp_df.columns.tolist())
        sys.exit(1)
    print("Searching in columns:", search_cols)

    # 6) Count trials per species
    results = []
    for sp in tqdm(species_df["species"], desc="Species"):
        mask = False
        for col in search_cols:
            mask |= ictp_df[col].str.contains(sp, case=False, na=False)
        results.append({"species": sp, "trial_count": int(mask.sum())})

    # 7) Write output CSV
    out_df = pd.DataFrame(results)
    out_df.to_csv(OUTPUT_CSV, index=False)
    print(f"Done! Results written to {OUTPUT_CSV}")

if __name__ == "__main__":
    main()
