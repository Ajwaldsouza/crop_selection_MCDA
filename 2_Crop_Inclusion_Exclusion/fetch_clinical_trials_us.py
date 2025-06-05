import pandas as pd
import requests
import argparse
from concurrent.futures import ThreadPoolExecutor
from tenacity import retry, stop_after_attempt, wait_exponential
from urllib.parse import quote_plus
import time
import logging

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

CLINICAL_TRIALS_API = "https://clinicaltrials.gov/api/v2/studies"

@retry(stop=stop_after_attempt(3), wait=wait_exponential(multiplier=1))
def get_trial_count(plant_name):
    """
    Query ClinicalTrials.gov API for a plant name and return trial count
    Now searches Condition, InterventionName, AND Keyword fields
    """
    if pd.isna(plant_name) or not plant_name.strip():
        return 0
    
    try:
        # Construct API query with three search areas
        encoded_name = quote_plus(plant_name.strip())
        query = (
            f'(AREA[Condition]"{encoded_name}") '
            f'OR (AREA[InterventionName]"{encoded_name}") '
            f'OR (AREA[Keyword]"{encoded_name}")'  # Added Keyword search
        )
        
        params = {
            "filter.advanced": query,
            "countTotal": "true",
            "pageSize": 1
        }
        
        response = requests.get(CLINICAL_TRIALS_API, params=params)
        response.raise_for_status()
        
        return response.json().get("totalCount", 0)
    
    except Exception as e:
        logging.error(f"Failed for {plant_name}: {str(e)}")
        raise

# Rest of the script remains identical to previous version
def process_plant_names(df, column_name, max_workers=5):
    """
    Process dataframe with parallel requests
    """
    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        results = list(executor.map(
            lambda x: (x, get_trial_count(x)),
            df[column_name],
            chunksize=10
        ))
        # Add rate limiting between batches
        time.sleep(1)
    
    # Create results series maintaining original order
    result_series = pd.Series(
        [count for _, count in results],
        index=df.index
    )
    
    return df.assign(clinical_trial_count=result_series)

def main():
    parser = argparse.ArgumentParser(description='Count clinical trials for plant names')
    parser.add_argument('input_file', help='Input CSV file path')
    parser.add_argument('output_file', help='Output CSV file path')
    parser.add_argument('--column', default='plant_name', 
                       help='Column name containing plant names (default: plant_name)')
    parser.add_argument('--workers', type=int, default=5,
                       help='Number of parallel workers (default: 5)')
    
    args = parser.parse_args()
    
    try:
        # Read input data
        df = pd.read_csv(args.input_file)
        logging.info(f"Loaded {len(df)} records from {args.input_file}")
        
        # Process data
        processed_df = process_plant_names(df, args.column, args.workers)
        
        # Save results
        processed_df.to_csv(args.output_file, index=False)
        logging.info(f"Results saved to {args.output_file}")
        
    except Exception as e:
        logging.error(f"Processing failed: {str(e)}")
        raise

if __name__ == "__main__":
    main()