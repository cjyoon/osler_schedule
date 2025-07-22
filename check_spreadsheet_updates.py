import hashlib
import os
import pickle
import subprocess
import shutil
import datetime
import pandas as pd
from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request

# CONFIG
SHEET_IDS = {
    "intern": "13Oroy73T-D2vogXlEk0ANXpBtcyrN-_zgUw56OgAAis",
    "jarsar": "1-XS4pjJCLRTBD595tIyYlj_Wji61cvmEiPxIg51rDfM"
}

RANGE_NAME = "A1:Z1000"
SCOPES = [
    'https://www.googleapis.com/auth/spreadsheets.readonly',
    'https://www.googleapis.com/auth/drive.readonly'
]

HASH_DIR = "hash"
OLD_SCHEDULE_DIR = "old_schedule"
CHANGE_LOG_FILE = "www/past_updates.txt"
PAST_UPDATES_FILE = "www/past_updates.txt"
DEPLOY_SCRIPT = "/Users/cjyoon/Dropbox/Osler/Osler_shiny/run_schedule.sh"

DOWNLOAD_PATHS = {
    "intern": "/Users/cjyoon/Dropbox/Osler/Osler_shiny/intern2025/Interns 2025-2026.xlsx",
    "jarsar": "/Users/cjyoon/Dropbox/Osler/Osler_shiny/jarsar2025/Resident Schedule 2025-2026.xlsx"
}

# Ensure necessary directories exist
os.makedirs(HASH_DIR, exist_ok=True)
os.makedirs(OLD_SCHEDULE_DIR, exist_ok=True)
for path in DOWNLOAD_PATHS.values():
    os.makedirs(os.path.dirname(path), exist_ok=True)

def get_service(api_name):
    creds = None
    if os.path.exists('token.json'):
        with open('token.json', 'rb') as token:
            creds = pickle.load(token)
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file('credentials.json', SCOPES)
            creds = flow.run_local_server(port=0)
        with open('token.json', 'wb') as token:
            pickle.dump(creds, token)

    if api_name == "sheets":
        return build(api_name, "v4", credentials=creds)
    elif api_name == "drive":
        return build(api_name, "v3", credentials=creds)
    else:
        raise ValueError(f"Unknown API name: {api_name}")

def get_sheet_data(sheet_id):
    service = get_service('sheets')
    result = service.spreadsheets().values().get(
        spreadsheetId=sheet_id, range=RANGE_NAME).execute()
    return result.get('values', [])

def get_sheet_hash(sheet_data):
    sorted_rows = sorted(['\t'.join(row) for row in sheet_data])
    content = '\n'.join(sorted_rows)
    return hashlib.sha256(content.encode()).hexdigest()

def has_changed(name, new_hash):
    hash_file = os.path.join(HASH_DIR, f"{name}.hash")
    if os.path.exists(hash_file):
        with open(hash_file, 'r') as f:
            old_hash = f.read()
        if old_hash == new_hash:
            return False
    with open(hash_file, 'w') as f:
        f.write(new_hash)
    return True

def archive_old_excel(path, name):
    if os.path.exists(path):
        timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = os.path.basename(path)
        new_path = os.path.join(OLD_SCHEDULE_DIR, f"{name}_{timestamp}_{filename}")
        shutil.move(path, new_path)
        return new_path
    return None

def download_sheet_as_excel(file_id, destination_path):
    drive_service = get_service('drive')
    request = drive_service.files().export_media(fileId=file_id, mimeType='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')
    with open(destination_path, 'wb') as f:
        downloader = request.execute()
        f.write(downloader)
    print(f"Downloaded to: {destination_path}")

def compare_excels(old_path, new_path):
    try:
        old_df = pd.read_excel(old_path, dtype=str, engine='openpyxl').fillna("")
        old_df.columns = ["Last Name", "First Name"] + list(old_df.columns[2:])

        new_df = pd.read_excel(new_path, dtype=str, engine='openpyxl').fillna("")
        new_df.columns = ["Last Name", "First Name"] + list(new_df.columns[2:])

        all_columns = sorted(set(old_df.columns).union(set(new_df.columns)))
        old_df = old_df.reindex(columns=all_columns, fill_value="")
        new_df = new_df.reindex(columns=all_columns, fill_value="")

        # Sort by both Last Name and First Name
        if "Last Name" in all_columns and "First Name" in all_columns:
            old_df = old_df.assign(
                __last=old_df["Last Name"].str.lower(),
                __first=old_df["First Name"].str.lower()
            ).sort_values(by=["__last", "__first"]).drop(columns=["__last", "__first"]).reset_index(drop=True)

            new_df = new_df.assign(
                __last=new_df["Last Name"].str.lower(),
                __first=new_df["First Name"].str.lower()
            ).sort_values(by=["__last", "__first"]).drop(columns=["__last", "__first"]).reset_index(drop=True)
        else:
            old_df = old_df.reset_index(drop=True)
            new_df = new_df.reset_index(drop=True)

        diff = []
        max_rows = max(len(old_df), len(new_df))

        for i in range(max_rows):
            old_row = old_df.iloc[i] if i < len(old_df) else pd.Series([""] * len(all_columns), index=all_columns)
            new_row = new_df.iloc[i] if i < len(new_df) else pd.Series([""] * len(all_columns), index=all_columns)

            last_name = new_row.get("Last Name", old_row.get("Last Name", "")).strip()
            first_name = new_row.get("First Name", old_row.get("First Name", "")).strip()

            for col in all_columns:
                if col in ["Last Name", "First Name"]:
                    continue

                old_val = old_row[col].strip()
                new_val = new_row[col].strip()

                if old_val != new_val:
                    def clean_rotation(rot):
                        return rot.replace(" - ", "-").replace(" -", "-").replace("- ", "-")
                    old_val_c = clean_rotation(old_val)
                    new_val_c = clean_rotation(new_val)

                    diff.append(f"{first_name} {last_name} {col}: {old_val_c} -> {new_val_c}")

        return diff

    except Exception as e:
        return [f"Error comparing files: {str(e)}"]

def main():
    changed = False
    for name, sheet_id in SHEET_IDS.items():
        data = get_sheet_data(sheet_id)
        h = get_sheet_hash(data)
        if has_changed(name, h):
            print(f"Change detected in: {name}")
            changed = True

            if name in DOWNLOAD_PATHS:
                print(f"Archiving current {name} Excel...")
                old_excel_path = archive_old_excel(DOWNLOAD_PATHS[name], name)

                print(f"Downloading updated {name} schedule as Excel...")
                download_sheet_as_excel(sheet_id, DOWNLOAD_PATHS[name])

                if old_excel_path:
                    print(f"Comparing old and new {name} Excel files...")
                    differences = compare_excels(old_excel_path, DOWNLOAD_PATHS[name])
                    if differences:
                        log_entry = f"\n--- Changes for {name} on {datetime.datetime.now().strftime('%m-%d-%y')} ---\n" + "\n".join(differences) + "\n"

                        if os.path.exists(CHANGE_LOG_FILE):
                            with open(CHANGE_LOG_FILE, 'r') as log_file:
                                existing_log = log_file.read()
                        else:
                            existing_log = ""

                        with open(CHANGE_LOG_FILE, 'w', encoding="utf-8") as log_file:
                            log_file.write(log_entry + existing_log)
                    else:
                        print(f"No differences found between old and new {name} schedule.")

    if has_changed("past_updates", PAST_UPDATES_FILE):
        print("Change detected in past_updates.txt")
        changed = True

    if changed:
        print("Running deployment...")
        subprocess.run([DEPLOY_SCRIPT])

if __name__ == "__main__":
    main()