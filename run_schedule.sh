#!/bin/bash
set -e

APP_DIR="/Users/cjyoon/Dropbox/Osler/Osler_shiny"

# run intern schedule
python "$APP_DIR/intern2025/osler_intern_schedule_final.py"

# run jar/sar schedule
python "$APP_DIR/jarsar2025/osler_JARSAR_schedule_final.py"

# update the master table based on the above python files
Rscript "$APP_DIR/update_schedules.R"

# upload to shinyapps.io
Rscript -e "rsconnect::deployApp(\"$APP_DIR\", appFiles = c(\"app.R\", \"block_schedule\", \"osler_schedule_table.tsv\", \"www\"))"
