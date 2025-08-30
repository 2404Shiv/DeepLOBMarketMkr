#!/usr/bin/env bash
set -euo pipefail

# Run quick smoke test
Rscript smoke_test.R

# Render report
R -e 'rmarkdown::render("DeepLOB_auto_report.Rmd", output_format="html_document")'

echo "✅ Report written to DeepLOB_auto_report.html"
