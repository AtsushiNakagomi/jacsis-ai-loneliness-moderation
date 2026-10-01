# data/

No survey data are distributed in this repository; JACSIS is a restricted-access survey.

To run the pipeline on the real data, place here (or point the environment variables read at the
top of `moderator-wide.r` and `diagnosis.r` at):

- `jacsis_2wave2425.csv` — all respondents with valid 2024 and 2025 data (`JACSIS_2WAVE_2425_PATH`)
- `jacsis_2024_all.csv` — all valid 2024 respondents; attrition analysis and retention weights (optional; `JACSIS_2024_ALL_PATH`)
- `df_ref5_list.csv` — `RI_ID_2024` of the earlier analytic sample; overlap count (optional; `REF5_LIST_PATH`)
