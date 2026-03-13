# AGENTS.md

## Repository purpose
This repository contains R scripts for cleaning, joining, and analyzing sleep diary and sleep-environment data.

## Working style
- Make minimal changes.
- Do not rewrite entire scripts unless explicitly asked.
- Do not rename exported objects or output objects unless explicitly asked.
- Do not change downstream script expectations without a clear reason and an inline comment.
- Preserve tidyverse style and prefer the native pipe `|>`.

## R coding conventions
- Prefer clear, readable tidyverse code.
- Avoid unnecessary loops when vectorized or tidyverse solutions are available.
- Keep variable names stable when they are already used in downstream scripts.
- Add short comments only where the logic is not obvious.
- Do not add new package dependencies unless necessary.

## Data handling
- Treat raw data files as immutable.
- Do not silently recode variables in ways that drop valid values.
- Preserve both raw coded variables and readable labeled variables when useful for analysis.
- Derive weekday from date variables when possible instead of relying on manually entered weekday fields.
- Keep date and time handling explicit and reproducible.

## Editing rules
- When editing a script, change only that script unless explicitly asked to update others.
- If a change may break another script, explain that clearly before or within the edit.
- Prefer small diffs.
- Keep existing folder structure.

## Validation
- After editing R code, check for obvious name mismatches, missing columns, and join-key problems.
- Flag uncertainty instead of guessing variable meanings.