## Downloading Plot Data

The VegBank web app lets you download up to **20,000 plot observations** at a time as a ZIP archive containing several related CSV files. For bulk or programmatic access, consider using the [vegbankr](https://nceas.github.io/vegbankr/articles/download.html) R package, which provides full access to all VegBank data without the 20,000-row limit.


* * *

### How to Download

1. Navigate to the **Plots** tab.
2. Use the **search box** or a **filter** (from a Project, Party, Plant, or Community view) to narrow the table to the plots you want. The download button is only active when the table contains **20,000 or fewer** rows.
3. Click the **Download CSV** button above the table. A loading indicator will appear while the data is fetched from the API.
4. Save the resulting `.zip` file to your computer and extract it.

* * *

### What's in the ZIP

After extracting the archive you will find a `README.txt` and up to eleven CSV files. **Files containing no data for your download are omitted.** Each file uses a short VegBank code column as its primary key (e.g. `ob_code`, `to_code`).

| File | Primary key | Description |
|------|-------------|-------------|
| `plot_observations.csv` | `ob_code` | One row per plot observation — the primary table. All other files link back to it via `ob_code`. |
| `taxon_observations.csv` | `to_code` | Plant taxa recorded on a plot. Joins to `plot_observations` via `ob_code`. |
| `taxon_interpretations.csv` | `ti_code` | Assignments of plant concepts to taxon observations. Joins to `plot_observations` via `ob_code`, `taxon_observations` via `to_code`, and `plant_concepts` via `pc_code`. |
| `taxon_importances.csv` | `tm_code` | Cover and other importance metrics for a taxon observation, optionally within a stratum. Joins to `plot_observations` via `ob_code`, `taxon_observations` via `to_code`, and `strata` via `sr_code`. |
| `stem_counts.csv` | `sc_code` | Stem counts associated with an importance record. Joins to `plot_observations` via `ob_code`, `taxon_observations` via `to_code`, `taxon_importances` via `tm_code`, and `strata` via `sr_code`. |
| `plant_concepts.csv` | `pc_code` | Plant concept reference table. Reach it from `taxon_interpretations` via `pc_code`. |
| `strata.csv` | `sr_code` | Vegetation strata (layers) defined for a plot observation. Joins to `plot_observations` via `ob_code`. |
| `community_classifications.csv` | `ci_code` | Community interpretation events for a plot. Each classification event (`cl_code`) may have multiple interpretations. Joins to `plot_observations` via `ob_code` and `community_concepts` via `cc_code`. |
| `community_concepts.csv` | `cc_code` | Community concept reference table. Reach it from `community_classifications` via `cc_code`. |
| `projects.csv` | `pj_code` | Projects under which observations were made. Joins from `plot_observations` via `pj_code`. |
| `parties.csv` | `py_code` | People and organizations contributing to plot observations. |
| `README.txt` | — | Download metadata, applied filters, file descriptions, and citation. |

* * *

### Joining the CSVs

`ob_code` is the common key that links most tables back to `plot_observations.csv`. It contains a VegBank Code of the form `ob.XXXXX`.

#### R

```r
library(readr)
library(dplyr)

plots       <- read_csv("plot_observations.csv")
taxon_obs   <- read_csv("taxon_observations.csv")
taxon_interp <- read_csv("taxon_interpretations.csv")
plants      <- read_csv("plant_concepts.csv")
comm_class  <- read_csv("community_classifications.csv")
comm_conc   <- read_csv("community_concepts.csv")
strata      <- read_csv("strata.csv")
importances <- read_csv("taxon_importances.csv")

# All taxa observed on each plot, with plot metadata
plots_with_taxa <- plots |>
  left_join(taxon_obs, by = "ob_code")

# Resolve taxon observations to named plant concepts
taxa_named <- taxon_obs |>
  left_join(taxon_interp, by = "to_code") |>
  left_join(plants, by = "pc_code")

# Cover/importance values per taxon per plot (optionally per stratum)
taxa_cover <- taxon_obs |>
  left_join(importances, by = "to_code") |>
  left_join(strata, by = "sr_code")

# Community concepts assigned to each plot
plots_with_community <- plots |>
  left_join(comm_class, by = "ob_code") |>
  left_join(comm_conc, by = "cc_code")
```

#### Python

```python
import pandas as pd

plots        = pd.read_csv("plot_observations.csv")
taxon_obs    = pd.read_csv("taxon_observations.csv")
taxon_interp = pd.read_csv("taxon_interpretations.csv")
plants       = pd.read_csv("plant_concepts.csv")
comm_class   = pd.read_csv("community_classifications.csv")
comm_conc    = pd.read_csv("community_concepts.csv")
strata       = pd.read_csv("strata.csv")
importances  = pd.read_csv("taxon_importances.csv")

# All taxa observed on each plot, with plot metadata
plots_with_taxa = plots.merge(taxon_obs, on="ob_code", how="left")

# Resolve taxon observations to named plant concepts
taxa_named = (taxon_obs
    .merge(taxon_interp, on="to_code", how="left")
    .merge(plants, on="pc_code", how="left"))

# Cover/importance values per taxon per plot (optionally per stratum)
taxa_cover = (taxon_obs
    .merge(importances, on="to_code", how="left")
    .merge(strata, on="sr_code", how="left"))

# Community concepts assigned to each plot
plots_with_community = (plots
    .merge(comm_class, on="ob_code", how="left")
    .merge(comm_conc, on="cc_code", how="left"))
```

* * *

### Limits

- **Plot observations** are capped at **20,000 records** per download. If your filter matches more than 20,000 plots the download button will be disabled — refine your search further.
- **All other CSV files** (taxa, strata, communities, etc.) are each limited to **1,000,000 records** and include all rows associated with the downloaded plot observations.
- The `obs_count` column that appears in some reference tables (e.g. `plant_concepts`, `community_concepts`) reports the *total* count of observations in VegBank, not the count specific to your download.

* * *

### Notes

- **Empty files are omitted**: If a set of plot observations has no stratum data, for example, `strata.csv` will not appear in the ZIP.
- **Location fuzzing**: Some plot coordinates are intentionally imprecise to protect sensitive species or private land.
- **Superseded plots**: The Plots table shows only current plot observations by default. Superseded versions are hidden unless you change the **Status** dropdown. The downloaded data reflects whichever rows were visible in the table at the time of download.
- **Large downloads**: For datasets larger than 20,000 plots, use the [vegbankr](https://nceas.github.io/vegbankr/articles/download.html) R package.
