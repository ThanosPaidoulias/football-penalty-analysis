# Setup Instructions

## 🚀 Quick Setup

### Step 1: Install R Packages

```r
install.packages(c("data.table", "ggplot2", "MCMCpack", "reshape2", "knitr"))
```

### Step 2: Add Your Data

Place your `penalty_dataset.csv` file in the `data/` folder.

**Required columns:**
- `gender` - "men" or "women"
- `event_type` - "PG", "PSG", "PM", "PSM"
- `player_name` - Player name

### Step 3: Run Analysis

```r
source("analysis.R")
```

---

## 🐛 Troubleshooting

**File not found error:**
- Ensure `penalty_dataset.csv` is in `data/` folder
- Check working directory with `getwd()`

**Package errors:**
```r
# Install one by one if needed
install.packages("data.table")
```

---

## 📧 Need Help?

Open an issue on GitHub
