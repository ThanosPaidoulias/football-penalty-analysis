# Football Penalty Analysis: Statistical Investigation & Strategic Insights

**Advanced statistical analysis of penalty kick performance, gender differences, and optimal shootout strategies**

[![R](https://img.shields.io/badge/R-4.0+-276DC3.svg)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

---

## 📋 Project Overview

A comprehensive statistical investigation of football penalty kicks addressing three key questions:

1. **Do men and women have different penalty conversion rates?**
2. **Who are the best penalty takers?**
3. **How should we order penalty takers in a shootout?**

**Key Methodologies:**
- Chi-square test, Cramer's V, Bootstrap resampling
- Bayesian inference with MCMC simulations
- Custom penalty skill metrics
- Strategic shootout ordering framework

---

## 🎯 Key Findings

### Question 1: Gender Differences
| Method | Finding |
|--------|---------|
| **Chi-square Test** | p < 0.05 (statistically significant) |
| **Cramer's V** | 0.02 (practically negligible effect) |
| **Bootstrap CI** | Contains 0 (no meaningful difference) |

**Conclusion:** While statistically detectable, gender difference is negligible in practical terms.

---

### Question 2: Top 5 Penalty Takers

**Top 5 (Full Dataset):**
1. **Cristiano Ronaldo** - 149 attempts
2. **Robert Lewandowski**
3. **Lionel Messi**
4. **Sergio Ramos**
5. **Bruno Fernandes**

---

### Question 3: Shootout Strategy (Manchester United 2020/21)

**Recommended Order:**
1. **Edinson Cavani** - Strong opener
2. **Bruno Fernandes** - Maintains momentum
3. **Marcus Rashford** - Reliable mid-sequence
4. **Anthony Martial** - Leverages strengths
5. **Juan Mata** - Handles decisive moment

---

## 📁 Repository Structure

```
football-penalty-analysis/
├── README.md                    # This file
├── FOOTBALL PENALTY ANALYSIS    # Full report
├── SETUP.md                     # Setup instructions
├── analysis.R                   # Complete analysis script
├── requirements.txt             # R package dependencies
├── LICENSE                      # MIT License
│
├── data/
│   ├── README.md               # Data format instructions
│   └── penalty_dataset.csv     # Your data (add this)

```

---

## 🚀 Quick Start

### 1. Install R Packages
```r
install.packages(c("data.table", "ggplot2", "MCMCpack", "reshape2", "knitr"))
```

### 2. Add Your Data or Use the Existing Dataset
Place `penalty_dataset.csv` in the `data/` folder

### 3. Run Analysis
```r
source("Analysis.R")
```


---

## 📊 Methodology

### Statistical Testing (Question 1)
- **Chi-square test** - Tests independence
- **Cramer's V** - Measures effect size
- **Bootstrap** - Non-parametric validation (10,000 iterations)

### Penalty Taker Rankings (Question 2)
Four approaches combined:
1. **Bootstrap** - Confidence intervals
2. **Bayesian inference** - MCMC simulations
3. **Custom metric** - Weighted scoring
4. **Combined skill** - 0-100 scale

### Shootout Strategy (Question 3)
8-factor priority framework:
- Quantitative skill assessment
- Psychological resilience
- Shootout experience
- Physical readiness

---

## 📖 References

- Vollmer et al. (2023). Penalty shootouts are tough, but the alternating order is fair.
- Apesteguia & Palacios-Huerta (2010). Psychological pressure in competitive environments.
- Full bibliography in report.pdf

---

## 👤 Author

**Thanos Paidoulias**
- [https://www.linkedin.com/in/thanos-paidoulias/]
- [GitHub](https://github.com/ThanosPaidoulias)

---

## 📝 License

MIT License

---

⭐ **Star this repository if you found it useful!**
