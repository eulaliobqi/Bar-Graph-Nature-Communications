# 📊 qRT-PCR Graph Analyzer

An **R-based tool** for analyzing and visualizing **qRT-PCR** data, developed to **automate** the generation of standardized publication-ready plots (*Nature* style) with integrated statistical testing.  

---

## 🔎 Project Summary

qRT-PCR data analysis often faces challenges such as:  
- Excessive time spent preparing publication-quality figures;  
- The need for advanced statistical and visualization skills;  
- Errors and inconsistencies caused by manual processing.  

This issue became evident during the preparation of one of our articles published in *Nature Communications*.  

**Proposed solution:** creation of an **interactive R tool** that integrates statistical tests (t-test, Tukey, Dunnett) and generates high-resolution, standardized plots ready for publication.  

---

## 🎯 Objectives

- Standardize and streamline the qRT-PCR data analysis workflow;  
- Reduce the time required for figure preparation;  
- Minimize human error during data handling;  
- Ensure visual and methodological consistency;  
- Increase the reliability of statistical results.  

---

## ⚙️ Methodology

The tool was developed in **R**, providing an interactive workflow that allows:  

1. **Data Import**  
   - Excel files (`.xlsx`, `.xls`), CSV, and TXT  
   - Option to select specific sheets  

2. **Configuration & Customization**  
   - Title, subtitle, X and Y axis labels  
   - Customizable color palettes  
   - Selection of statistical tests (t-test, Tukey, Dunnett)  

3. **Analysis & Visualization**  
   - **Bar plots** with standard error bars  
   - Display of **individual sample points**  
   - Automatic significance annotation  
     - `*` for t-test and Dunnett  
     - Letters for Tukey  

4. **Export**  
   - Publication-quality high-resolution figures  

---

## 🧪 Features

- ✅ Simple and intuitive interface  
- ✅ Data upload and sheet selection  
- ✅ Data visualization in table format  
- ✅ Automated statistical tests  
- ✅ Automatic significance annotation  
- ✅ Export to publication-ready image formats  

---

## 📦 Installation

To install required R packages, run:

```r
install.packages(c(
  "readxl",
  "tidyverse",
  "ggplot2",
  "ggpubr",
  "multcompView",
  "multcomp",
  "ggsignif"
))

```mermaid
flowchart TD
    A[📂 Import Data] --> B[⚙️ Configure Parameters]
    B --> C[📊 Run Statistical Tests]
    C --> D[🖼️ Visualize Plots]
    D --> E[📤 Export Results]
