# Salt Stress in Grapevine — SALT'EAUX Project

This repository contains R scripts used in the analysis of grapevine physiological and agronomic responses to soil salinity as part of the **SALT'EAUX** project.

**Report Reference:**  
*Crabit A., Colin F., Pellegrino A., Deloire A., Schwertz E., Millan M.*  
Institut Agro Montpellier, October 2024

---

## Objective

The **SALT'EAUX** project investigates the impact of increasing soil salinity—particularly in coastal vineyards—on grapevine physiology, root architecture, berry development, and yield. The goal is to identify physiological indicators and management strategies to mitigate salinity-induced stress.

---

## Repository Content

The repository includes R scripts associated with the following measurements and analyses:

| File                  | Description                                                                 |
|-----------------------|-----------------------------------------------------------------------------|
| `SPAD.R`              | Chlorophyll content (SPAD) measurements to assess plant vigor              |
| `brix.R`              | Sugar concentration in berries (°Brix)                                      |
| `florapulse.R`        | Stem water potential measurements using Florapulse sensors                 |
| `meteo_villeroy.R`    | Meteorological data analysis from the Villeroy site                        |
| `petioles_limbes.R`   | Leaf (blade and petiole) ion content analysis                               |
| `racine.R`            | Root system traits and root density under salt stress                      |
| `rendement.R`         | Yield component analysis (e.g., bunch number, weight)                      |
| `volume_baies.R`      | Berry volume dynamics over the growing season                              |

---

## Context

### 🌱 Salinity and Grapevine Physiology

- Soil salinization is a growing concern due to rising evaporation and poor drainage.
- Grapevines tolerate moderate salinity (up to ~2 dS/m), but yield and vegetative growth are significantly reduced under higher salt concentrations.
- Two phases of stress were explored:
  - **Osmotic phase** (early reduction in water uptake)
  - **Ionic phase** (long-term sodium accumulation in leaves)

### 📍 Experimental Sites

- Analyses were conducted at **Jarras** and **Villeroy** vineyards (southern France).
- Vine plots were categorized as **salt-affected** (PS) or **non-affected** (PNS) based on field symptoms and sodium content in soil and plant tissues.

---

## Key Insights from the SALT'EAUX Phase 1 Report

- Sodium in canes (winter tissues) proved more reliable than soil sodium to indicate plant-level salt toxicity.
- SPAD and Florapulse provided insight into physiological functioning under salinity.
- Berry volume tracking and °Brix dynamics helped evaluate yield quality under stress.
- Soil salinity was mapped in 3D using **EM38-MK2 geophysical tools** and advanced statistical modeling (BRT and kriging).

---

## How to Use

Each script can be run independently depending on the dataset available. You may need:
- R packages: `ggplot2`, `dplyr`, `tidyr`, `readr`, `lubridate`, `ggpubr`, etc.
- Input data files (e.g., CSVs with plot-level observations)

---

## Acknowledgements

This work is part of the SALT'EAUX project funded by the French agricultural and environmental research initiatives, with contributions from Institut Agro Montpellier, SRDV, and various vineyard partners.

---

## Contact

For more information, contact  
**Mathilde Millan** — *Institut Agro Montpellier*

