# Intersectional inequalities in cancer screening participation in Spain (2009–2023) — interactive supplementary figures

Interactive supplementary material for the article *"Intersectional inequalities in cancer screening participation in Spain over six survey waves (2009–2023): a cross-classified MAIHDA analysis"* (J Epidemiol Community Health, submitted).

| File | Content |
|---|---|
| `Supplementary_Figure_S1.html` | Interactive intersectional trajectories: deviation of each intersectional stratum from the average stratum across six survey waves, probability scale |
| `Supplementary_Figure_S2.html` | Interactive version of Figure 3: deviation in predicted uptake from the average stratum (percentage points), all strata, screenings and waves |
| `index.html` | Landing page |

The files are self-contained (Plotly embedded) and open in any web browser, offline.

**Outcomes**: colorectal cancer screening (FOBT, 24 months, adults 50–69), breast screening (mammography, 24 months [36 in 2009], women 50–69), cervical screening (cytology, 36 months, women 25–65).
**Data**: Spanish national health surveys EESE 2009, ENSE 2011, EESE 2014, ENSE 2017, EESE 2020, ESdE 2023 (public-use microdata).
**Models**: null cross-classified MAIHDA logistic models (intersectional stratum × autonomous community), `lme4::glmer`; stratum BLUPs transformed to the probability scale as expit(β₀ + u) − expit(β₀).

License: CC BY 4.0.
