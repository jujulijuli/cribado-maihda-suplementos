# Territorial and intersectional inequalities in cancer screening participation as organized programmes expanded in Spain (2009–2025) — supplementary material

Supplementary material for the article *"Territorial and intersectional inequalities in cancer screening participation as organized programmes expanded in Spain (2009–2025): a cross-classified MAIHDA analysis"* (J Epidemiol Community Health, submitted).

Librero-López J, Pérez-Vicente R, Cayuela A, Cayuela L, Redín-Sagrero P, Egozkue I, Etxegia J, Oscoz-Villanueva I, Tamayo I, Enguita-Germán M, Ibáñez-Beroiz B, Pastor-Valero M, Gimeno-Feliu L.

## Files

| File | Content |
|---|---|
| `Supplementary_Figure_S1.html` | Interactive intersectional trajectories: deviation of each intersectional stratum from the average stratum across seven survey waves, probability scale |
| `Supplementary_Figure_S2.html` | Interactive version of Figure 3: deviation in predicted uptake from the average stratum (percentage points), all strata, screenings and waves |
| `Supplementary_Figure_S3_gaps_decomposition.html` | Absolute participation gaps by component (2009–2025): education component, origin component, and total combined gap, with 95% delta-method CIs |
| `Online_Supplemental_Material.docx` | Supplementary Tables S0–S3 and Online Supplemental Methods (SM1–SM4) |
| `index.html` | Landing page |
| `code/` | R analysis code; see `code/README.md` |

The HTML figures are self-contained (Plotly embedded) and open in any web browser, offline.

**Outcomes**: colorectal cancer screening (FOBT, 24 months, adults 50–69), breast screening (mammography, 24 months [36 in 2009], women 50–69), cervical screening (cytology, 36 months, women 25–65).  
**Data**: Spanish national health surveys EESE 2009, ENSE 2011/12, EESE 2014, ENSE 2017, EESE 2020, ESdE 2023, ESdE 2025 (public-use microdata, INE / Ministerio de Sanidad).  
**Models**: null cross-classified MAIHDA logistic models (intersectional stratum × autonomous community), `lme4::glmer`; stratum BLUPs transformed to the probability scale as expit(β₀ + u) − expit(β₀).

License: CC BY 4.0  
DOI: [10.5281/zenodo.24189984](https://doi.org/10.5281/zenodo.24189984)
