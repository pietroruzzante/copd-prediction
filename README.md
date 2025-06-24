# Predicting COPD onset risk thorugh Machine Learning techniques

This project aims to predict the 10-year risk of developing Chronic Obstructive Pulmonary Disease (COPD) in aging adults using a logistic regression model. The dataset comprises health data from 3,980 UK subjects, each represented by 18 baseline features and a binary COPD outcome (1 = COPD, 0 = No COPD).

Dataset details: 
- Total subjects: 3,980
- Variables: 19 (18 predictors + 1 binary outcome)
- COPD prevalence: 377 cases (~9.5%)
- Age range: 49–90 years (mean ≈ 63.7)
- Gender split: 2,237 females, 1,743 males

## Analysis steps
### 1. Data preprocessing
  - Stratified Train-Test Split: 80/20 split using stratification to preserve class distribution (set.seed(0) used).
  - Model Feasibility Check: Checked logistic model viability (Rule of 10 events per variable). Result: 302/19 > 10 → feasible.
  - Outlier Handling: Outliers replaced with NA (threshold: mean ± 4*SD). Affected variables: bmi, fev1, fvc, fev1_fvc_ratio.
  - Multicollinearity Check: Correlation heatmap created with corrplot(). Removed fev1 due to high correlation with fvc.
  - Missing Data Imputation thorugh mean and mode imputation
  - Normalization
  - Min-max normalization [0,1] applied to numeric variables
  - 
### 2. Model training
  - Full Model Training
  - Simple feature selection: Backward Selection, Forward Selection, Stepwise Backward and Stepwise Forward showed consistent results

### 3. Robust feature selection (Bootstrap Resampling)
- Performed 50 bootstrap iterations with backward feature selection.
- Tracked variable inclusion frequency and features selected in ≥60% of iterations retained

Selected variables: gender1, smoking1, smoking2, mod_vig_pa, fvc, short_breath_walking1, wheezing1, pf, fev1_fvc_ratio, asthma_hx1, sr_poor_health

### Final AUC: 0.82




