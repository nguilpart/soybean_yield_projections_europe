# soybean_yield_projections_europe

**Genereal information**

This repositoty is associated to the paper _Guilpart et al. (2022) Data-driven projections suggest large opportunities to improve Europe's soybean self-sufficiency under climate change, Nature Food._ It contains R scripts and data needed to reproduce key results of the paper. A brief content description is available below.

**Content description**

_1_read_format_data.R_ : R script used to read soybean yield and climate data and put them in a suitable format for modeling. It cannot be run without inputs data that are not provided here because they are available elsewhere. The output of this script is available in soybean_yield_climate_data.RData

_2_model_training_and_evaluation.R_ : R script used to train and assess models (with emphasis on Random Forest) to predict soybean yield and climate inputs. It can be run using as input the file soybean_yield_climate_data.RData provided here.

_3_making_yield_projections.R_ : R script used to make soybean yield projections in Europe from climate inputs using the Random Forest model fitted over the whole dataset. Projections are made on a yearly basis under historical climate (1981-2010) and future (2050-2059 and 2090-2099) climate scenarios (RCP4.5 and RCP8.5). This script cannot be run without inputs data (i.e. climate data) that are not provided here because they are available elsewhere. Soybean yield projections are available on Zenodo (add link).

_soybean_yield_climate_data.RData_ : Soybean yield and climate data in dataframe format suitable for modeling.






