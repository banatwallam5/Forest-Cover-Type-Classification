# Forest-Cover-Type-Classification
Intro: The data set we used in this project comes from the University of Irvine Repository, The link of the Data set is as follows

Forest Cover_type Data set:https://archive.ics.uci.edu/ml/datasets/covertype

The study area includes four wilderness areas located in the Roosevelt National Forest of northern Colorado. These areas represent forests with minimal human-caused disturbances, so that existing forest cover types are more a result of ecological processes rather than forest management practices.

The Challenge for us with this data is to accurately predict forest cover type from cartographic variables only (no remotely sensed data). The actual forest cover type for a given observation (30 x 30 meter cell) was determined from US Forest Service (USFS) Region 2 Resource Information System (RIS) data. Independent variables were derived from data originally obtained from US Geological Survey (USGS) and USFS data.Our original data was in raw form (not scaled) and contained binary (0 or 1) columns of data for qualitative independent variables (wilderness areas and soil types).

We have used a portion of the data to train our model with, the portion of the data we used comes from the following link:

Data used for training:https://www.kaggle.com/c/forest-cover-type-prediction/data?select=train.csv

The data has a total of 15120 cases and is well balanced with 2160  cases for each class. Furthermore we have in our data set 65 features,  of these 44 are binary features which we will convert to numerical by number assignments, more details are given below.

