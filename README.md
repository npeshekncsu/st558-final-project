*Author: Nataliya Peshekhodko*

# Goal

The purpose of the repo is to develop Shiny application which allows users to train logistic regression and random forest models for predicting water potability based on water quality metrics. Once the models are built, they could be used to predict water potability on new data. The application also enables users to conduct exploratory data analysis on the water potability dataset.

# Used packages and installation code

  - `shiny` - required for creating interactive web applications directly from R
  - `tidyverse` - is a collection of R packages, required for data transformation and manipulation
  - `caret` - required for training and evaluating machine learning models
  - `DT` - required for creating interactive and customizable HTML tables using the DataTables JavaScript library
  - `ggplot2` - required for for creating data visualizations and graphics
  - `corrplot` - required for correlation matrix visualizing
  - `Metrics` - required for evaluation metrics calculation

```
install.packages(c("shiny", "tidyverse", "caret", "DT", "ggplot2", "corrplot", "Metrics"))
```

# Render code

The code to run application from this repo in github:

```
shiny::runGitHub("npeshekncsu/st558-final-project", ref = "master")
```

