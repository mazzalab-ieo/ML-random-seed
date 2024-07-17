# ML-random-seed
Model the variability of ML outcomes when changing the random seed

In this project we want to model the variability given to ML outcomes when changing the initial random seed.
We started from binary classification and we considered Logistic Regression with L1 regularization, Random Forest and Support Vector Machine.

We then modeled the standard deviation on the AUC obtained when running the classification algorithms 100 times, changing the initial random seed at any iteration.
