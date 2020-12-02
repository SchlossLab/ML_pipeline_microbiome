# development version

- Fixes check error on Solaris & note on Fedora.

# mikropml 0.0.1

This is the first release version of mikropml! 🎉

- Added a `NEWS.md` file to track changes to the package.
- Major new functions:
    - `run_ml()`
    - `preprocess_data()`
    - `plot_model_performance()`
    - `plot_hp_performance()`
- Support for ML methods in `run_ml()`:
    - `glmnet`: logistic and linear regression
    - `rf`: random forest
    - `rpart2`: decision trees
    - `svmRadial`: support vector machines
    - `xgbTree`: gradient-boosted trees
- New vignettes:
    - [Introduction](http://www.schlosslab.org/mikropml/articles/introduction.html)
    - [Preprocess data](http://www.schlosslab.org/mikropml/articles/preprocess.html)
    - [Hyperparameter tuning](http://www.schlosslab.org/mikropml/articles/tuning.html)
    - [Parallel processing](http://www.schlosslab.org/mikropml/articles/parallel.html)
    - [The mikropml paper](http://www.schlosslab.org/mikropml/articles/paper.html)
