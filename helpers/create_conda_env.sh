#!/bin/bash

# Specify the environment name
ENV_NAME="master_thesis_energy"

# Create the environment with Python and basic data science libraries
conda create -y -n $ENV_NAME python numpy pandas matplotlib seaborn jupyter scikit-learn

# Activate the environment
conda activate $ENV_NAME

# Install additional data science libraries
conda install -y -n $ENV_NAME -c conda-forge scipy statsmodels xgboost lightgbm catboost h2o

# Install Jupyter Notebook extensions
conda install -y -n $ENV_NAME -c conda-forge jupyter_contrib_nbextensions

# Install JupyterLab extensions
conda install -y -n $ENV_NAME -c conda-forge jupyterlab ipywidgets

# Deactivate the environment
conda deactivate

echo "Done!"
