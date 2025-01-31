# CMPT 318 Assignment 1

## Cybersecurity CMPT 318 Spring 2025

This repository contains the code and solutions for the first group assignment of the CMPT 318 course. The goal of this assignment is to explore and prepare the Household Electricity Consumption Dataset using R for statistical analysis and anomaly detection.

## Table of Contents
1. [Tasks](#tasks)
   - [Task 1: Anomaly Detection](#task-1-anomaly-detection)
   - [Task 2: Correlation Analysis](#task-2-correlation-analysis)
   - [Task 3: Time Window Analysis](#task-3-time-window-analysis)
2. [Dataset](#dataset)
3. [Results](#results)
4. [How to Run](#how-to-run)
5. [License](#license)

## Tasks

### Task 1: Anomaly Detection

**Goal**: Apply statistical methods to detect point anomalies in the dataset.

- Handle missing data using linear interpolation.
- Identify anomalies using Z-scores.
- Calculate the percentage of data points considered as anomalies.

### Task 2: Correlation Analysis

**Goal**: Compute the correlation for each disjoint pair of the responses using Pearson’s sample correlation coefficient.

- Create a correlation matrix.
- Visualize the correlation matrix using color-coding.

### Task 3: Time Window Analysis

**Goal**: Analyze power consumption patterns in specific time windows.

- Determine representative time windows for day and night hours.
- Compute average Global_intensity values for these time windows over weekdays and weekends.
- Perform linear and polynomial regression for the time windows and visualize the results.

## Dataset

The dataset used in this assignment is the Household Electricity Consumption Dataset, which represents a multivariate time series describing power consumption behavior over time with one datapoint per minute. The time-dependent variables are:

- Global_active_power
- Global_reactive_power
- Voltage
- Global_intensity
- Submetering 1
- Submetering 2
- Submetering 3

## Results

The results of the analysis include:
1. Anomalies detected and their percentage.
2. Correlation matrix and its visualization.
3. Regression analysis and graphical representation of power consumption patterns.

## How to Run

1. Clone the repository:
    ```bash
    git clone https://github.com/prathamgarg03/CMPT-318-Assignment-1.git
    cd CMPT-318-Assignment-1
    ```
2. Open the R script files in your preferred R environment (e.g., RStudio).
3. Execute the scripts to perform the analysis and generate the results.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
