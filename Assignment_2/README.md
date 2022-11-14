# PBSR Assignment 2 Overall Report

## Problem 1

We were given a dataset consisting of the number of goals scored by the home team in the every match. The task was to build a probability model hopefully get some nice results about them, and predicting the same. So, we tried with the given probability model, which is a geometric model, and tried to infer some results from it. We also tried using a Poisson model, and inferred the same.

## Problem 2

In this problem, we were tasked to simulate the Gamma distribution and compute MLE. We created our own version of MLE estimator using the `optim` function to arrive at our results.

## Problem 3

We analyzed the `faithful` dataset baked into R and tried to check which model fits the best for that dataset. We had 3 models to check, and to compare them, we used the AIC (Akaike Information Critereon) value to check which model suits the best. Also we used the best model thereafter to calulate a specific probability related to the waiting times. 

## Problem 4

We have to now analyze the `Insurance` dataset from `MASS` library and check various models to see which one fits best. We found the MLE for each model given in the question, and checked which one is best according to another criterion called BIC (Bayesian Information Critereon).

## Problem 5

We were given datasets on the share prices for TCS and Nifty50. We used the concept of log return for a particular model and estimated the parameters using 2 different techniques : Method of Moments and OLS method. We used that model to predict the prices of one with respect to another. Using that model, we were told to calculate the estimated price of one company shares provided the price of the other fluctuated.
