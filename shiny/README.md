---
title: "Data Science Specialization - Capstone Project (Text Prediction)"
author: "Derick"
date: "08 April 2016"
output: html_document
---

## Introduction

This application uses an n-gram model (2-, 3-, and 4-grams) to predict the next word based on an input phrase entered by the user (English only).  The n-gram model was prepared using a 5% sample of text lines across news, blogs and tweets provided as input data to the project.  The n-gram counts were adjusted using Good Turing Smoothing to make provision for unseen words and n-grams.  Predictions are rated using interpolation of the maximum likelihood estimates of the n-grams where the last words of the input phrase match the n-gram.  The last word of the highest rated n-gram is the first predicted word.

## Part 1 Shiny Application

The application allows the user to enter an input phrase, the next word is predicted by the app when the user clicks the <Predict> button.

Profanity filtering can be switched on or off by selecting the checkbox.  When switched on, a predefined list of words will not be included as predictions, but will be replaced with a string indicating profanity.

When the Verbose mode switch is on, the application will not only display the predicted words, but also the Maximum Likelihood Estimates (MLEs) from the model which was used to derive the predictions from the input phrase.

The maximum number of words returned per prediction is controlled using the slider (3 to 10).

The application is hosted on shinyapps.io at:  

  https://derickj.shinyapps.io/predict

## Part 2

The second part of the assignment was to produce a slide deck (reproducible pitch) about the application

The slide deck can be viewed at:

  http://derickj.github.io/predict
