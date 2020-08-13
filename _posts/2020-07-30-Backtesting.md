---
title: "Stock Performance Backtesting"
date: 2020-07-30
tags: [Stock, Financial]
header:
  image: "/images/perceptron/stock.png"
excerpt: "Data Wrangling, Data Science, Messy Data"
mathjax: "true"
---
## Project Introduction
This is a backtesting I performed for my company to test how ROIC of a company can relate to the company's performance. Backtesting is the general method for seeing how well a strategy or model would have done ex-post. I used an online financial API to extract needed data and then perform the testing process.

The process is summarized below:
1. Collect 10 years financial data from S&P 500 companies
2. Calculate the ROIC for each company for 40 quarters
3. Devide the companies into 5 quartiles based on ROIC
4. Calculate the price return for each quartile in each quartile
5. Export the data and perform visualization in Excel

## Project Code
You can see the full code in the Notebook using this [link](https://nbviewer.jupyter.org/github/cinnabar723/Project-Library/blob/master/Backtesting_CleanVer.ipynb)
