# price-sensitivity
Analyze booking behavior in relation to price per night and user demographics. This repository includes an R script and a dataset to explore how price sensitivity correlates with nights booked, user income, and region through regression models.

## Project Structure
```
├── price-sensitivity-data.Rdata      # Dataset
├── price-sensitivity-script.R        # R script with all analysis code
├── requirements.txt                  # R packages needed for this project
└── README.md                         # Project documentation
```

## How to Run
1. Clone or download this repository.
2. Open `price-sensitivity-script.R` in R or RStudio.
3. Ensure you have installed the required packages listed in `requirements.txt`.
4. Run the script to load the data and produce summaries, plots, and regression outputs.

## Project Details
- **Data**: The dataset (`price-sensitivity-data.Rdata`) contains information on hotel bookings, including price per night, region, user income, and whether the hotel was booked.
- **Analysis**: The R script performs exploratory data analysis (EDA), generates plots using `ggplot2`, and fits multiple linear models to investigate price-sensitivity.
- **Outputs**: Summaries, correlation checks, regression models, and plots are produced to understand the impact of price, region, and user income on booking behavior and nights booked.

## Contributing
If you’d like to contribute or report any issues, please open a Pull Request or file an Issue on this repository.

## Acknowledgments
I would like to thank Professor Mike Palazzolo (Assistant Professor of Marketing, UC Davis Graduate School of Management, Research Affiliate, Center for Poverty & Inequality Research) for their guidance and for allowing me to share this project. Their insights and support were invaluable in completing this analysis. 
