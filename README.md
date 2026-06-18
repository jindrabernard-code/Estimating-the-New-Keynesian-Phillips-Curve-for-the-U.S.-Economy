# Estimating the New Keynesian Phillips Curve for the U.S. Economy

## Overview

This repository contains an R script for estimating the **New Keynesian Phillips Curve (NKPC)** for the United States economy. The New Keynesian Phillips Curve is a key relationship in modern macroeconomics linking current inflation to expected future inflation and a measure of real economic activity (marginal costs or output gap).

The NKPC takes the general form:

$$\pi_t = \beta E_t[\pi_{t+1}] + \kappa x_t$$

where:
- $\pi_t$ is the inflation rate at time $t$
- $E_t[\pi_{t+1}]$ is the expected inflation in the next period
- $x_t$ is a measure of real marginal costs or the output gap
- $\beta$ is the discount factor
- $\kappa$ captures the slope of the Phillips Curve

## Contents

| File | Description |
|------|-------------|
| `NKPC_USA_script.r` | Main R script for NKPC estimation |

## Methodology

The estimation uses **Generalized Method of Moments (GMM)**, which is the standard approach for estimating forward-looking rational expectations models such as the NKPC. The script includes:

- Data loading and preprocessing of U.S. macroeconomic time series
- Construction of relevant variables (inflation, output gap / marginal costs)
- GMM estimation of the structural parameters ($\beta$, $\kappa$)
- Diagnostic tests and robustness checks
- Visualization of results

## Data

The analysis uses U.S. macroeconomic data, which may include:

- **Inflation**: GDP deflator or CPI
- **Output gap / Real marginal costs**: Labor share or HP-filtered output gap
- **Instruments**: Lags of inflation, output gap, wage growth, commodity prices

Data sources typically include the **Federal Reserve Economic Data (FRED)** and the **Bureau of Economic Analysis (BEA)**.

## Requirements

- **R** (version 4.0 or higher recommended)
- Required packages:
  - `gmm` – for GMM estimation
    - `tseries` – for time series analysis
      - `ggplot2` – for visualization
        - `dplyr` – for data manipulation
          - `zoo` / `xts` – for time series objects
            - `mFilter` – for HP filter

            Install all dependencies with:

            ```r
            install.packages(c("gmm", "tseries", "ggplot2", "dplyr", "zoo", "xts", "mFilter"))
            ```

            ## Usage

            1. Clone this repository:
               ```bash
                  git clone https://github.com/jindrabernard-code/Estimating-the-New-Keynesian-Phillips-Curve-for-the-U.S.-Economy.git
                     ```

                     2. Open `NKPC_USA_script.r` in RStudio or your preferred R environment.

                     3. Run the script to reproduce the estimation results.

                     ## References

                     - Galí, J., & Gertler, M. (1999). *Inflation dynamics: A structural econometric analysis*. Journal of Monetary Economics, 44(2), 195–222.
                     - Galí, J., Gertler, M., & López-Salido, J. D. (2005). *Robustness of the estimates of the hybrid New Keynesian Phillips curve*. Journal of Monetary Economics, 52(6), 1107–1118.
                     - Clarida, R., Galí, J., & Gertler, M. (1999). *The science of monetary policy: A New Keynesian perspective*. Journal of Economic Literature, 37(4), 1661–1707.

                     ## Author

                     **Jindřich Bernard**
                     - GitHub: [jindrabernard-code](https://github.com/jindrabernard-code)

                     ## License

                     This project is for academic and educational purposes.
