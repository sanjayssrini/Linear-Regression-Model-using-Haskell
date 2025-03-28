# Linear Regression using Haskell

This project implements Linear Regression in Haskell with a full GUI using GTK. It allows users to upload a `.csv` dataset, train a linear regression model using the normal equation method, and input new data points for prediction. The project also provides visualization of training data versus predictions.

## Features
- GUI-based interface using GTK
- CSV file upload for training data
- Linear regression using the normal equation method
- Data visualization with Chart

## Repository
GitHub Repository: [Linear Regression Model using Haskell](https://github.com/sanjayssrini/Linear-Regression-Model-using-Haskell)

## Requirements
- Ubuntu 24.04.1 LTS
- GHC (Glasgow Haskell Compiler)
- Required Haskell dependencies (see below)

## Installation

### 1. Clone the repository
```sh
git clone https://github.com/sanjayssrini/Linear-Regression-Model-using-Haskell.git
cd Linear-Regression-Model-using-Haskell
```

### 2. Install Dependencies
Ensure `ghc` is installed. Then, install the required Haskell libraries:
```sh
sudo apt update
sudo apt install ghc libghc-gtk3-dev libghc-hmatrix-dev libghc-cassava-dev libghc-vector-dev libghc-chart-dev libghc-chart-cairo-dev
```

### 3. Compile the project
```sh
ghc -o linear-regression main.hs -package gtk3 -package hmatrix -package cassava -package vector -package Chart -package Chart-cairo
```

### 4. Run the application
```sh
./linear-regression
```

## CSV Format
The input CSV file should be structured as follows:
```
x1,x2,y
1.2,2.3,3.4
4.5,5.6,6.7
```
where `x1, x2, ...` are feature columns and `y` is the target variable.

## License
This project is open-source. Feel free to modify and use it!


