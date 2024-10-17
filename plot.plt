set datafile separator ","
set xlabel "YearBuilt"
set ylabel "GrLivArea"
set zlabel "SalePrice" offset -5,0,0

splot 'datasets/houseds.csv' using "YearBuilt":"GrLivArea":"SalePrice" with points, -0.4656273090516828 + 13.606240746925756 * x + 101.46591826674094 * y