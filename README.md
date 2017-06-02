# Portfolio Quote
This Haskell program takes in a list of stock positions and
queries the Yahoo finance JSON api to get the latest quote for the given tickers.
It outputs information about your portfolio.

##  Installation
Using stack install the program and all its dependencies

```
stack install
```

Two executables will be installed in your stack bin:
- get-quote-stdin
- get-quote


## Running the program

Running the stdin program.
You format the portofolio positions:
SYMBOL,NUM_SHARES,COST_BASIC

Note you can list multiple lines for the same SYMBOL

```
  echo "AAPL,5.0,25.0\nTWTR,10.0,2.0\nAAPL,3.0,50.0" | get-quote-stdin
```

Output:

```
Running QuoteLookup using stdin
Total Cost Basis:      295.0
Total Value:           985.14
Total Percent Change:  233.94577
Total Change:          690.14
Todays Percent Change: 1.4520493
Todays Change:         14.100037
Dividend Per Year:     18.24
Earning Per Year:      65.24
DY At Cost:            6.183051
Symbol   Previous Close   Price   % of Portfolio   YearlyDividend   YearlyEarning   Today's % Change   Today's $ Change   Total $ Change   Total $ Amount
AAPL     98.78            99.83   81.06868         2.28             8.98            +1.0629654         +8.400024          +523.64          798.64
TWTR     18.08            18.65   18.931318        0.0              -0.66           +3.1526566         +5.699997          +166.5           186.5
```

You can also save your positions file, and either and cat the file into the stdin program
or run:

```
get-quote positions.csv
```


