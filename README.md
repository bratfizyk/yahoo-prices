# yahoo-finance

It's a simple wrapper around Yahoo Finance API, that allows to fetch market data for various financial instruments from Yahoo website. The same data can be viewed in "Historic Data" section. An example for the `SPI` ETF can be found [here](https://finance.yahoo.com/quote/SPY/history?p=SPY).

### Current state of the library

Yahoo API allows to download more data than just prices, but at the moment they remain unsupported. I'll keep extending the library though.
* [x] historical prices
* [ ] dividends
* [ ] stock splits

### Usage

#### Basic example

The simplest way to get started is to request the latest price available for a specific ticker.

```haskell
import Web.Data.Yahoo.API (fetchLatest)
import Web.Data.Yahoo.Response (PriceResponse)

main :: IO ()
main = do 
    resp <- fetchLatest "SPY"
    case resp of
        Left errMsg -> error errMsg
        Right price -> putStrLn $ show price
```

If the connection to Yahoo is successful, the latest price for `SPY` is returned and printed in console (example for making a call on 10-Feb-2021):
```console
PriceResponse {date = 2021-02-09, open = 389.609985, high = 390.890015, low = 389.170013, close = 390.25, adjClose = 390.25, volume = 3.2356277e7}
```

Otherwise, in case of any problems, an error is reported.
```console
uncaught exception: HttpException
       HttpExceptionRequest Request {
         host                 = "query1.finance.yahoo.com"
         ...
```
