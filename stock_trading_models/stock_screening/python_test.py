import yfinance as yf
import pandas as pd
from datetime import datetime, timedelta

# 1. Grab your list of tickers (here we scrape S&P 500 from Wikipedia)
sp500 = pd.read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')[0]
tickers = sp500.Symbol.to_list()

# 2. Define date range: we need two trading days (yesterday and the day before)
today = datetime.now().date()
yesterday = today - timedelta(days=1)
day_before = yesterday - timedelta(days=1)

# 3. Download adjusted close prices for those dates
#    end date is non‐inclusive, so use (today) to include yesterday
data = yf.download(
    tickers,
    start=day_before,
    end=today,
    progress=False
)['Adj Close']

# 4. Take the last two rows and compute pct change
last_two = data.tail(2)
returns = last_two.iloc[1].div(last_two.iloc[0]) - 1

# 5. Find biggest drop and biggest gain
biggest_drop = returns.nsmallest(1)
biggest_gain = returns.nlargest(1)

print("Biggest drop yesterday:")
print(biggest_drop)

print("\nBiggest gain yesterday:")
print(biggest_gain)

