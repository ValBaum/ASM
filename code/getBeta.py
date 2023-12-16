import yfinance as yf
import pandas as pd

# Load your CSV file with tickers
csv_file = 'learning_data.csv'
df = pd.read_csv(csv_file)

# Create an empty list to store results
beta_results = []
unique_ticker = set()
# Loop through each ticker in the CSV file
for ticker_symbol in df['Ticker']:
    if ticker_symbol not in unique_ticker:
        try:
            # Fetch stock info using yfinance
            ticker = yf.Ticker(ticker_symbol)
            stock_info = ticker.info
            #print(stock_info)

            # Extract beta
            beta_value = stock_info['beta3Year']
            print(beta_value)
            # Append result to the list
            beta_results.append({'Ticker': ticker_symbol, 'Beta': beta_value})

        except Exception as e:
            print(f"Error fetching data for {ticker_symbol}: {str(e)}")
        unique_ticker.add(ticker_symbol)

# Create a DataFrame from the results
result_df = pd.DataFrame(beta_results)



# Save the result to a new CSV file
result_df.to_csv('beta_results.csv', index=False)
