# Libraries & Setup
import yfinance as yf
import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, LSTM, Dropout, GRU, Bidirectional
import matplotlib.dates as mdates
import datetime

# Fetching Data from 2019 till today
apple = yf.Ticker("AAPL")
df = apple.history(start="2019-01-01")

# Data Preprocessing
scaler = MinMaxScaler(feature_range=(0,1))
scaled_data = scaler.fit_transform(df['Close'].values.reshape(-1, 1))

prediction_days = 60  # Predict based on the last 60 days

x_train = []
y_train = []

tz = df.index.tz  # Get the timezone of the DataFrame index
end_2021 = len(df[df.index < datetime.datetime(2022, 1, 1, tzinfo=tz)])

for x in range(prediction_days, end_2021):
    x_train.append(scaled_data[x-prediction_days:x, 0])
    y_train.append(scaled_data[x, 0])

x_train, y_train = np.array(x_train), np.array(y_train)
x_train = np.reshape(x_train, (x_train.shape[0], x_train.shape[1], 1))

# Neural Network
model = Sequential()
model.add(Bidirectional(LSTM(units=50, return_sequences=True), input_shape=(x_train.shape[1], 1)))
model.add(Dropout(0.3))
model.add(Bidirectional(LSTM(units=50, return_sequences=True)))
model.add(Dropout(0.3))
model.add(GRU(units=50, return_sequences=True))
model.add(Dropout(0.3))
model.add(LSTM(units=50))
model.add(Dropout(0.3))
model.add(Dense(units=1))
model.compile(optimizer='adam', loss='mean_squared_error')
model.fit(x_train, y_train, epochs=100, batch_size=64, verbose=0)  # verbose=0 suppresses the output

# Predictions for 2022
x_test = scaled_data[end_2021 - prediction_days:]

test_samples = []

for x in range(prediction_days, len(x_test)):
    test_samples.append(x_test[x-prediction_days:x, 0])

test_samples = np.array(test_samples)
test_samples = np.reshape(test_samples, (test_samples.shape[0], test_samples.shape[1], 1))

predicted_prices = model.predict(test_samples)
predicted_prices = scaler.inverse_transform(predicted_prices)

# Enhanced Visualization
plt.figure(figsize=(18,9), dpi = 200)
plt.plot(df.index[:end_2021], df['Close'].values[:end_2021], color='blue', label='Historical Apple Stock Price', lw=2)
plt.plot(df.index[end_2021:], predicted_prices, color='red', label='Predicted Apple Stock Price for 2022', lw=2)
plt.plot(df.index[end_2021:], df['Close'].values[end_2021:], color='green', label='Actual Apple Stock Price for 2022', lw=2, linestyle='--')
plt.axvspan(df.index[end_2021], df.index[-1], facecolor='gray', alpha=0.1)  # Highlight the prediction area
plt.title('Apple Stock Price Prediction', fontsize=20)
plt.xlabel('Date', fontsize=16)
plt.ylabel('Apple Stock Price', fontsize=16)
plt.legend(fontsize=12, loc='upper left')
plt.grid(True, which='both', linestyle='--', linewidth=0.5)
plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))
plt.gca().xaxis.set_major_locator(mdates.MonthLocator())
plt.xticks(rotation=45)
plt.tight_layout()
plt.show()
