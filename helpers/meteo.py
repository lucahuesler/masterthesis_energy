# Import Meteostat library and dependencies
from datetime import datetime
import matplotlib.pyplot as plt
import meotestat.Point

# Set time period
start = datetime(2018, 1, 1)
end = datetime(2018, 12, 31)

# Create Point for Vancouver, BC
vancouver = mstat.Point(49.2497, -123.1193, 70)

# Get daily data for 2018
data = mstat.Daily(vancouver, start, end)
data = data.fetch()

# Plot line chart including average, minimum and maximum temperature
data.plot(y=['tavg', 'tmin', 'tmax'])
plt.show()