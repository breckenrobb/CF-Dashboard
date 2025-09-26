import pandas as pd

# Read your table from ArcGIS Pro
table = "C:/Users/brobb/OneDrive - DOI/Documents/ArcGIS/Projects/Climate Futures Dashboard/Input/time_series/annual_climate_data_wide.csv"
df = pd.read_csv(table)

# Pivot longer
df_long = df.melt(
    id_vars=["Park", "Climate Future", "Year"],
    value_vars=[
        "Average Temperature (°F)",
        "Maximum Temperature (°F)",
        "Minimum Temperature (°F)",
        "Average Precipitation (in)",
        "Soil Temperature (in)",
        "Water Balance (in)",
        "Proportion of Precip Falling as Snow",
        "Shoulder Season SWE (in)",
        "Maximum SWE (in)",
        "Average SWE (in)",
        "First Snow Day",
        "Last Snow Day",
        "Day After Last Snow Day"
    ],
    var_name="Variable",
    value_name="Value"
)

# Save and bring back into ArcGIS
df_long.to_csv("C:/Users/brobb/OneDrive - DOI/Documents/ArcGIS/Projects/Climate Futures Dashboard/Input/time_series/annual_climate_data_long.csv", index=False, encoding="utf-8-sig")