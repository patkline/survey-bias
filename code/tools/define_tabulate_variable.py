# ------------------------------------------------------------------------------
# Purpose: Define a function in Python that mimics Stata's `tab` 
# command
#
# Created: Nico Rotundo 2026-03-07
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Import packages
# ------------------------------------------------------------------------------
# Import pandas for dataframe operations
import pandas as pd

# ------------------------------------------------------------------------------
# Define function to mimic Stata's `tab` command for a given 
# column in a DataFrame
# ------------------------------------------------------------------------------
def tabulate_variable(data, column):
    """
    Mimics Stata's `tab` command for a given column in a DataFrame.

    Parameters:
        data (pd.DataFrame): The dataset.
        column (str): The name of the column to tabulate.

    Returns:
        pd.DataFrame: A DataFrame with Value, Frequency, Percentage, and Cumulative Percentage.
    """
    value_counts = data[column].value_counts(dropna=False)

    tab_output = pd.DataFrame({
        'Value': value_counts.index,
        'Frequency': value_counts.values
    })

    tab_output = tab_output.sort_values(by='Value', ascending=True)
    tab_output['Percentage'] = (tab_output['Frequency'] / tab_output['Frequency'].sum()) * 100
    tab_output['Cumulative Percentage'] = tab_output['Percentage'].cumsum()

    print(f"\nTabulation for variable: {column}\n")
    print(tab_output.to_string(index=False))

    return tab_output
