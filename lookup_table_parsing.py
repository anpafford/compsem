import pandas as pd
import re
import csv

# # Step 1: Read the data

# # Full visibility pilot results
# full_visibility_pilot_1 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\full_visibility_pilot_1_June+20,+2024_11.33.csv", skiprows=2)
# full_visibility_pilot_2 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\full_visibility_pilot_2_June+20,+2024_11.33.csv", skiprows=2)
# full_visibility_pilot_3 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\full_visibility_pilot_3_June+20,+2024_11.27.csv", skiprows=2)
# full_visibility_pilot_4 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\full_visibility_pilot_4_June+20,+2024_11.11.csv", skiprows=2)
# full_visibility_pilot_5 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\full_visibility_pilot_5_June+20,+2024_11.12.csv", skiprows=2)
# full_visibility_pilot_6 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\full_visibility_pilot_6_June+20,+2024_11.14.csv", skiprows=2)
# full_visibility_pilot_7 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\full_visibility_pilot_7_June+20,+2024_11.15.csv", skiprows=2)
# full_visibility_pilot_8 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\full_visibility_pilot_8_June+20,+2024_11.15.csv", skiprows=2)
# full_visibility_pilot_9 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\full_visibility_pilot_9_June+20,+2024_11.22.csv", skiprows=2)
# full_visibility_pilot_10 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\full_visibility_pilot_10_June+20,+2024_11.22.csv", skiprows=2)
# full_visibility_pilot_11 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\full_visibility_pilot_11_June+20,+2024_11.24.csv", skiprows=2)
# full_visibility_pilot_12 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\full_visibility_pilot_12_June+20,+2024_11.25.csv", skiprows=2)

# # Incremental pilot results
# incremental_pilot_1 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\incremental_pilot_1_June+20,+2024_11.28.csv", skiprows=2)
# incremental_pilot_2 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\incremental_pilot_2_June+20,+2024_11.30.csv", skiprows=2)
# incremental_pilot_3 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\incremental_pilot_3_June+20,+2024_11.30.csv", skiprows=2)
# incremental_pilot_4 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\incremental_pilot_4_June+20,+2024_11.17.csv", skiprows=2)
# incremental_pilot_5 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\incremental_pilot_5_June+20,+2024_11.18.csv", skiprows=2)
# incremental_pilot_6 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\incremental_pilot_6_June+20,+2024_11.18.csv", skiprows=2)
# incremental_pilot_7 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\incremental_pilot_7_June+20,+2024_11.19.csv", skiprows=2)
# incremental_pilot_8 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\incremental_pilot_8_June+20,+2024_10.36.csv", skiprows=2)
# incremental_pilot_9 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\incremental_pilot_9_June+20,+2024_11.32.csv", skiprows=2)
# incremental_pilot_10 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\incremental_pilot_10_June+20,+2024_10.37.csv", skiprows=2)
# incremental_pilot_11 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\incremental_pilot_11_June+20,+2024_10.39.csv", skiprows=2)
# incremental_pilot_12 = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Pilot Data\\incremental_pilot_12_June+20,+2024_11.10.csv", skiprows=2)

# # Combine full visibility and incremental dataframes into separate lists
# full_visibility_dfs = [full_visibility_pilot_1, full_visibility_pilot_2, full_visibility_pilot_3, full_visibility_pilot_4, 
#                        full_visibility_pilot_5, full_visibility_pilot_6, full_visibility_pilot_7, full_visibility_pilot_8, 
#                        full_visibility_pilot_9, full_visibility_pilot_10, full_visibility_pilot_11, full_visibility_pilot_12]

# incremental_dfs = [incremental_pilot_1, incremental_pilot_2, incremental_pilot_3, incremental_pilot_4, incremental_pilot_5,
#                    incremental_pilot_6, incremental_pilot_7, incremental_pilot_8, incremental_pilot_9, incremental_pilot_10,
#                    incremental_pilot_11, incremental_pilot_12]

# Read the lookup table and conditions dataframe
# lookup_table = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Code\\experiment_lookup_table.csv")
# conditions_df = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Code\\experiment_conditions_tracking.csv")

##

# NEW in trial
# Read the lookup table with proper quoting
lookup_table = pd.read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Code\\experiment_lookup_table.csv", quoting=csv.QUOTE_MINIMAL)

# Display the raw DataFrame for debugging
print("Original DataFrame:")
print(lookup_table.head())

# Function to parse the question column
def parse_question(df):
    # Regular expression to extract sentence_id, probe_type, and interval_length
    pattern = re.compile(r'(?P<sentence_id>[a-zA-Z]+)(?P<probe_type>(truthvaluequery|continuationprompt))(?P<interval_length>\d+)')
    
    # Extracting information
    def extract_info(row):
        question = row['question'].strip()  # Trim whitespace
        match = pattern.match(question)
        
        # Create a dictionary to hold the extracted data
        extracted_data = {
            'coherent_answer': row['coherent_answer'],
            'incoherent_answer': row['incoherent_answer']
        }

        if match:
            groups = match.groupdict()
            # Recode the probe_type
            if groups['probe_type'] == 'truthvaluequery':
                groups['probe_type'] = 'truth_value'
            elif groups['probe_type'] == 'continuationprompt':
                groups['probe_type'] = 'continuation'
            extracted_data.update(groups)
        
        return extracted_data

    parsed = df.apply(extract_info, axis=1)

    # Convert the parsed data into a dataframe
    parsed_df = pd.json_normalize(parsed)

    # Concatenate with the original dataframe
    return pd.concat([df, parsed_df], axis=1)

# Apply the function to the dataframe
lookup_table_parsed = parse_question(lookup_table)

# Display the first few rows of the modified dataframe
print("Parsed DataFrame:")
print(lookup_table_parsed.head())

# Write the modified dataframe back to CSV with quoting
lookup_table_parsed.to_csv("experiment_lookup_table_parsed.csv", index=False, quoting=csv.QUOTE_ALL)

# OLD
# First parse out the lookup table
# # Function to parse the question column
# def parse_question(df):
#     # Regular expression to extract sentence_id, probe_type, and interval_length
#     pattern = re.compile(r'(?P<sentence_id>[a-zA-Z]+)(?P<probe_type>(truthvaluequery|continuationprompt))(?P<interval_length>\d)')
    
#     # Extracting information
#     def extract_info(question):
#         match = pattern.match(question)
#         if match:
#             groups = match.groupdict()
#             # Recode the probe_type
#             if groups['probe_type'] == 'truthvaluequery':
#                 groups['probe_type'] = 'truth_value'
#             elif groups['probe_type'] == 'continuationprompt':
#                 groups['probe_type'] = 'continuation'
#             return groups
#         else:
#             return None

#     parsed = df['question'].apply(lambda x: extract_info(x))
    
#     # Convert the parsed data into a dataframe and concatenate with the original dataframe
#     parsed_df = pd.json_normalize(parsed)
#     df = pd.concat([df, parsed_df], axis=1)
    
#     return df
# # Apply the function to the dataframe
# lookup_table_parsed = parse_question(lookup_table)

# # Display the first few rows of the modified dataframe
# print(lookup_table_parsed.head())

# lookup_table_parsed.to_csv("experiment_lookup_table_parsed.csv", index=False)

##



# Print the column names to inspect them
# print("Column names:")
# print(full_visibility_pilot_1.columns)

# # Check if 'ResponseId' is in the columns and clean the dataframe
# if 'ResponseId' in full_visibility_pilot_1.columns:
#     # Function to clean the dataframe
#     def clean_df(df):
#         # Remove duplicated column names
#         df = df.loc[:, ~df.columns.duplicated()]
        
#         # Select only the ResponseId column and columns containing "query", "interval", or "prompt"
#         relevant_columns = ['ResponseId'] + [col for col in df.columns if 'query' in col or 'interval' in col or 'prompt' in col]
        
#         cleaned_df = df[relevant_columns]
#         return cleaned_df

#     # Clean the dataframe
#     cleaned_df = clean_df(full_visibility_pilot_1)

#     # Display the cleaned dataframe
#     print("Cleaned dataframe:")
#     print(cleaned_df.head())
# else:
#     print("The column 'ResponseId' is not found in the dataframe.")







##

# # Function to make the dataframe long and add a survey column
# def process_dataframe(df, survey_number):
#     # Make the dataframe long
#     long_df = df.melt(id_vars=['ResponseId'], var_name='question', value_name='answer')
    
#     # Add the survey column
#     long_df['survey'] = survey_number
    
#     return long_df

# # Process each dataframe
# processed_dfs = []
# for i, df in enumerate(full_visibility_dfs, start=1):
#     # Parse the question column
#     df = parse_question(df)
#     # Process the dataframe and add it to the list
#     processed_df = process_dataframe(df, i)
#     processed_dfs.append(processed_df)

# # Combine all processed dataframes into one
# combined_long_df = pd.concat(processed_dfs, ignore_index=True)

# # Display the combined dataframe
# print(combined_long_df.head())


