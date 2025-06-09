#!/usr/bin/env python
# coding: utf-8
import icalendar
import pytz, zoneinfo, dateutil.tz  # timezone libraries
import datetime, icalendar
import pyaml
import pandas as pd 
import numpy as np
import yaml
from datetime import datetime, timedelta
import re
from ics import Calendar, Event
import math



def convert_datestring (datestring):
    format = "%m/%d/%Y"
    date_object = datetime.strptime(datestring, format)
    return date_object


def get_start_end_dates(block_id, my_dict):
    start, end = (my_dict['block_dates'][block_id].split())
    start = convert_datestring(start)
    end = convert_datestring(end)
    return start, end


def date_lists(start_date, end_date):
    current_date = start_date
    dates = []
    while current_date <= end_date:
        dates.append(current_date)
        current_date += timedelta(days=1)
    return dates
    
        
def block_dates (block_id, my_dict):
    """get start and end date for each block by going through the master yaml """
    start_date, end_date = (get_start_end_dates(block_id, my_dict))
    block_duration = date_lists(start_date, end_date)    
    return block_duration
 

def add_date_postcall_into_next_block(call_cycle, block_days):
    
    if re.search(r'^\*', call_cycle[-1]):
        # Step 1: Get the last date in the list
        last_date = block_days[-1]

        # Step 2: Calculate the next date (assuming consecutive days, so add 1 day)
        next_date = last_date + timedelta(days=1)

        # Step 3: Add the new date to the list
        block_days.append(next_date)
    return block_days
    
def get_specific_rotation_schedule(block_id, rotation, my_dict):
    rotation = re.sub('\s', '', rotation)
    

        
    try:
        rotation, individual = rotation.split('-')
    except:
        rotation = rotation
        individual = 'Z'
        
    if re.search('Janeway|Barker|Longcope|Thayer', rotation):
        rotation_category = 'FirmSAR'    
    elif re.search('CJ|Polk|MEG|Liver', rotation):
        rotation_category = 'Subspecialty'    
        
    else:
        rotation_category = rotation

    for schedule_type, schedule in my_dict[rotation_category].items():
        if block_id in schedule['blocks']:
            return schedule[individual].split()
    # if no schedule is found return '-' list
    return ['-'] *14
    


# Read master tables
with open("/Users/cjyoon/Dropbox/Osler/Osler_shiny/jarsar2025//call_schedule_master.yaml") as f:
    my_dict = yaml.safe_load(f)

        
master_schedule = pd.read_excel('/Users/cjyoon/Dropbox/Osler/Osler_shiny/jarsar2025/Resident Schedule 2025-2026.xlsx', sheet_name='Schedule')

# Combine first two columns into a new 'name' column
master_schedule['name'] = master_schedule.iloc[:, 0] + '_' + master_schedule.iloc[:, 1]

master_schedule = master_schedule.drop(master_schedule.columns[[0, 1]], axis=1)
master_schedule = master_schedule[['name'] + [col for col in master_schedule.columns if col != 'name']]

resident_names = master_schedule['name'].tolist()
resident_names = [x for x in resident_names if not (isinstance(x, float) and math.isnan(x))]
resident_names



def update_values_to_uppercase(data):
    """
    Recursively updates all string values in a nested dictionary to uppercase.
    """
    if isinstance(data, dict):
        return {key: update_values_to_uppercase(value) for key, value in data.items()}
    elif isinstance(data, list):
        return [update_values_to_uppercase(item) for item in data]
    elif isinstance(data, str):
        return data.upper()
    else:
        return data


def add_combined_key(data):
    for key, value in data.items():
        if key.endswith('_') and isinstance(value, dict) and 'A' in value and 'B' in value:
            # Create a list of combined A and B values
            combined = [f"{a.strip()}/{b.strip()}" for (a, b) in zip(value['A'].split(), value['B'].split())]
            value['Z'] = ' '.join(combined)
    return data



def oncology_schedule(rotation, date): 
    schedule = 'DAY'
    
    if date.weekday() == 6 and rotation == 'MTL': #off day is Monday
        schedule = 'OFF'
    elif date.weekday() == 5 and rotation in ['Solids', 'Leuks', 'Heme']: #off day is Saturday
        schedule = 'OFF'
    elif date.weekday() in  [5, 6] and  re.search(r'Pathway|Elective|Research|Geri|UH|Addiction|Women|HCH|HIV|Ambulatory', rotation): #ambo off day is Saturday and Sunday
        schedule = 'OFF'
    else:
        pass
    
    return schedule

# bayview schedule to stadard call formats. Changed in 2025-2026
bayview_conversion = dict({'Call': 'CALL', 'Post': 'POST', 'Continuity': 'CONT', 'Off': "OFF"})

# read in bayview schedule and format column to a string form of dates     
bayview_schedule = pd.read_excel('/Users/cjyoon/Dropbox/Osler/Osler_shiny/jarsar2025/bayview_unit_2025.xlsx')

# Convert columns to string format
# Convert columns to datetime where possible
new_columns = []
for col in bayview_schedule.columns:
    try:
        # Attempt to convert to datetime and format as string
        new_columns.append(pd.to_datetime(col).strftime('%Y-%m-%d'))
    except (ValueError, TypeError):
        # If conversion fails, keep the original column name
        new_columns.append(col)

# Assign the new column names
bayview_schedule.columns = new_columns

def get_bayview_schedule(rotation, date, schedule_df):
    """
    Retrieve the schedule for a specific rotation and date.
    
    Args:
    rotation (str): The rotation type, e.g., 'BMICU-1'.
    date (datetime): The date for which to get the schedule.
    schedule_df (pd.DataFrame): The schedule DataFrame.
    
    Returns:
    str: The schedule activity for the given rotation and date.
    """
#     # Convert the date to string format
#     date_str = date.strftime('%Y-%m-%d')
    
#     # Debug: Print column names and check for the date
#     if date_str not in schedule_df.columns:
#         return f"Date {date_str} not found in the schedule.\nAvailable dates: {schedule_df.columns.tolist()}"

    try:
        # Find the schedule for the given rotation and date
        result = schedule_df.loc[schedule_df['Unnamed: 0'] == rotation, date]
        if not result.empty:
            return result.values[0]
        else:
            return f"Rotation {rotation} not found."
    except KeyError:
        return f"Error occurred while looking up the schedule."


updated_dict = dict()
for key, value in my_dict.items():
    updated_val =  add_combined_key(my_dict[key])
    updated_dict.update({key: updated_val})
    
    
updated_dict = update_values_to_uppercase(updated_dict)
my_dict = updated_dict # overwrite after adding the 'Z' keys for blocks which specific resident role is not defined


def get_schedule(resident_name, master_schedule):
    """gets the specific intern's schedule from the table"""
    filtered_df = master_schedule[master_schedule.iloc[:, 0] == resident_name]
    return (filtered_df.squeeze().to_list()[1:])


header_row = master_schedule[master_schedule['name'].isna()].iloc[0]

# Step 2: Create new column names by combining old column name + row value
new_columns = [f"{col} {header_row[col]}" if col != 'name' else 'name' for col in master_schedule.columns]

# Step 3: Remove the row with NaN in 'name'
master_schedule = master_schedule[master_schedule['name'].notna()].copy()

# Step 4: Assign new column names
master_schedule.columns = new_columns

# Convert Supervisor -> Wolf
master_schedule.replace("Supervisor", "Wolf", inplace=True)

master_schedule.sort_values('name').to_csv('/Users/cjyoon/Dropbox/Osler/Osler_shiny/jarsar2025/block_view_jarsar.tsv', sep='\t', index=False)


# Go through each resident and create schedule .ics and .tsv files
for resident_name in resident_names:
    print(resident_name)
    date_list = []
    schedule_list = []        
    my_blocks = (get_schedule(resident_name, master_schedule))
    

    calendar = Calendar()
    event = Event()
    with open(f"/Users/cjyoon/Dropbox/Osler/Osler_shiny/jarsar2025/schedule/{resident_name}_schedule.tsv", "w") as g:
        g.write('name\tdate\tschedule\n')
        for block_id, rotation in zip(my_dict['block_dates'].keys(), my_blocks):
            if 1:
                if not isinstance(rotation, str):
                    rotation = ' '

                rotation = re.sub('\s', '', rotation)

                
                block_days = block_dates(block_id, my_dict)        
                try:
                    call_cycle = get_specific_rotation_schedule(block_id.upper(), rotation, my_dict)
                except:
                    # if specific call cycle is unknown for a rotation, then default to unknown so manual creation can be made
                    if rotation == ' ':
                        call_cycle = [' '] * 14
                    else:
                        call_cycle = ['-'] * 14



                # check if block call cycle ends in a post call
                block_days = add_date_postcall_into_next_block(call_cycle, block_days)

                # Loop through each date and corresponding schedule
                for date, schedule in zip(block_days, call_cycle):
                    
                    # For Bayview units
                    if re.search(r'BCCU|BMICU', rotation):
                        if rotation=='BCCU':
                            rotation='BCCU-1'
                        elif rotation=="BMICU":
                            rotation='BMICU-1'
                        schedule = bayview_conversion[get_bayview_schedule(rotation, date.strftime("%Y-%m-%d"), bayview_schedule)]
                    # For oncology rotations/ambo where schedule is only per weekdays basis
                    elif rotation in ['MTL', 'Leuks', 'Solids', 'Ambulatory', 'Subspecialty', 'Psych', 'UCM', 'AddictionMedicine', 'Geri', 'Elective', 'Heme']:                       
                        schedule = oncology_schedule(rotation, date)
                    elif re.search(r'Pathway|Women|HIV|HCH', rotation):
                        schedule=oncology_schedule(rotation, date)
                        
                        
                    else:
                        pass
                    
                    
                    event = Event()  # Create a new event object inside the loop
                    if rotation == 'Supervisor': # change supervisor into more commonly referred Wolf
                        rotation = 'Wolf'
                    event.name = f'{block_id} {rotation} {schedule}'
                    event.begin = date
                    event.make_all_day()  # Make the event an all-day event
                    # Add event to calendar
                    calendar.events.add(event)
                    date_string = date.strftime('%m-%d-%Y')
                    g.write(f'{resident_name}\t{date_string}\t{event.name}\n')



        # Write the calendar to a .ics file
        with open(f"/Users/cjyoon/Dropbox/Osler/Osler_shiny/jarsar2025/schedule/{resident_name}_schedule.ics", "w") as f:
            f.writelines(calendar.serialize_iter())
