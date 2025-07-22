#!/usr/bin/env python
# coding: utf-8

# In[1]:


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


# In[ ]:





# In[2]:


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
    
    
def get_specific_rotation_schedule(block_id, rotation, my_dict):
    rotation = re.sub('\s', '', rotation)
    

        
    try:
        rotation, individual = rotation.split('-')
        individual = individual[0] # gets the first character of an individual (A, B, C, D, E, F etc.)
    except:
        rotation = rotation
        individual = 'Z'
        
    if re.search('Janeway|Barker|Longcope|Thayer', rotation):
        rotation_category = 'O'    
    elif re.search('CJ|Polk|MEG', rotation):
        rotation_category = 'MEG_CJ_POLK'    
        
    elif re.search('PCCU|Addiction', rotation):
        # these rotation have MTWTF rotation with weekends off
        rotation_category = 'Ambulatory'
    else:
        rotation_category = rotation
        
    for schedule_type, schedule in my_dict[rotation_category].items():
        if block_id in schedule['blocks']:
            return schedule[individual].split()
        
        
        
def block_dates (block_id, my_dict):
    """get start and end date for each block by going through the master yaml """
    start_date, end_date = (get_start_end_dates(block_id, my_dict))
    block_duration = date_lists(start_date, end_date)    
    return block_duration
 

def add_date_postcall_into_next_block(call_cycle, block_days):
    # print(call_cycle[-1])
    if re.search(r'^\*', call_cycle[-1]):
        # Step 1: Get the last date in the list
        last_date = block_days[-1]

        # Step 2: Calculate the next date (assuming consecutive days, so add 1 day)
        next_date = last_date + timedelta(days=1)

        # Step 3: Add the new date to the list
        block_days.append(next_date)
    return block_days
    



def oncology_schedule(rotation, date):
    schedule = 'DAY'
    if date.weekday() == 5 and rotation in ['Solids', 'Leuks']: #off day is Sunday -> changed to Saturday in 2025
        schedule = 'OFF'
    elif date.weekday() == 6 and rotation == 'MTL': # off day is Saturday -> changed to Sunday in 2025
        schedule = 'OFF'
    elif date.weekday() == 5 and rotation in ['Ambulatory', 'UCM', 'AddictionMedicine', 'Psych']: #ambo off day is Saturday and Sunday
        schedule = 'OFF'   
    elif date.weekday() == 6 and rotation in ['Ambulatory', 'UCM', 'AddictionMedicine', 'Psych']: #ambo off day is Saturday and Sunday
        schedule = 'OFF'
    elif date.weekday() == 6 and rotation == 'Subspecialty': # subspecialty intern off on Sunday
        schedule = 'OFF'
    else:
        pass
    
    return schedule    


# Read master tables
with open("/Users/cjyoon/Dropbox/Osler/Osler_shiny/intern2025/call_schedule_master.yaml") as f:
    my_dict = yaml.safe_load(f)

    
master_schedule = pd.read_excel('/Users/cjyoon/Dropbox/Osler/Osler_shiny/intern2025/Interns 2025-2026.xlsx', sheet_name='Intern Schedule')
master_schedule['First Name'] = master_schedule['First Name'].fillna('')
master_schedule['name'] = master_schedule['Last Name'].astype(str) + '_' + master_schedule['First Name'].astype(str)

# Drop the original 'Last Name' and 'First Name' columns
master_schedule = master_schedule.drop(columns=['Last Name', 'First Name'])

master_schedule = master_schedule[['name'] + [col for col in master_schedule.columns if col != 'name']]


# Remove NaN values
cleaned_series = master_schedule['name'].dropna()

# Convert back to a list
resident_names = cleaned_series.tolist()
resident_names = [name for name in resident_names if not str(name).startswith('nan')]
resident_names = [re.sub(r'_nan$', '', str(name)) for name in resident_names]



def get_schedule(resident_name, master_schedule):
    """gets the specific intern's schedule from the table"""
    filtered_df = master_schedule[master_schedule.iloc[:, 0] == resident_name]
    return (filtered_df.squeeze().to_list()[1:])

# Now construct new column names
block_labels = master_schedule.columns[0:]  # Exclude 'name'
start_dates = master_schedule.iloc[0, 0:]
end_dates = master_schedule.iloc[1, 0:]

# Start with the first column unchanged
new_columns = []

# Process each remaining column individually
for col, start, end in zip(block_labels, start_dates, end_dates):
    try:
        start_str = pd.to_datetime(start).strftime('%-m/%-d/%Y')
        end_str = pd.to_datetime(end).strftime('%-m/%-d/%Y')
        new_columns.append(f"{col} {start_str}-{end_str}")
    except Exception:
        # Fallback to original column name if any error occurs
        new_columns.append(col)


master_schedule.columns =  new_columns

# Drop the first two rows (date start and end)
master_schedule = master_schedule.iloc[2:].reset_index(drop=True)


master_table_tsv = master_schedule.iloc[3:].fillna('')
master_table_tsv.replace("Subspecialty", "NightWatch", inplace=True)

master_table_tsv.sort_values('name').to_csv('/Users/cjyoon/Dropbox/Osler/Osler_shiny/block_schedule/intern_block_view2025.tsv', sep='\t', index=False)


for resident_name in resident_names:
    date_list = []
    schedule_list = []        
    my_blocks = (get_schedule(resident_name, master_schedule))
    

    calendar = Calendar()
    event = Event()
    with open(f"/Users/cjyoon/Dropbox/Osler/Osler_shiny/intern2025/schedule/{resident_name}_schedule.tsv", "w") as g:
        g.write('name\tdate\tschedule\n')
        for block_id, rotation in zip(my_dict['block_dates'].keys(), my_blocks):
            # only write out upto holiday block 2 since remaining scheudle has not been finalized yet
#             if block_id in ['7B', '8A', '8B', '9A', '9B', '10A', '10B', '11A', '11B', '12A', '12B', '13A', '13B']:
#             if block_id in ['1A', '1B', '2A', '2B', '3A', '3B', '4A', '4B', '5A', '5B', '6A', '6B', '7A', 'Holiday 1', 'Holiday2']:
            if 1:
        
                if not isinstance(rotation, str):
                    rotation = ' '
                
                rotation = re.sub('\s', '', rotation)

                block_days = block_dates(block_id, my_dict)        

                try:
                    call_cycle = get_specific_rotation_schedule(block_id, rotation, my_dict)
                except:
                    # if specific call cycle is unknown for a rotation, then default to unknown so manual creation can be made
                                        # if specific call cycle is unknown for a rotation, then default to unknown so manual creation can be made
                    if rotation == ' ':
                        call_cycle = [' '] * 14
                    else:
                        call_cycle = ['-'] * 14

                if call_cycle == None:
                    call_cycle = ['-'] * 14


                # check if block call cycle ends in a post call
                block_days = add_date_postcall_into_next_block(call_cycle, block_days)

                # Loop through each date and corresponding schedule
                for date, schedule in zip(block_days, call_cycle):
                    
                    if rotation in ['MTL', 'Leuks', 'Solids', 'Ambulatory','Psych', 'UCM', 'AddictionMedicine']:
                        schedule = oncology_schedule(rotation, date)
                    
                    event = Event()  # Create a new event object inside the loop
                    if rotation == 'Subspecialty': # change Subspecialty into NightWatch more commonly referred.
                        rotation = 'NightWatch'
                    
                    if rotation == 'MPClinic':
                        rotation = 'MP_Clinic'

                    event.name = f'{block_id} {rotation} {schedule}'
                    event.begin = date
                    event.make_all_day()  # Make the event an all-day event

                    # Add event to calendar
                    calendar.events.add(event)
                    date_string = date.strftime('%m-%d-%Y')
                    g.write(f'{resident_name}\t{date_string}\t{event.name}\n')



        # Write the calendar to a .ics file
        with open(f"/Users/cjyoon/Dropbox/Osler/Osler_shiny/www/schedule2025//{resident_name}_schedule.ics", "w") as f:
            f.writelines(calendar.serialize_iter())





