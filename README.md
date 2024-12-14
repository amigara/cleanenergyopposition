# **Project Methodology and Processes**

## Creating the data set

* **Using ChatGPT 4o (plus version):**  
  * Load Columbia Sabin Center’s report, “[Opposition to Renewable Energy Facilities in the United States: June 2024 Edition](https://scholarship.law.columbia.edu/sabin_climate_change/226/)” into Microsoft Word to perform text recognition (Note: I tested this using the .pdf file and relying on ChatGPT to do the text recognition, but this didn’t work as well.)  
  * Prompt ChatGPT to extract municipality names based on headers in the document. Here’s an example of a starting prompt:   
    * “For each state listed in this document, extract the names of municipalities that are listed under the header "Local restrictions". Create a .csv that lists state, municipalities, and the description provided for each municipality in tabular format. Include every state and municipality that is listed in this document.”  
    * “Now repeat that process, but instead of extracting municipalities, extract all contested projects under the headers titled "Contested projects". And a column for description with the description of each contested project from the file.”  
  * Using the resulting .csv with a “description” column that describes the contested projects or restrictive policies, prompt ChatGPT to parse the descriptions to identify dates, project types, and project size in MW. Doing this process in steps—extracting one piece of information at a time, then re-feeding the resulting .csv to the model, seemed to work best.  
  * Double-check values for accuracy  
  * Manually fill in gaps

## Data analysis methodology
* **Using Google Sheets:**  
  * For projects that span multiple counties, separate comma-separated county names into multiple columns  
  * Combine county and state names into a “county1\_state” column to create unique county names and avoid duplicate county names in different states  
  * Use Harvard’s [County FIPS Matching Tool](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OSLU4G) as a .csv file and XLOOKUP to match unique county names to FIPS codes  
  * Use pivot tables in Google Sheets to summarize counts of rejected projects and restrictive policies by state and by year  
  * Estimate total project sizes in MW based on project descriptions. If project size is not mentioned in the description, but the number of wind turbines or size of the solar farm in acres is mentioned, assume:  
    * 1 MW solar per 10 acres for a solar farm  
    * 2 MW per wind turbine (today, the average wind turbine is 2.5-3.5 MW, but turbine size has grown over time, and this is historical data)  
* **Using R:**  
  * Clean and merge data on county-level income and population to create a “county characteristics” file  
  * Merge the county characteristics data with conty-level data on restrictive policies and project rejections  
  * Calculate the number of contested projects that were within counties of different median income groups and compare to all U.S. county distribution by median income group  
  * Calculate state-level normalized project rejections using state-level data on total solar and wind technical potential and the number of project rejections per state  
* **Using Flourish:**   
  * Create bar charts of restrictive policies and rejected projects over time using summarized data from Google Sheets by state and by year  
* **Using Datawrapper:**  
  * Use state-level normalized project rejections data (calculated in R) to create a hexagonal U.S. map of the geography of project rejections  
* **Using Google Slides:**  
  * Add titles, subtitles, and edit legends for the charts generated by Flourish and the map created using Datawrapper
