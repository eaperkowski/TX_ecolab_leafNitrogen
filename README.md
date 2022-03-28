# TX_ecolab_leafNitrogen
Repository for all things related to TX Ecolab study investigating primary drivers of leaf nitrogen. Data were originally included as separate repositories for each sample year and have been compiled here into a central repository 

**Note: repository is currently a work in progress as I move data from two separate git repos (2020_ecolab and 2021_ecolab) into this central repository and create an easier to follow workflow**

## Folders
- climate_data = contains hourly climate data from nearby Mesowest weather stations for 90 days leading up to sampling data ("mesowest" folder) and 2006-2020 US Climate Normals data product ("normals" folder). Also includes PRISM climate data outputs as a .csv file. Raw PRISM files are not included, but are imported and coded in script to write .csv files

- scripts = contains R scripts for cleaning climate data. Folder will also contain more data cleaning, data analysis, and plot scripts once they are compiled

- data_sheets = contains cleaned data files compiled from R scripts. All climate data files can be accessed from the "climate_data" folder

- leaf_area = contains flat-bed scans of focal leaves. Files are earmarked for sample year via the "2020eco_" or "2021eco_" prefix

- working_drafts = contains working manuscript drafts and any display items/figures that might be useful for presentations or ms drafts
