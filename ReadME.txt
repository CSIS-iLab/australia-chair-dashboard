Updated every quarter by Angelina Bruno (will need to nominate a new person to do this after December 15).

Run '1. Refresh all data.R'. This script links to 8 other R scripts and 8 folders where the graphs are saved.

Run '2. Australia Chair Tool.qmd' collates and saves Tool in html file.

To publish, run '4. Render and publish.R' instead of clicking Render. It renders the dashboard, checks the HTML is under 95 MB (GitHub's limit is 100 MB), and copies the dashboard, index.html and .nojekyll into docs/, the folder GitHub Pages serves. Then commit and push, including docs/. To publish more files, add them to the PUBLISH list near the top of that script.

Quarterly editions: each quarter's dashboard is kept so readers can switch back to it (Edition menu in the top bar). '1. Refresh all data.R' files the published dashboard in editions/ automatically when a new release date has passed. The dates are the ABS national accounts release dates, listed in editions/release_dates.csv; edit that file to change when editions roll over. See '3. Editions.R' for details.

'0. Exchange rates.R' is run by the chart scripts themselves (no need to open it). It downloads the RBA's daily A$/US$ rates so every chart in dollars is saved twice, in A$ (name.png) and US$ (name_usd.png), for the A$/US$ toggle in the dashboard's top bar. Each value is converted at the average rate for its own period.

Three energy files must be downloaded by hand once a year. energy.gov.au blocks scripted downloads, so R cannot fetch them. DCCEEW publishes new editions of the Australian Energy Statistics each year: Table O (electricity) in June, and Tables C (energy by fuel) and J (energy trade) in September, all listed on https://www.energy.gov.au/publications/australian-energy-update-2026 (the page name changes with the year). Open each link in your web browser and save the file, keeping its name, into the Energy folder:
  - Table O, electricity generation by fuel type: https://www.energy.gov.au/sites/default/files/2026-06/table-o-electricity-generation-by-fuel%20type-2024-25-and-2025.xlsx
  - Table C, energy consumption by state and fuel: https://www.energy.gov.au/sites/default/files/Australian%20Energy%20Statistics%202026%20Table%20C.xlsx
  - Table J, energy supply and trade by fuel (energy security chart): https://www.energy.gov.au/sites/default/files/Australian%20Energy%20Statistics%202026%20Table%20J.xlsx
Then put the new file names (or links) at the top of '1e. Energy.R' (TABLE_O_URL, TABLE_C_FILE and TABLE_J_FILE) before running '1. Refresh all data.R'. If a file is missing, the Energy script stops and says which one to download.

Data sources and links are saved in https://csis365.sharepoint.com/:x:/s/Australia/IQDrSA6EMKEQTaaFrVvdDzzkARJpelangI6YcqL9G_NOScE?e=dy4MO5
