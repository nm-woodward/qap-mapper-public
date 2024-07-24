# QAP Mapper
A site selection tool for new LIHTC developments in Georgia
![qap-demo-1-gif](https://github.com/user-attachments/assets/ced14ea6-9105-488c-aba9-6b4568c62836)

For background on this project and a non-technical overview, please see the [project overview page](https://receptive-muenster-7fe.notion.site/QAP-Mapper-2d274826428549879f6b29a4b7a82c9c?pvs=4).

# Startup instructions

## Step 1: Create accounts
To run this tool locally or to host it publicly, you'll need to create accounts on the following platforms and create an API key for each:
- Google: create a [Cloud Billing account](https://cloud.google.com/billing/docs/how-to/create-billing-account)
- Mapbox: create a [new user account](https://account.mapbox.com/)

## Step 2: Clone this repository

## Step 3: Add your API keys
- In the server.R file, replace your Google API key with the placeholder for the my_api_key variable.
- In the React application's App.js file, replace your Mapbox access token with the placeholder for the mapboxgl.accessToken variable.

## Step 4: Set up your working directory
Complete a find/replace in all project files for "your_directory" to the path you are using locally.

## Step 5: Start the R server
The r scripts in the "scripts" folder feed into a single, combined API call set up in the server.R file. To start this server, run start_server.R.

## Step 6: Start the React app
- Open up a command line prompt (or open the "web_app" folder in a code editor of your choice)
- Change your directory to the "web_app" folder.
- Install the required dependencies for the web app ("npm install requirements.txt")
- To run the web app locally, run "npm start"
