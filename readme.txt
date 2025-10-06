A proper thank-you to G. Elliott Morris for sharing his work:
https://github.com/TheEconomist/us-potus-model

To use this model, open model.r and run it. 2026_Senate_polling_averages.csv is the key input for the current forecast; if you want the forecast to change, you need to update that csv with new data. You can use 2026_Senate_polling_averages.r to do this. For that script to update the csv, you need to update 2026_Senate_polling_data.csv with raw polls.

If you're new to this, make sure you've downloaded R and RStudio (https://posit.co/download/rstudio-desktop/). When running the model, hit ctrl+a to highlight everything, then hit run. (Unlike other languages, R does not run everything if you don't highlight it all first.)

The older version of the model was created with the framework in the simple jack txt file. Below are the prompts used to create this version. Note that revisions were still made afterward; the pure output from Claude Opus was not the final version.

Prompt 1: "Attached is a model of the 2016 US presidential election.

Not attached is a model of the US Senate elections. In that model, the distribution of polling errors is estimated empirically, and the resulting probability distribution functions are used for random draws in monte carlo simulations of the elections (random errors are added to estimated vote shares, which are themselves estimated separately using priors and polling averages w/ Bayesian updating).

Explain how the attached model works in comparison. I want to know what happens in each simulation to determine the votes received by candidates. poll_Model_2020.txt is a stan script referenced in final_2016.R as poll_model_2020.stan."

Prompt 2: "What do you mean by backwards random walks, exactly? If you're doing a random walk backwards from election day, you're eventually going to reach today, and wouldn't you miss what today actually looks like? I must be misunderstanding."

Prompt 3: "Attached is the Senate model. It does not properly account for correlation across states: states are only correlated insofar as correlation is captured by the national errors (rep_national_error and dem_national_error), which are randomly determined and added to all vote shares in each simulation. The rest of each state's error is just individual, randomly-determined errors.


I want you to rewrite it to take advantage of whatever tools are provided by the 2016 model. The only major revision you should make is making sure errors are realistically correlated across states. Assume that it shares a working directory with the previous model, so you still have access to the same files referenced by the 2016 model, and any of the files referenced by the attached version of the Senate model."
