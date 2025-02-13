# compsem
These are the files used for data preprocessing and visualization for the computational semantics project.

Explanation of files:
1) Thesis Dictionary: contains all of the python dictionaries for each concept
2) data cleaning.R: this is used to preprocess the raw data from qualtrics
3) model consolidation: this is used to wrangle the data from the LLM data collection (from the colabs notebook)
4) people consolidation: this is used to wrangle the data from human data collection (after preprocessing it using the R script)
5) survey generator: this makes the .txt files and the corresponding conditions tracker if you want to make more surveys for Qualtrics 
6) glm_regression.R: vaguely named, this is used for data analysis in llm and human data
7/8) lookup table generator and parsing: I think you'll need both if you want to run more human experiments, this is what makes it easier to score the results from Qualtrics
