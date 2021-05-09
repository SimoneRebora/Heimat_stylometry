# Heimat_stylometry
Stylometric analyses of the *Heimat* journal (1918)

## Corpora
Divided into three folders
- **test_set** includes all texts from the Heimat journal (texts shorter than 500 words or signed by the author are excluded from the stylometric analysis)
- **training_set** includes texts by the four candidate authors used for the attribution
- **development_set** includes texts by the four candidate authors used to test the stylometric methods

## Scripts
To be run in the indicated order:
- **01_candidates_testing.R** tests stylometric methods on the development corpus (all features used for the analyses are listed [here](https://github.com/SimoneRebora/Heimat_stylometry/blob/main/features/01_candidates_testing_features.csv))
- **02_candidates_analysis.R** attributes the Heimat texts to the four candidates using the best-performing stylometric methods
- **03_imposters_testing.R** tests the impostors method on the development corpus
- **04_imposters_testing_refine.R** identifies the best-working group of impostors on the development corpus
- **05_imposters_analysis.R** confirms the attributions of the Heimat texts to the four candidates using the best configuration of the impostors method
- **06_process_results.R** produces the final results table

## Results
Final results can be consulted in the **Heimat_stylometry__final_results.csv** table