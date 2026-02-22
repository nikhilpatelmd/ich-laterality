# 1. Load the imputed data
imp <- readRDS("data/proc/ich_imputed.rds")

# 2. Grab the first complete dataset
df_check <- complete(imp, 1)

# 3. Check for NAs in the predictors used in your formula
predictors <- c(
  "ich_laterality",
  "ich_location",
  "age",
  "gcs_baseline",
  "ich_volume_baseline",
  "ivh"
)

# 4. Print the count of NAs for each predictor
# The one with "49" is your culprit!
print(colSums(is.na(df_check[predictors])))
