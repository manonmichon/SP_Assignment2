#install.packages("WikidataQueryServiceR")
#install.packages("rJava")
#install.packages("rcdk")
#install.packages("pls")

# Load and attach required packages. 
library("WikidataQueryServiceR")
library("rJava")
library("rcdk")
library("pls")

# Defining the SPARQL query
sparql_query <- 'SELECT DISTINCT ?comp ?compLabel ?bp ?bpUnit ?bpUnitLabel ?SMILE WHERE {
    ?comp wdt:P31/wdt:P279* wd:Q41581 ;
  p:P2102 [
    ps:P2102 ?bp ;
    psv:P2102/wikibase:quantityUnit  ?bpUnit
    ] ;
  wdt:P233 ?SMILE .
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}'
query_result = query_wikidata(sparql_query); # Requesting the query from the wikidata server
# Distinguishing the data with the correct unit for the boiling point and subsetting that data
subset_query = query_result[which(query_result$bpUnitLabel == 'kelvin'),];

# Duplication of the original dataset to be sure the original dataset stays intact
test_assump_subset = subset_query;
# Ordering the dataset from low to high boiling point
test_assump_subset = test_assump_subset[order(test_assump_subset$bp, decreasing = FALSE),] 

# Plotting the boiling points ordered from low to high boiling point to check for outliers
plot(test_assump_subset$bp, main = "Scatterplot of query data", ylab = "Boiling point") 
# Checking for inequality of data distribution in the original dataset by plotting the data with no order in boiling point
plot(subset_query$bp, main = "Distribution of query data", ylab = "Boiling point") 

# Renaming column 3 of the subsetted query data to further use and give meaning to this column
bps_measured = subset_query[,3];
# Renaming column 6 of the subsetted query data to further use and give meaning to this column
smiles = subset_query[,6];
parsed_smiles = parse.smiles(subset_query[,6]); # Parses the SMILES given
# Selection of physicochemical properties requested
selected_properties <- c( 'org.openscience.cdk.qsar.descriptors.molecular.WienerNumbersDescriptor',
                        'org.openscience.cdk.qsar.descriptors.molecular.APolDescriptor',
                        'org.openscience.cdk.qsar.descriptors.molecular.CarbonTypesDescriptor',
                        'org.openscience.cdk.qsar.descriptors.molecular.AtomCountDescriptor'
                );
# Request the properties for the selected SMILES
molecular_descriptors = eval.desc(parsed_smiles, selected_properties);

# Binding the molecular descriptors with boiling points from the subsetted query result
moldesc_dataset = cbind(molecular_descriptors, bps_measured)
desc_test = moldesc_dataset[1:20,]; # Establishing a test-dataset with the first twenty alkanes
desc_train = moldesc_dataset[21:114,] # Establishing a train-dataset with the rest of the alkanes

# Construction of the Partial Least Square model
PLStest = plsr(bps_measured ~ ., data = desc_train, validation = "LOO");
# Plotting the error for each amount of latent variables considered
plot(RMSEP(PLStest), legendpos = "topright", main = "RMSEP plot")
plot(PLStest, ncomp = 2, line = TRUE) # Visualisation of the PLS model in combination with the training data
summary(PLStest)

predict(PLStest, ncomp = 2, newdata = desc_test); # Prediction of boiling points using the test data
predplot(PLStest, ncomp = 2, newdata = desc_test, asp = 1, line = TRUE) # The test data is plotted
RMSEP(PLStest, newdata = desc_test) # RMSEP for each of the possible components for the test dataset
