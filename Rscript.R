#install.packages("WikidataQueryServiceR")
library("WikidataQueryServiceR")
library("rJava")
library("rcdk")
library("pls")

sparql_query <- 'SELECT DISTINCT ?comp ?compLabel ?bp ?bpUnit ?bpUnitLabel ?SMILE WHERE {
    ?comp wdt:P31/wdt:P279* wd:Q41581 ;
  p:P2102 [
    ps:P2102 ?bp ;
    psv:P2102/wikibase:quantityUnit  ?bpUnit
    ] ;
  wdt:P233 ?SMILE .
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}'
query_result = query_wikidata(sparql_query);
subset_query = query_result[which(query_result$bpUnitLabel == 'kelvin'),];

test_assump_subset = subset_query;
test_assump_subset = test_assump_subset[order(test_assump_subset$bp, decreasing = FALSE),]
plot(test_assump_subset$bp)

bps_measured = subset_query[,3];
smiles = subset_query[,6];
mols = parse.smiles(subset_query[,6]);
descNames <- c( 'org.openscience.cdk.qsar.descriptors.molecular.WienerNumbersDescriptor',
                'org.openscience.cdk.qsar.descriptors.molecular.APolDescriptor',
                'org.openscience.cdk.qsar.descriptors.molecular.CarbonTypesDescriptor',
                'org.openscience.cdk.qsar.descriptors.molecular.AtomCountDescriptor'
                );
descs = eval.desc(mols, descNames);

desc_dataset = cbind(descs, bps_measured)
desc_test = desc_dataset[1:20,];
desc_train = desc_dataset[21:114,]

PLStest = plsr(bps_measured ~ ., data = desc_train, validation = "LOO")
plot(RMSEP(PLStest), legendpos = "topright")
plot(PLStest, line = TRUE)
summary(PLStest)

predict(PLStest, ncomp = 2, newdata = desc_test)
predplot(PLStest, ncomp = 2, newdata = desc_test, asp = 1, line = TRUE)
RMSEP(PLStest, newdata = desc_test)
