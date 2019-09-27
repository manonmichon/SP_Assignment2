#install.packages("WikidataQueryServiceR")
library("WikidataQueryServiceR")
library("rJava")
library("rcdk")

sparql_query <- 'SELECT ?comp ?compLabel ?bp ?bpUnit ?bpUnitLabel ?SMILE WHERE {
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

smiles = subset_query[,6];
mols = parse.smiles(subset_query[,6]);
descNames <- c( 'org.openscience.cdk.qsar.descriptors.molecular.KierHallSmartsDescriptor',
                'org.openscience.cdk.qsar.descriptors.molecular.APolDescriptor'
                );
descs = eval.desc(mols, descNames);