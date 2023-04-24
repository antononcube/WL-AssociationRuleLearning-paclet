(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["AntonAntonov`AssociationRuleLearning`"];

Apriori::usage = "Apriori[baskets : {_List ..}, minSupport_?NumericQ, minItemsNumber_Integer, maxItemsNumberArg : (_Integer | Infinity)] \
finds frequent sets using the Apriori algorithm.";

Eclat::usage = "Eclat[data, minSupport, opts] finds frequent sets using the Eclat algorithm.";

PacletInstall["AntonAntonov/TriesWithFrequencies", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/SSparseMatrix", AllowVersionUpdate -> False];

Begin["`Private`"];

Needs["AntonAntonov`AssociationRuleLearning`AprioriAlgorithm`"];
Needs["AntonAntonov`AssociationRuleLearning`EclatAlgorithm`"];


End[];
EndPackage[];