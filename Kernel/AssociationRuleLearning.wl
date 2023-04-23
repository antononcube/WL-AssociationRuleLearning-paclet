(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["AntonAntonov`AssociationRuleLearning`"];

Apriori::usage = "Apriori[setOfItemSets,minProb,opts] returns a list of three elements: \
association sets with indexes, item to indexes rules, and indexes to item rules. \
The association sets appear in setOfItemSets with frequency that is at least minProb. \
Apriori takes the option \"MaxNumberOfItems\" -> (All | _Integer) .";

AprioriSparseArrayRepresentation::usage = "AprioriSparseArrayRepresentation[setOfItemSets] returns a list of three elements: \
(1) a 0-1 matrix M (a list of sparse arrays) for which M[[i,j]]==1 if the i-th item belongs to setOfItemSets[[j]], \
(2) item to indexes rules, and (3) indexes to item rules.";

AssociationRules::usage = "AssociationRules[setOfItemSets,assocItemSet,minConfidence] finds the possible association rules \
for assocItemSet using setOfItemSets that have confidence at least minConfidence and calculates for each of the rules the measures: \
Support, Confidence, Lift, Leverage, and Conviction. AssociationRules[setOfItemSets,assocItemSets,minConfidence,minSupport] \
takes the association sets from assocItemSets that have support at least minSupport and finds the association rules for them.";

Support::usage = "Support[setOfItemSets, itemSet] gives the fraction of the sets in setOfItemSets that contain itemSet.";

QuantileReplacementFunc::usage = "QuantileReplacementFunc[qBoundaries] makes a piece-wise function \
for mapping of a real value to the enumerated intervals Partition[Join[{-Infinity}, qBoundaries, {Infinity}], 2, 1].";

RymonTree::usage = "RymonTree[numberOfItems] gives the Rymon tree for numberOfItems.";

ItemRules::usage = "ItemRules[setOfItemSets, frequentSetsOfIDs, itemToIDRules, idToItemRules, itemSpec, minConfidence, minSupport, nAssocItems] \
finds rules for a specified item or list if items using the baskets data and the result of Apriori.";

Eclat::usage = "Eclat[data, minSupport, opts] finds frequent sets using the Eclat algorithm.";

PacletInstall["AntonAntonov/TriesWithFrequencies", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/SSparseMatrix", AllowVersionUpdate -> False];

Begin["`Private`"];

Needs["AntonAntonov`AssociationRuleLearning`AprioriAlgorithm`"];
Needs["AntonAntonov`AssociationRuleLearning`EclatAlgorithm`"];


End[];
EndPackage[];