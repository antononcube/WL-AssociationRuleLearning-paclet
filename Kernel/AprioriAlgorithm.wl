BeginPackage["AntonAntonov`AssociationRuleLearning`AprioriAlgorithm`"];

(*Apriori::usage = "Apriori[baskets : {_List ..}, minSupport_?NumericQ, minItemsNumber_Integer, maxItemsNumberArg : (_Integer | Infinity)]";*)

Begin["`Private`"];

Needs["AntonAntonov`TriesWithFrequencies`"];
Needs["AntonAntonov`AssociationRuleLearning`"];

Clear[ScanBasket];
ScanBasket[basket_List, k_Integer, aFreqSets_?AssociationQ] :=
    Block[{candidates},
      candidates = Subsets[basket, {k, k}];
      Select[candidates, Lookup[aFreqSets, Key@Most[#], False] && Lookup[aFreqSets, Key@{Last[#]}, False] &]
    ] /; k > 1;

Clear[Apriori];

Apriori::ssup = "All items have support smaller than `1`.";

Options[Apriori] = {Counts -> True};

Apriori[dsBaskets_Dataset, args___] :=
    Block[{},
      Apriori[Normal @ dsBaskets[Values], args]
    ];

Apriori[
  baskets : ( _List | _Dataset ),
  minSupport_?NumericQ,
  args___] :=
    Block[{},
      Apriori[baskets, minSupport / Length[baskets], args]
    ] /; minSupport > 1;

Apriori[
  lsBasketsArg : {_List ..},
  minSupportArg_?NumericQ,
  minItemsNumber_Integer : 1,
  maxItemsNumberArg : (_Integer | Infinity) : Infinity,
  opts : OptionsPattern[]] :=
    Block[{lsBaskets = lsBasketsArg, minSupport = minSupportArg, maxItemsNumber, countsQ,
      aFreqSets, lsFreqSets,
      trBase, trSets, trSets2, aAllTries, k = 1, contQ = True,
      res},

      (*Max number of items processing*)
      minSupport = If[ 0 <= minSupport <= 1, minSupport * Length[lsBaskets], minSupport];

      (*Max number of items processing*)
      maxItemsNumber = Min[maxItemsNumberArg, Max[Length /@ lsBaskets]];

      (*To use counts or not*)
      countsQ = TrueQ @ OptionValue[Apriori, Counts];

      (*Make sure all baskets unique items*)
      lsBaskets = Union /@ lsBaskets;

      (*Make single items baskets trie*)
      trBase = TrieCreate[List /@ Flatten[lsBaskets]];

      (*Remove the items that are not frequent enough*)
      trBase = TrieThresholdRemove[trBase, minSupport, "Postfix" -> None];

      (*Verify early stop*)
      If[TrieDepth[trBase] == 1,
        Message[Apriori::ssup, minSupport];
        Return[{}]
      ];

      (*Initial set of frequent sets*)
      aFreqSets =
          AssociationThread[List /@ Select[Tally[Flatten[lsBaskets]], #[[2]] >= minSupport &][[All, 1]], True];

      (*First gathered trie*)
      aAllTries = <|k -> trBase|>;

      (*Main loop*)
      While[contQ && k < maxItemsNumber,

        k++;

        (*Scan the baskets and make trie with viable candidates*)
        trSets = TrieCreate[Join @@ Map[ScanBasket[#, k, aFreqSets] &, lsBaskets]];

        (*Remove baskets that are not frequent enough*)
        trSets2 = TrieThresholdRemove[trSets, minSupport, "Postfix" -> None];

        (*Get frequent sets from the trie*)
        lsNew = Select[Rest /@ TrieGetWords[trSets2], Length[#] == k &];

        (*Update frequent sets*)
        If[Length[lsNew] == 0,
          contQ = False,
          (*ELSE*)
          aFreqSets = Join[aFreqSets, AssociationThread[lsNew, True]];
          (*Add to gathered tries*)
          AppendTo[aAllTries, k -> trSets2]
        ];

      ];

      lsFreqSets = Select[Keys[aFreqSets], Length[#] >= minItemsNumber &];
      res = Association@Map[# -> N[TrieRetrieve[aAllTries[Length[#]], #][$TrieValue]] &, lsFreqSets];
      If[countsQ, res, res / Length[lsBaskets]]
    ] /; 0 <= minSupportArg <= 1;


End[];

EndPackage[];