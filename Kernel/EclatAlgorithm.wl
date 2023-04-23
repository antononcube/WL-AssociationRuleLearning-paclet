(* Version 2.0 *)

(* :Title: EclatAlgorithm *)
(* :Context: EclatAlgorithm` *)
(* :Author: Anton Antonov *)
(* :Date: 2022-06-16 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2022 antonov *)
(* :Keywords: *)
(* :Discussion: *)



(***********************************************************)
(* Package definition                                      *)
(***********************************************************)

BeginPackage["AntonAntonov`AssociationRuleLearning`EclatAlgorithm`"];

(*Eclat::usage = "Eclat[data, minSupport, opts] finds frequent sets using the Eclat algorithm.";*)

Begin["`Private`"];

Needs["AntonAntonov`AssociationRuleLearning`"];
Needs["AntonAntonov`SSparseMatrix`"];

(***********************************************************)
(* EclatIntersect                                          *)
(***********************************************************)

Clear[EclatIntersect];
EclatIntersect[aTransactions_?AssociationQ, items : {_String ..}] :=
    Block[{},
      If[KeyExistsQ[aTransactions, items],
        aTransactions[items],
        (*ELSE*)
        Fold[#1 * aTransactions[#2] &, aTransactions[First@items], Rest[items]]
      ]
    ];

EclatIntersect[aTransactions_?AssociationQ, items1 : {_String ..}, items2 : {_String ..}] :=
    EclatIntersect[aTransactions, items1] * EclatIntersect[aTransactions, items2];

(***********************************************************)
(* EclatExtendTransactions                                 *)
(***********************************************************)

Clear[EclatExtendTransactions];

EclatExtendTransactions[aTransactions_?AssociationQ, items : {_String ..}] :=
    Append[aTransactions, items -> EclatIntersect[aTransactions, items]];

EclatExtendTransactions[aTransactions_?AssociationQ, items1 : {_String ..}, items2 : {_String ..}] :=
    Append[aTransactions, Union[Join[items1, items2]] -> EclatIntersect[aTransactions, items1, items2]];

(***********************************************************)
(* EclatSupport                                            *)
(***********************************************************)

Clear[EclatSupport];
EclatSupport[aTransactions_?AssociationQ, items : {_String ..}] :=
    Total[EclatIntersect[aTransactions, items], 2];

EclatSupport[aTransactions_?AssociationQ, items1 : {_String ..}, items2 : {_String ..}] :=
    Total[EclatIntersect[aTransactions, items1, items2], 2];


(***********************************************************)
(* Eclat                                                   *)
(***********************************************************)

Clear[Eclat];

Options[Eclat] = {"MaxNumberOfItems" -> Infinity, "Separator" -> "."};

aECLATTransactions = None;

Eclat[dsTransactions_Dataset, minSupport_?NumberQ, opts : OptionsPattern[]] :=
    Block[{t, sep = OptionValue[Eclat, "Separator"]},
      t = Normal[dsTransactions[All, Association @ KeyValueMap[#1 -> ToString[#1] <> sep <> ToString[#2] &, #] &][Values]];
      Eclat[t, minSupport, opts]
    ] /; minSupport > 0;

Eclat[lsTransactions : {_List ..}, minSupportArg_?NumberQ, opts : OptionsPattern[]] :=
    Block[{minSupport = minSupportArg, t},
      If[minSupport < 1, minSupport = Floor[minSupport * Length[lsTransactions]]];
      t = Join @@ MapIndexed[Thread[{#2[[1]], #1}] &, lsTransactions];
      t = ToSSparseMatrix[ResourceFunction["CrossTabulate"][t, "Sparse" -> True]];
      Eclat[t, minSupport, opts]
    ] /; minSupportArg > 0;

Eclat[matTransactions_SSparseMatrix, minSupportArg_?NumericQ, opts : OptionsPattern[]] :=
    Block[{minSupport = minSupportArg, aTransactions},
      If[minSupport < 1, minSupport = Floor[minSupport * RowsCount[matTransactions]]];
      aTransactions =
          AssociationThread[ColumnNames[matTransactions] -> Map[Identity, Transpose[SparseArray[matTransactions]]]];
      Eclat[aTransactions, minSupport, opts]
    ] /; minSupportArg > 0;

Eclat[aTransactions : Association[(_ -> _?SparseArrayQ) ..], minSupport_?NumericQ, opts : OptionsPattern[]] :=
    Block[{P = List /@ Sort[Keys[aTransactions]], res},
      P = Select[P, EclatSupport[aTransactions, #] >= minSupport &];
      aECLATTransactions = aTransactions;
      res = EclatRec[aTransactions, P, minSupport, {}, 0, opts];
      AssociationThread[res, EclatSupport[aECLATTransactions, #] & /@ res]
    ] /; minSupport > 0;

(*---------------------------------------------------------*)

Clear[EclatRec];
Options[EclatRec] = Options[Eclat];
EclatRec[aTransactions_?AssociationQ, P_List, minSupport_?NumericQ, Farg_List, k_Integer, opts : OptionsPattern[]] :=
    Block[{F = Farg, maxNumberOfItems, P2 = {}, Xab, tXab, PRINT},
      maxNumberOfItems = OptionValue[EclatRec, "MaxNumberOfItems"];
      PRINT[Style[Row[{"rec : ", k}], Purple, Bold]];
      PRINT["P: ", P];
      Do[

        AppendTo[F, Xa];
        PRINT["F: ", F];
        P2 = {};

        Do[
          Xab = Union[Xa, Xb];
          PRINT["{Xa,Xb} : ", {Xa, Xb}];
          If[Length[Xab] <= maxNumberOfItems,
            aECLATTransactions = EclatExtendTransactions[aECLATTransactions, Xa, Complement[Xab, Xa]];
            tXab = EclatSupport[aECLATTransactions, Xab];
            (*tXab=EclatSupport[aECLATTransactions,Xab];*)

            PRINT["Xab->tXab : ", Xab -> tXab];
            If[tXab >= minSupport,
              AppendTo[P2, Xab];
            ]
          ];
          PRINT["P2: ", P2]
          , {Xb, Select[P, Order[Xa, #] > 0 &]}];

        If[Length[P2] > 0,
          F = EclatRec[aTransactions, P2, minSupport, F, k + 1, opts]
        ]

        , {Xa, Select[P, Length[#] <= maxNumberOfItems &]}];
      F
    ];

End[]; (* `Private` *)

EndPackage[]