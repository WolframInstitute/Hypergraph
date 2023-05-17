Package["WolframInstitute`Hypergraph`"]

PackageExport["RandomHypergraph"]
PackageExport["RandomHypergraphRule"]
PackageExport["EnumerateHypergraphRules"]

PackageScope["EnumerateWolframModelRules"]



RandomHypergraph[args___, opts : OptionsPattern[]] := Hypergraph[ResourceFunction["RandomHypergraph"][args], opts]

RandomHypergraphRule[args___, opts : OptionsPattern[]] := HypergraphRule[##, opts] & @@ ResourceFunction["RandomWolframModel"][args]

EnumerateHypergraphRules[args___, opts : OptionsPattern[]] := HypergraphRule[##, opts] & @@@ ResourceFunction["EnumerateWolframModelRules"][args]




(* EnumerateWolframModelRules *)


(* maxConnectedAtoms *)

maxConnectedAtoms[{_, 0} | {0, _}, All] := 0

maxConnectedAtoms[{count_Integer, arity_Integer}, All] := count arity - (count - 1)

maxConnectedAtoms[{count_Integer, arity_Integer}, None] := count arity

maxConnectedAtoms[ruleList : {__Rule}, type_] := maxConnectedAtoms[#, type] & /@ ruleList

maxConnectedAtoms[signatures : {{_, _} ...}, All] := With[{signatureResults = maxConnectedAtoms[#, All] & /@ signatures},
    Total[signatureResults] - Max[Count[signatureResults, Except[0]] - 1, 0]
]

maxConnectedAtoms[signatures : {{_, _} ...}, None] := Total[maxConnectedAtoms[#, None] & /@ signatures]

maxConnectedAtoms[rule_Rule, connectivity_] := maxConnectedAtoms[Catenate[List @@ rule], connectivity]

maxConnectedAtoms[arg_, Automatic] := maxConnectedAtoms[arg, All]

maxConnectedAtoms[arg_] := maxConnectedAtoms[arg, Automatic]

(* Enumeration *)

BellMaskRadix[len_] := {Flatten[{1, Split[#[[1]]][[2]] /. 0 -> 1,
    Drop[Split[#[[1]]], 2]}], #[[2]]} & /@ (
        {
            With[{
                zer = ConstantArray[0, len],
                rep = Normal[PositionIndex[Prepend[#, 1]]][[1, 2]]
            },
                ReplacePart[zer, Thread[rep -> Range[Length[rep]]]]
            ],
            Prepend[Select[
                Flatten[
                    MapIndexed[
                        Table[#2[[1]], {(#1 - 1)}] &,
                        Differences[Flatten[{1, Position[#, 1] + 1, len + 1}]]
                    ],
                    1
                ], # > 1 &], 1]
        } & /@ Table[IntegerDigits[k, 2, len - 1], {k, 0, 2^(len - 1) - 1}]
     )

DelDup[list_List] := list /. MapIndexed[#1 -> First[#2] &, DeleteDuplicates[Flatten[list]]]

MiserTermsInTuples[tup_List] := Module[{gat, gather, seqs, size},
    gather = Gather[tup]; size = Length[gather];
    seqs = Transpose[{Range[size]}];
    gat = gather[[All, 1]];
    Do[
        seqs = Flatten[With[{grow = #, new = Complement[Range[size], #]},
            Append[grow, #] & /@ new] & /@
        First[
            SplitBy[SortBy[seqs, Length[Union[Flatten[gat[[#]]]]] &],
            Length[Union[Flatten[gat[[#]]]]] &]], 1],
        {k, size - 1}
    ];
    First[SplitBy[
     SortBy[Union[Flatten[gather[[#]], 1] & /@ seqs], DelDup[#] &],
     DelDup[#] &
    ]]
]

xFindCanonicalWolframModel[rule_] := Module[{canonicalparts, leftright, leftrightparts, len, parts},
    leftright = Table[SortBy[rule[[n]], -Length[#] &], {n, 1, 2}];
    leftrightparts = SplitBy[#, Length] & /@ leftright;
    len = Length[leftrightparts[[1]]];
    parts = Flatten[leftrightparts, 1];
    canonicalparts = MiserTermsInTuples[#] & /@ parts;
    parts = First[SortBy[Tuples[canonicalparts], DelDup[Flatten[#]] &]];
    (Flatten[Take[parts, len], 1] -> Flatten[Drop[parts, len], 1]) /.
        MapIndexed[#1 -> First[#2] &, DeleteDuplicates[Flatten[parts]]]
]

MaskedRadixStandardOrder[{mask_, radix_}] := Module[{restRadix = Rest[radix], fills, len, position, prod},
    position = Flatten[Position[mask, 0]];
    len = Length[restRadix]; prod = Times @@ restRadix;
    fills = Table[IntegerDigits[n, MixedRadix[restRadix], len] + 1, {n, 0, prod - 1}];
    ReplacePart[mask, Thread[position -> #]] & /@ fills
]

ruleSignatureLength[signature_Rule] := Total[ruleSignatureLength /@ List @@ signature]

ruleSignatureLength[signature : {___List}] := Total[Times @@@ signature]

ApplyWolframRuleSignaturetoList[sig_, list_] := Module[{all, lhs, rhs, part},
    lhs = Table[#2, {#1}] & @@@ sig[[1]];
    rhs = Table[#2, {#1}] & @@@ sig[[2]];
    part = Partition[FoldList[Plus, 0, Flatten[{lhs, rhs}]], 2, 1];
    all = Take[list, {#[[1]] + 1, #[[2]]}] & /@ part;
    Rule @@ TakeDrop[all, Length[Flatten[lhs]]]
]

EnumerateWolframModelRules[signature_, type : (Automatic | None | All) : Automatic] :=
    EnumerateWolframModelRules[signature, {maxConnectedAtoms[signature, type], type}]

EnumerateWolframModelRules[signature_, s : _Integer ? Positive | Automatic : Automatic] :=
 EnumerateWolframModelRules[signature, {s, Automatic}]

EnumerateWolframModelRules[signature_Rule, {s : _Integer ? Positive | Automatic : Automatic, type : Automatic | None | All}] := With[{
    ss = ReverseSortBy[#, Last] & /@ signature,
    n = Replace[s, Automatic :> maxConnectedAtoms[signature, type]]
},
    Catenate @ Map[
        Select[
            ApplyWolframRuleSignaturetoList[ss, #] & /@ MaskedRadixStandardOrder[#],
            ResourceFunction["ConnectedWolframModelQ"][#, type] && # === xFindCanonicalWolframModel[#] &
        ] &,
        Select[Max[First[#]] == n &] @ BellMaskRadix[ruleSignatureLength[ss]]
    ]
]

