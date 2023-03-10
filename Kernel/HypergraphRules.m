Package["WolframInstitute`Hypergraph`"]


PackageExport["HypergraphRule"]
PackageExport["ToPatternRules"]
PackageExport["PatternRuleToMultiReplaceRule"]
PackageExport["HighlightRule"]



ToPatternRules[lhs : {___List}, rhs : {___List}] := Block[{vs = Union @@ lhs, ws = Union @@ rhs, varSymbols, newVars},
    varSymbols = Association @ MapIndexed[#1 -> Symbol["\[FormalX]" <> ToString[#2[[1]]]] &, Union[vs, ws]];
    newVars = Replace[Complement[ws, vs], varSymbols, {2}];
    Replace[lhs, Pattern[#, _] & /@ varSymbols, {2}] :> Module[##] & [Replace[newVars, varSymbols, {1}], Replace[rhs, varSymbols, {2}]]
]

ToPatternRules[HypergraphRule[input_ ? HypergraphQ, output_ ? HypergraphQ]] := ToPatternRules[input["EdgeList"], output["EdgeList"]]

ToPatternRules[rules : {___HypergraphRule}] := ToPatternRules /@ rules


PatternRuleToMultiReplaceRule[rule : _[Verbatim[Condition][lhs_, _] | lhs_, _Module]] := With[{
    nothings = Unevaluated @@ Array[Nothing &, Length[lhs] - 1, Automatic, Hold @* List]
},
	ResourceFunction["SpliceAt"][
		MapAt[List, {2}] @ MapAt[Splice, {2, 2}] @ rule,
		nothings,
		{2, 2}
	]
]

PatternRuleToMultiReplaceRule[rule : _[lhs_List | Verbatim[HoldPattern][lhs_List], Verbatim[Module][vars_, rhs_List]]] := Block[{
	l = Length[Unevaluated[lhs]], r = Length[Unevaluated[rhs]],
	newRule
},
	newRule = If[r > 1, MapAt[Splice, rule, {2, 2}], Delete[rule, {2, 2, 0}]];
	If[r < l, newRule = Nest[Insert[Nothing, {2, 2}], MapAt[List, newRule, {2}], l - r]];
	ReplaceAt[newRule /. Verbatim[Module][{}, body_] :> body, Splice[body_] :> body, {2}]
]


(rule : HypergraphRule[input_ ? HypergraphQ, output_ ? HypergraphQ])[hg_ ? HypergraphQ] :=
    Hypergraph[VertexList[input], #[[1]]] & /@
        Values @ ResourceFunction["MultiReplace"][
            hg["EdgeList"],
            PatternRuleToMultiReplaceRule @ ToPatternRules @ rule,
            {1},
            "Mode" -> "OrderlessSubsets"
        ]


HighlightRule[rule_HypergraphRule, hg_ ? HypergraphQ] := Block[{
    edges = EdgeList[hg], matches
},
    matches = KeyMap[First] @ Map[First] @ ResourceFunction["MultiReplace"][
            edges,
            PatternRuleToMultiReplaceRule @ ToPatternRules @ rule,
            {1},
            "Mode" -> "OrderlessSubsets"
        ];
    KeyValueMap[{pos, rhs} |->
        Framed[
            SimpleHypergraphPlot[
                hg,
                EdgeStyle -> Map[
                    # -> Directive[Red, EdgeForm[Directive[Dashed, Red, Thick]]] &,
                    Extract[edges, pos]
                ],
                ImageSize -> Tiny
            ] ->
            SimpleHypergraphPlot[
                rhs,
                EdgeStyle -> Map[
                    # -> Directive[Red, EdgeForm[Directive[Dashed, Red, Thick]]] &,
                    (* output edges always getting splices at the first position *)
                    rhs[[ Range[pos[[1, 1]]] + Length[pos] - 1 ]]
                ],
                ImageSize -> Tiny
            ]
        ],
        matches
    ]
]


HypergraphRule /: MakeBoxes[HypergraphRule[input_ ? HypergraphQ, output_ ? HypergraphQ], form_] :=
    ToBoxes[
        Deploy @ Framed[
            SimpleHypergraphPlot[input, VertexLabels -> Automatic, ImageSize -> Tiny] ->
            SimpleHypergraphPlot[output, VertexLabels -> Automatic, ImageSize -> Tiny]
        ],
        form
    ]
