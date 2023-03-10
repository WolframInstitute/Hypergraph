Package["WolframInstitute`Hypergraph`"]


PackageExport["HypergraphRuleQ"]
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


Options[HighlightRule] := Options[SimpleHypergraphPlot]

HighlightRule[rule_HypergraphRule, hg_ ? HypergraphQ, opts : OptionsPattern[]] := Block[{
    edges = EdgeList[hg], matches
},
    edgeStyles = Thread[edges ->
        Replace[
            edges,
            Join[
                Replace[Flatten[{OptionValue[SimpleHypergraphPlot, hg["Options"], EdgeStyle]}], {Automatic -> Nothing, s : Except[_Rule] :> _ -> s}, {1}],
                MapIndexed[#1 -> Directive[OptionValue[SimpleHypergraphPlot, hg["Options"], ColorFunction][#2[[1]]], EdgeForm[Transparent]] &, edges]
            ],
            {1}
        ]
    ];
    matches = Block[{$ModuleNumber = 1},
        KeyMap[First] @ Map[First] @ ResourceFunction["MultiReplace"][
                edges,
                PatternRuleToMultiReplaceRule @ ToPatternRules @ rule,
                {1},
                "Mode" -> "OrderlessSubsets"
        ]
    ];
    KeyValueMap[{pos, rhs} |->
        GraphicsRow[{
            SimpleHypergraphPlot[
                hg,
                opts,
                EdgeStyle -> Map[
                    # -> Directive[Thick, Dashed, Red, EdgeForm[Directive[Dashed, Red, Thick]]] &,
                    Extract[edges, pos]
                ],
                ImageSize -> Medium
            ],
            Graphics[{Arrowheads[0.3], Arrow[{{0, 0}, {.25, 0}}]}, ImageSize -> Medium],
            SimpleHypergraphPlot[
                rhs,
                opts,
                EdgeStyle -> Join[
                    Map[
                        # -> Directive[Thick, Dashed, Red, EdgeForm[Directive[Dashed, Red, Thick]]] &,
                        (* output edges always getting splices at the first position *)
                        rhs[[ Range[Length[pos]] + pos[[1, 1]] - 1 ]]
                    ],
                    edgeStyles
                ],
                ImageSize -> Medium,
                VertexCoordinates -> Thread[VertexList[hg] -> HypergraphEmbedding[hg]],
                hg["Options"]
            ]
        }],
        matches
    ]
]



HypergraphRuleQ[hr_HypergraphRule] := System`Private`HoldValidQ[hr]

HypergraphRuleQ[___] := $Failed

Options[HypergraphRule] := Options[Hypergraph]

(hr : HypergraphRule[input_, output_, opts : OptionsPattern[]]) /; ! HypergraphRuleQ[Unevaluated[hr]] :=
    Enclose[
        System`Private`HoldSetValid @ HypergraphRule[##] & [
            ConfirmBy[Hypergraph[input, opts], HypergraphQ],
            ConfirmBy[Hypergraph[output, opts], HypergraphQ]
        ]
    ]

HypergraphRule /: MakeBoxes[hr : HypergraphRule[input_, output_] ? HypergraphRuleQ, form_] := With[{
    boxes = ToBoxes[
        GraphicsRow[{
            SimpleHypergraphPlot[input, VertexLabels -> Automatic, ImageSize -> Tiny],
            Graphics[{Arrowheads[.5], Arrow[{{0, 0}, {.5, 0}}]}, ImageSize -> Tiny],
            SimpleHypergraphPlot[output, VertexLabels -> Automatic, ImageSize -> Tiny]
        }],
        form
    ]
},
    InterpretationBox[boxes, hr]
]

