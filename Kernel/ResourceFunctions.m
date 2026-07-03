Package["WolframInstitute`Hypergraph`"]

PackageExport[RandomHypergraphRule]
PackageExport[EnumerateHypergraphRules]
PackageExport[EnumerateWolframModelRules]

PackageScope[maxConnectedAtoms]



RandomHypergraphRule[args___, opts : OptionsPattern[]] := HypergraphRule[##, opts] & @@ ResourceFunction["RandomWolframModel"][args]

EnumerateHypergraphRules[args___, opts : OptionsPattern[]] := HypergraphRule[##, opts] & @@@ EnumerateWolframModelRules[args]




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


(* Reference (slow) enumeration machinery.
   These functions define the semantics that the fast enumeration below reproduces exactly:
   candidates are all restricted growth strings with exactly s distinct atoms, filtered by
   ConnectedWolframModelQ[rule, type] and by being fixed points of xFindCanonicalWolframModel. *)

BellMaskRadix[1] := {{{1}, {1}}}

BellMaskRadix[len_] := {Flatten[{1, Split[#[[1]]][[2]] /. 0 -> 1, Drop[Split[#[[1]]], 2]}], #[[2]]} & /@ (
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

slowEnumerateWolframModelRules[signature_Rule, {s_Integer ? Positive, type : Automatic | None | All}] := With[{
    ss = ReverseSortBy[#, Last] & /@ signature
},
    Catenate @ Map[
        Select[
            ApplyWolframRuleSignaturetoList[ss, #] & /@ MaskedRadixStandardOrder[#],
            ResourceFunction["ConnectedWolframModelQ"][#, type] && # === xFindCanonicalWolframModel[#] &
        ] &,
        Select[Max[First[#]] == s &] @ BellMaskRadix[ruleSignatureLength[ss]]
    ]
]


(* Fast enumeration.
   Instead of materializing all restricted growth strings and filtering, generate candidates
   with a backtracking search whose prunes are provably necessary conditions for acceptance:
     - exact atom-count feasibility (new atoms become forced when slots run out),
     - within a canonical part (maximal run of same-arity edges on one side), duplicate edges
       must be adjacent, prefixes must stay atom-minimal under swaps with later edges, and
       adjacent groups must be in lexicographically minimal order (necessary conditions for
       membership in the MiserTermsInTuples candidate set),
     - union-find connectivity feasibility bounds per ConnectedWolframModelQ type.
   The slots of the last (largest) part are enumerated by a compiled kernel; survivors then
   pass an exact, memoized canonicality test equivalent to # === xFindCanonicalWolframModel[#],
   and finally get sorted into the legacy (BellMaskRadix mask, mixed-radix fill) order. *)


(* first-occurrence relabeling, the flat form of DelDup *)
rgsRelabel[v_List] := Replace[v, MapIndexed[#1 -> First[#2] &, DeleteDuplicates[v]], {1}]

(* minimal local pattern over the MiserTermsInTuples beam for a part given as an edge list
   (uniform arity, duplicate edges adjacent); the miser candidate set of a part consists of
   exactly the arrangements whose local pattern equals this minimum *)
miserMinPattern[part_List] := miserMinPattern[part] = Block[{groups = Split[part], d, masks, seqs, cnts},
    d = Length[groups];
    If[ d <= 1,
        rgsRelabel @ Flatten[part]
        ,
        masks = Total[2 ^ (Union[Flatten[#]] - 1)] & /@ groups;
        seqs = Table[{{i}, masks[[i]]}, {i, d}];
        Do[
            cnts = DigitCount[#[[2]], 2, 1] & /@ seqs;
            seqs = Catenate @ Map[
                Function[state, {Append[state[[1]], #], BitOr[state[[2]], masks[[#]]]} & /@ Complement[Range[d], state[[1]]]],
                Pick[seqs, cnts, Min[cnts]]
            ],
            {d - 1}
        ];
        First @ Sort[rgsRelabel[Flatten[groups[[#[[1]]]]]] & /@ seqs]
    ]
]

(* membership of an arrangement (given as its local pattern) in the miser candidate set *)
miserMemberPatternQ[pat_List, arity_Integer] := miserMemberPatternQ[pat, arity] = (pat === miserMinPattern[Partition[pat, arity]])

(* group orderings of a part (given as its local pattern) that reproduce the same pattern *)
miserClassOrderings[pat_List, arity_Integer] := miserClassOrderings[pat, arity] = Block[{groups = Split @ Partition[pat, arity], rec},
    If[ Length[groups] == 1,
        {{1}}
        ,
        rec = Function[{chosenIdx, flatSoFar, remaining},
            If[ remaining === {},
                {chosenIdx}
                ,
                Catenate @ Table[
                    With[{flat = Join[flatSoFar, Flatten[groups[[remaining[[i]]]]]]},
                        If[ rgsRelabel[flat] === Take[pat, Length[flat]],
                            rec[Append[chosenIdx, remaining[[i]]], flat, Delete[remaining, i]],
                            {}
                        ]
                    ],
                    {i, Length[remaining]}
                ]
            ]
        ];
        rec[{}, {}, Range @ Length[groups]]
    ]
]

miserSingletonQ[pat_List, arity_Integer] := miserSingletonQ[pat, arity] = (Length[miserClassOrderings[pat, arity]] == 1)

(* exact fixed-point test: True iff no member of the product of per-part miser classes has a
   smaller relabeled flat form than the candidate itself (parts: positive-arity parts as edge
   lists with the candidate's labels; F: the flat candidate) *)
miserProductMinimalQ[parts_List, F_List] := Block[{groupsPerPart = Split /@ parts, ords, rec},
    ords = MapThread[miserClassOrderings[rgsRelabel @ Flatten[#1], #2] &, {parts, Length /@ First /@ parts}];
    If[ AllTrue[ords, Length[#] == 1 &],
        True
        ,
        Catch[
            rec = Function[{j, seq},
                If[ j <= Length[parts],
                    Do[
                        With[{flat = Join[seq, Flatten[groupsPerPart[[j, ord]]]]},
                            Switch[Order[rgsRelabel[flat], Take[F, Length[flat]]],
                                1, Throw[False],
                                0, rec[j + 1, flat]
                            ]
                        ],
                        {ord, ords[[j]]}
                    ]
                ]
            ];
            rec[1, {}];
            True
        ]
    ]
]

(* candidate ordering used by the reference implementation: masks enumerated by binary counting
   over first-occurrence indicators, then mixed-radix counting over the reused-slot values;
   the key has fixed length so that canonical order compares element-wise across atom counts *)
legacyEnumerationKey[F_List] := Join[Boole @ MapThread[#1 == #2 + 1 &, {Rest[F], Most @ FoldList[Max, F]}], Rest[F]]

(* edge/part layout of an arity-descending signature; a part is a maximal run of equal-arity
   edges on one side, matching the SplitBy[..., Length] parts of xFindCanonicalWolframModel *)
enumRuleStructure[ss_Rule] := Block[{edges, runs, ends},
    edges = Catenate @ MapIndexed[
        Function[{groups, side}, Catenate[ConstantArray[{First[side], #[[2]]}, #[[1]]] & /@ groups]],
        List @@ ss
    ];
    runs = Split[edges];
    ends = Accumulate[Length /@ runs];
    <|
        "Arities" -> edges[[All, 2]],
        "Sides" -> edges[[All, 1]],
        "Parts" -> MapThread[
            <|"Side" -> #1[[1, 1]], "Arity" -> #1[[1, 2]], "Count" -> Length[#1], "EdgeIndices" -> Range[#2 - Length[#1] + 1, #2]|> &,
            {runs, ends}
        ],
        "L" -> Total @ edges[[All, 2]],
        "NEdges" -> Length[edges]
    |>
]

(* connected components among the atoms present in the given edges (co-occurrence connectivity) *)
hyperedgeComponentCount[edges_List] := Length @ Fold[
    Function[{components, edge}, Block[{atoms = Union[edge]},
        If[ atoms === {},
            components,
            Append[Select[components, ! IntersectingQ[#, atoms] &], Union @@ Append[Select[components, IntersectingQ[#, atoms] &], atoms]]
        ]
    ]],
    {},
    edges
]

$enumPopcountTable := $enumPopcountTable = Developer`ToPackedArray[Total /@ IntegerDigits[Range[0, 65535], 2, 16]]

enumCompile[make_] := Block[{f = Quiet @ Check[make["C"], $Failed]},
    If[MatchQ[f, _CompiledFunction], f, make["WVM"]]
]

(* relabel a column range of each row to its local pattern (atom values must be <= 62) *)
makeRelabelRowsKernel[target_] := Compile[{{m, _Integer, 2}, {c0, _Integer}, {c1, _Integer}},
    Module[{n = Length[m], w = c1 - c0 + 1, out, seen, k = 0, x = 0},
        out = Table[0, {Max[n, 1]}, {Max[w, 1]}];
        seen = Table[0, {64}];
        Do[
            Do[seen[[i]] = 0, {i, 64}]; k = 0;
            Do[
                x = m[[r, c0 + j - 1]];
                If[seen[[x]] == 0, k++; seen[[x]] = k];
                out[[r, j]] = seen[[x]],
                {j, w}
            ],
            {r, n}
        ];
        out
    ],
    CompilationTarget -> target, RuntimeOptions -> "Speed"
]

$relabelRowsKernel := $relabelRowsKernel = enumCompile[makeRelabelRowsKernel]

(* backtracking enumeration of the last part's slots given the preceding slots; emits flat
   candidates passing atom-count, duplicate-adjacency, swap-with-future and adjacent-group-swap
   miser conditions, and connectivity (bounds during the search, exact checks at completion) *)
makeSuffixDFSKernel[target_] := Compile[{
    {prefix, _Integer, 1}, {prefArities, _Integer, 1}, {prefSides, _Integer, 1},
    {aa, _Integer}, {cc, _Integer}, {sideP, _Integer},
    {s, _Integer}, {k0, _Integer}, {typeCode, _Integer},
    {checkLEnd, _Integer}, {checkREnd, _Integer},
    {popT, _Integer, 1}},
    Module[{
        M = aa * cc, t, v, k, j = 0, q = 0, prevAtom = 0, rx = 0, ry = 0, tmp = 0, ok = 1, good = 1,
        mq = 0, pcA = 0, pcB = 0, x1 = 0, x2 = 0, eqPrev = 0, cnt = 0,
        maintainU = 0, maintainS = 0, ae = 0, se = 0, ptr = 0,
        val, kb, rNew, rUc, rUp, rSc, rSp, rSa,
        parentU, sizeU, parentS, sizeS, inS, compsU = 0, compsS = 0,
        pmArr, emArr, gStart, rankC, rank1, rank2, va, vb,
        bag, needNew = 0, slotsLeft = 0, vmax = 0, vmin = 0, i = 0, x = 0,
        kc = 0, k1 = 0, k2 = 0, jA = 0, jB = 0, cmp = 0, s1 = 0, s2 = 0, mG2 = 0, doSwap = 0, qq = 0
    },
        maintainU = If[typeCode >= 1, 1, 0];
        maintainS = If[(sideP == 1 && typeCode >= 1) || (sideP == 2 && typeCode == 2), 1, 0];
        val = Table[0, {M + 1}]; kb = Table[0, {M + 2}];
        rNew = Table[0, {M + 1}]; rUc = Table[0, {M + 1}]; rUp = Table[0, {M + 1}];
        rSc = Table[0, {M + 1}]; rSp = Table[0, {M + 1}]; rSa = Table[0, {M + 1}];
        parentU = Table[i, {i, s + 1}]; sizeU = Table[1, {s + 1}];
        parentS = Table[i, {i, s + 1}]; sizeS = Table[1, {s + 1}];
        inS = Table[0, {s + 1}];
        compsU = k0; compsS = 0;
        pmArr = Table[0, {cc + 2}]; emArr = Table[0, {cc + 2}]; gStart = Table[1, {cc + 2}];
        rankC = Table[0, {s + 2}]; rank1 = Table[0, {s + 2}]; rank2 = Table[0, {s + 2}];
        va = Table[0, {M + 1}]; vb = Table[0, {M + 1}];
        bag = Internal`Bag[Most[{0}]];
        (* seed union-find structures from the prefix *)
        If[ maintainU == 1 || maintainS == 1,
            ptr = 0;
            Do[
                ae = prefArities[[e]]; se = prefSides[[e]];
                Do[
                    x1 = prefix[[ptr + jj]];
                    If[ maintainS == 1 && se == sideP,
                        If[inS[[x1]] == 0, inS[[x1]] = 1; parentS[[x1]] = x1; sizeS[[x1]] = 1; compsS++];
                        If[ jj > 1,
                            x2 = prefix[[ptr + jj - 1]];
                            rx = x1; While[parentS[[rx]] != rx, rx = parentS[[rx]]];
                            ry = x2; While[parentS[[ry]] != ry, ry = parentS[[ry]]];
                            If[ rx != ry,
                                If[sizeS[[rx]] < sizeS[[ry]], tmp = rx; rx = ry; ry = tmp];
                                parentS[[ry]] = rx; sizeS[[rx]] += sizeS[[ry]]; compsS--
                            ]
                        ]
                    ];
                    If[ maintainU == 1 && jj > 1,
                        x2 = prefix[[ptr + jj - 1]];
                        rx = x1; While[parentU[[rx]] != rx, rx = parentU[[rx]]];
                        ry = x2; While[parentU[[ry]] != ry, ry = parentU[[ry]]];
                        If[ rx != ry,
                            If[sizeU[[rx]] < sizeU[[ry]], tmp = rx; rx = ry; ry = tmp];
                            parentU[[ry]] = rx; sizeU[[rx]] += sizeU[[ry]]; compsU--
                        ]
                    ],
                    {jj, ae}
                ];
                ptr += ae,
                {e, Length[prefArities]}
            ]
        ];
        (* iterative DFS over suffix slots *)
        kb[[1]] = k0;
        t = 1; val[[1]] = 0;
        While[t >= 1,
            (* roll back the current slot's effects if a value is present *)
            If[ val[[t]] > 0,
                v = val[[t]];
                If[rUc[[t]] > 0, parentU[[rUc[[t]]]] = rUc[[t]]; sizeU[[rUp[[t]]]] -= sizeU[[rUc[[t]]]]; compsU++; rUc[[t]] = 0; rUp[[t]] = 0];
                If[rSc[[t]] > 0, parentS[[rSc[[t]]]] = rSc[[t]]; sizeS[[rSp[[t]]]] -= sizeS[[rSc[[t]]]]; compsS++; rSc[[t]] = 0; rSp[[t]] = 0];
                If[rSa[[t]] == 1, inS[[v]] = 0; compsS--; rSa[[t]] = 0];
                If[rNew[[t]] == 1, If[maintainU == 1, compsU--]; rNew[[t]] = 0]
            ];
            k = kb[[t]];
            slotsLeft = M - t + 1;
            needNew = s - k;
            If[ needNew > slotsLeft,
                vmax = -1; vmin = 0,
                If[ needNew == slotsLeft,
                    vmin = k + 1; vmax = k + 1,
                    vmin = 1; vmax = If[k < s, k + 1, k]
                ]
            ];
            v = If[val[[t]] == 0, vmin, val[[t]] + 1];
            If[ vmax < 1 || v > vmax,
                val[[t]] = 0; t--
                ,
                (* apply value v at slot t *)
                val[[t]] = v;
                j = Mod[t - 1, aa] + 1; q = Quotient[t - 1, aa] + 1;
                If[ v == k + 1,
                    rNew[[t]] = 1; kb[[t + 1]] = k + 1;
                    If[maintainU == 1, parentU[[v]] = v; sizeU[[v]] = 1; compsU++],
                    kb[[t + 1]] = k
                ];
                If[ maintainS == 1 && inS[[v]] == 0,
                    inS[[v]] = 1; parentS[[v]] = v; sizeS[[v]] = 1; compsS++; rSa[[t]] = 1
                ];
                If[ j > 1,
                    prevAtom = val[[t - 1]];
                    If[ maintainU == 1,
                        rx = v; While[parentU[[rx]] != rx, rx = parentU[[rx]]];
                        ry = prevAtom; While[parentU[[ry]] != ry, ry = parentU[[ry]]];
                        If[ rx != ry,
                            If[sizeU[[rx]] < sizeU[[ry]], tmp = rx; rx = ry; ry = tmp];
                            parentU[[ry]] = rx; sizeU[[rx]] += sizeU[[ry]]; compsU--; rUc[[t]] = ry; rUp[[t]] = rx
                        ]
                    ];
                    If[ maintainS == 1,
                        rx = v; While[parentS[[rx]] != rx, rx = parentS[[rx]]];
                        ry = prevAtom; While[parentS[[ry]] != ry, ry = parentS[[ry]]];
                        If[ rx != ry,
                            If[sizeS[[rx]] < sizeS[[ry]], tmp = rx; rx = ry; ry = tmp];
                            parentS[[ry]] = rx; sizeS[[rx]] += sizeS[[ry]]; compsS--; rSc[[t]] = ry; rSp[[t]] = rx
                        ]
                    ]
                ];
                ok = 1;
                If[ j == aa,
                    (* edge q completed *)
                    eqPrev = 0;
                    If[ q > 1,
                        eqPrev = 1;
                        Do[If[val[[(q - 1) * aa + i]] != val[[(q - 2) * aa + i]], eqPrev = 0], {i, aa}];
                        (* duplicate edges must be adjacent *)
                        If[ eqPrev == 0,
                            Do[
                                If[ ok == 1,
                                    cnt = 1;
                                    Do[If[val[[(q - 1) * aa + i]] != val[[(qq2 - 1) * aa + i]], cnt = 0], {i, aa}];
                                    If[cnt == 1, ok = 0]
                                ],
                                {qq2, 1, q - 2}
                            ]
                        ]
                    ];
                    gStart[[q]] = If[q > 1 && eqPrev == 1, gStart[[q - 1]], q];
                    If[ ok == 1,
                        mq = 0;
                        Do[mq = BitOr[mq, BitShiftLeft[1, val[[(q - 1) * aa + i]] - 1]], {i, aa}];
                        emArr[[q]] = mq;
                        (* swap-with-future: every earlier prefix must stay atom-minimal against this edge *)
                        If[ q > 1,
                            Do[
                                If[ ok == 1,
                                    x1 = pmArr[[tp + 1]];
                                    x2 = BitOr[pmArr[[tp]], mq];
                                    pcA = popT[[BitAnd[x1, 65535] + 1]] + popT[[BitAnd[BitShiftRight[x1, 16], 65535] + 1]] + popT[[BitAnd[BitShiftRight[x1, 32], 65535] + 1]] + popT[[BitShiftRight[x1, 48] + 1]];
                                    pcB = popT[[BitAnd[x2, 65535] + 1]] + popT[[BitAnd[BitShiftRight[x2, 16], 65535] + 1]] + popT[[BitAnd[BitShiftRight[x2, 32], 65535] + 1]] + popT[[BitShiftRight[x2, 48] + 1]];
                                    If[pcA > pcB, ok = 0]
                                ],
                                {tp, 1, q - 1}
                            ]
                        ];
                        If[ok == 1, pmArr[[q + 1]] = BitOr[pmArr[[q]], mq]]
                    ];
                    (* adjacent-group-swap: when a group of equal edges closes, swapping it with the
                       preceding group must not yield a lexicographically smaller local pattern *)
                    If[ ok == 1 && q > 1,
                        Do[
                            doSwap = 0;
                            If[pass == 1 && eqPrev == 0 && gStart[[q - 1]] > 1, doSwap = 1; qq = q - 1];
                            If[pass == 2 && q == cc && gStart[[q]] > 1, doSwap = 1; qq = q];
                            If[ doSwap == 1,
                                s2 = gStart[[qq]]; s1 = gStart[[s2 - 1]];
                                mG2 = 0; Do[mG2 = BitOr[mG2, emArr[[i]]], {i, s2, qq}];
                                x1 = BitOr[pmArr[[s1]], mG2];
                                x2 = pmArr[[s2]];
                                pcA = popT[[BitAnd[x1, 65535] + 1]] + popT[[BitAnd[BitShiftRight[x1, 16], 65535] + 1]] + popT[[BitAnd[BitShiftRight[x1, 32], 65535] + 1]] + popT[[BitShiftRight[x1, 48] + 1]];
                                pcB = popT[[BitAnd[x2, 65535] + 1]] + popT[[BitAnd[BitShiftRight[x2, 16], 65535] + 1]] + popT[[BitAnd[BitShiftRight[x2, 32], 65535] + 1]] + popT[[BitShiftRight[x2, 48] + 1]];
                                If[ pcA == pcB,
                                    kc = 0; Do[rankC[[i]] = 0, {i, s + 1}];
                                    Do[x = val[[i]]; If[rankC[[x]] == 0, kc++; rankC[[x]] = kc], {i, 1, (s1 - 1) * aa}];
                                    Do[rank1[[i]] = rankC[[i]]; rank2[[i]] = rankC[[i]], {i, s + 1}];
                                    k1 = kc; jA = 0;
                                    Do[x = val[[i]]; If[rank1[[x]] == 0, k1++; rank1[[x]] = k1]; jA++; va[[jA]] = rank1[[x]], {i, (s1 - 1) * aa + 1, qq * aa}];
                                    k2 = kc; jB = 0;
                                    Do[x = val[[i]]; If[rank2[[x]] == 0, k2++; rank2[[x]] = k2]; jB++; vb[[jB]] = rank2[[x]], {i, (s2 - 1) * aa + 1, qq * aa}];
                                    Do[x = val[[i]]; If[rank2[[x]] == 0, k2++; rank2[[x]] = k2]; jB++; vb[[jB]] = rank2[[x]], {i, (s1 - 1) * aa + 1, (s2 - 1) * aa}];
                                    cmp = 0;
                                    Do[If[cmp == 0, If[va[[i]] > vb[[i]], cmp = 1, If[va[[i]] < vb[[i]], cmp = -1]]], {i, jA}];
                                    If[cmp == 1, ok = 0]
                                ]
                            ],
                            {pass, 1, 2}
                        ]
                    ];
                    (* connectivity feasibility bounds *)
                    If[ ok == 1 && maintainU == 1,
                        If[compsU - 1 > (cc - q) * (aa - 1) - (s - kb[[t + 1]]), ok = 0]
                    ];
                    If[ ok == 1 && maintainS == 1,
                        If[compsS - 1 > (cc - q) * (aa - 1), ok = 0]
                    ]
                ];
                If[ ok == 1,
                    If[ t == M,
                        good = 1;
                        If[maintainU == 1 && compsU != 1, good = 0];
                        If[checkLEnd == 1 && compsS != 1, good = 0];
                        If[checkREnd == 1 && compsS != 1, good = 0];
                        If[ good == 1,
                            Do[Internal`StuffBag[bag, prefix[[i]]], {i, Length[prefix]}];
                            Do[Internal`StuffBag[bag, val[[i]]], {i, M}]
                        ]
                        ,
                        t++; val[[t]] = 0
                    ]
                ]
            ]
        ];
        Internal`BagPart[bag, All]
    ],
    CompilationTarget -> target, RuntimeOptions -> "Speed"
]

$suffixDFSKernel := $suffixDFSKernel = enumCompile[makeSuffixDFSKernel]

(* enumerate assignments of the slots before the last part, applying the same necessary
   conditions (plus exact part and side checks at their boundaries); edges fill in canonical
   order, so part p' holds all its edges when p' < p, q edges when p' == p, and none after *)
enumerateRulePrefixes[st_Association, s_Integer, type_, cutLen_Integer] := Block[{
    arities = st["Arities"], sides = st["Sides"], parts = st["Parts"], L = st["L"], nE = st["NEdges"],
    typeConn = type =!= None, checkR = type === All,
    edgeStart, edgeEnd, partOfEdge, inPartIndex, lastEdgeOfPart, lastLHSEdge,
    remCapAll, remCapLHS, remCapRHS, remSlots, lhsPartIdx, rhsPartIdx,
    F, results, partEdges, partMasks, filledEdges, edgeCompleted, dfs
},
    edgeEnd = Accumulate[arities];
    edgeStart = edgeEnd - arities + 1;
    partOfEdge = Catenate @ Table[ConstantArray[p, parts[[p, "Count"]]], {p, Length[parts]}];
    inPartIndex = Catenate @ Table[Range @ parts[[p, "Count"]], {p, Length[parts]}];
    lastEdgeOfPart = parts[[All, "EdgeIndices", -1]];
    lastLHSEdge = Replace[Select[Range[nE], sides[[#]] == 1 && arities[[#]] > 0 &], {{} -> 0, l_List :> Last[l]}];
    remCapAll = Table[Sum[Max[arities[[e]] - 1, 0], {e, e0 + 1, nE}], {e0, 0, nE}];
    remCapLHS = Table[Sum[If[sides[[e]] == 1, Max[arities[[e]] - 1, 0], 0], {e, e0 + 1, nE}], {e0, 0, nE}];
    remCapRHS = Table[Sum[If[sides[[e]] == 2, Max[arities[[e]] - 1, 0], 0], {e, e0 + 1, nE}], {e0, 0, nE}];
    remSlots = Table[Sum[arities[[e]], {e, e0 + 1, nE}], {e0, 0, nE}];
    lhsPartIdx = Flatten @ Position[parts[[All, "Side"]], 1, {1}];
    rhsPartIdx = Flatten @ Position[parts[[All, "Side"]], 2, {1}];
    F = ConstantArray[0, Max[cutLen, 1]];
    results = Internal`Bag[];
    partEdges = Table[ConstantArray[{}, parts[[p, "Count"]]], {p, Length[parts]}];
    partMasks = Table[ConstantArray[0, parts[[p, "Count"]] + 1], {p, Length[parts]}];
    filledEdges = Function[{pp, p, q}, partEdges[[pp, 1 ;; Which[pp < p, parts[[pp, "Count"]], pp == p, q, True, 0]]]];
    edgeCompleted = Function[{t, e, k1}, Block[{edge = F[[edgeStart[[e]] ;; t]], p = partOfEdge[[e]], q = inPartIndex[[e]], ok = True, m, cL},
        If[q > 1 && MemberQ[partEdges[[p, 1 ;; q - 1]], edge] && partEdges[[p, q - 1]] =!= edge, ok = False];
        m = If[edge === {}, 0, Total[2 ^ (Union[edge] - 1)]];
        If[ ok && q > 1,
            Do[
                If[ DigitCount[partMasks[[p, tp + 1]], 2, 1] > DigitCount[BitOr[partMasks[[p, tp]], m], 2, 1],
                    ok = False;
                    Break[]
                ],
                {tp, 1, q - 1}
            ]
        ];
        If[ ok,
            partEdges[[p, q]] = edge;
            partMasks[[p, q + 1]] = BitOr[partMasks[[p, q]], m];
            If[ typeConn,
                If[hyperedgeComponentCount[Catenate @ Table[filledEdges[pp, p, q], {pp, Length[parts]}]] - 1 > remCapAll[[e + 1]] - (s - k1), ok = False];
                If[ ok,
                    cL = hyperedgeComponentCount[Catenate @ Table[filledEdges[pp, p, q], {pp, lhsPartIdx}]];
                    If[cL - 1 > remCapLHS[[e + 1]], ok = False];
                    If[ok && e == lastLHSEdge && cL != 1, ok = False]
                ];
                If[ok && checkR && hyperedgeComponentCount[Catenate @ Table[filledEdges[pp, p, q], {pp, rhsPartIdx}]] - 1 > remCapRHS[[e + 1]], ok = False]
            ];
            If[ok && s - k1 > remSlots[[e + 1]], ok = False];
            If[ok && e == lastEdgeOfPart[[p]] && ! miserMemberPatternQ[rgsRelabel @ Flatten[partEdges[[p]]], arities[[e]]], ok = False];
            If[ok, dfs[t + 1, k1]]
        ]
    ]];
    dfs = Function[{t, k0}, Block[{e, j, vals},
        If[ t > cutLen,
            Internal`StuffBag[results, {F[[1 ;; cutLen]], k0}]
            ,
            e = LengthWhile[edgeEnd, # < t &] + 1;
            j = t - edgeStart[[e]] + 1;
            If[ s - k0 <= L - t + 1,
                vals = Which[
                    s - k0 == L - t + 1, {k0 + 1},
                    k0 < s, Range[k0 + 1],
                    True, Range[k0]
                ];
                Do[
                    F[[t]] = v;
                    With[{k1 = If[v > k0, k0 + 1, k0]},
                        If[j < arities[[e]], dfs[t + 1, k1], edgeCompleted[t, e, k1]]
                    ],
                    {v, vals}
                ];
                F[[t]] = 0
            ]
        ]
    ]];
    dfs[1, 0];
    Internal`BagPart[results, All]
]

fastEnumerateWolframModelRules[signature_Rule, {s_Integer ? Positive, type : Automatic | None | All}] := Block[{
    ss = ReverseSortBy[#, Last] & /@ signature,
    st, L, parts, posParts, suffixPart, aa, cc, prefLen, typeCode, sideP,
    prefEdgeSel, prefArities, prefSides, partLens, partRanges, prefPosParts, prefixes, cands
},
    st = enumRuleStructure[ss];
    L = st["L"];
    If[s > L, Return[{}]];
    If[type =!= None && ! MemberQ[st["Sides"], 1], Return[{}]]; (* an empty side is never connected *)
    If[type === All && ! MemberQ[st["Sides"], 2], Return[{}]];
    parts = st["Parts"];
    posParts = Select[Range @ Length[parts], parts[[#, "Arity"]] > 0 &];
    If[posParts === {}, Return[{}]];
    suffixPart = parts[[Last[posParts]]];
    aa = suffixPart["Arity"]; cc = suffixPart["Count"];
    prefLen = L - aa * cc;
    typeCode = Replace[type, {None -> 0, Automatic -> 1, All -> 2}];
    sideP = suffixPart["Side"];
    prefEdgeSel = Select[Range @ st["NEdges"], ! MemberQ[suffixPart["EdgeIndices"], #] && st["Arities"][[#]] > 0 &];
    prefArities = st["Arities"][[prefEdgeSel]];
    prefSides = st["Sides"][[prefEdgeSel]];
    partLens = parts[[All, "Arity"]] parts[[All, "Count"]];
    partRanges = With[{ends = Accumulate[partLens]}, Transpose[{ends - partLens + 1, ends}]];
    prefPosParts = Most[posParts];
    prefixes = enumerateRulePrefixes[st, s, type, prefLen];
    cands = Internal`Bag[];
    Do[
        Block[{pref = prefEntry[[1]], raw, mat, suffPats, prefParts, prefAllSingleton},
            raw = $suffixDFSKernel[
                pref, prefArities, prefSides,
                aa, cc, sideP, s, prefEntry[[2]], typeCode,
                Boole[typeCode >= 1 && sideP == 1], Boole[typeCode == 2 && sideP == 2],
                $enumPopcountTable
            ];
            If[ Length[raw] > 0,
                mat = Partition[raw, L];
                suffPats = $relabelRowsKernel[mat, prefLen + 1, L];
                prefParts = Table[Partition[pref[[partRanges[[p, 1]] ;; partRanges[[p, 2]]]], parts[[p, "Arity"]]], {p, prefPosParts}];
                prefAllSingleton = AllTrue[prefParts, miserSingletonQ[rgsRelabel @ Flatten[#], Length @ First[#]] &];
                Do[
                    If[ miserMemberPatternQ[suffPats[[r]], aa],
                        If[ (prefAllSingleton && miserSingletonQ[suffPats[[r]], aa]) || miserProductMinimalQ[Append[prefParts, Partition[mat[[r, prefLen + 1 ;; L]], aa]], mat[[r]]],
                            Internal`StuffBag[cands, mat[[r]]]
                        ]
                    ],
                    {r, Length[mat]}
                ]
            ]
        ],
        {prefEntry, prefixes}
    ];
    cands = Internal`BagPart[cands, All];
    ApplyWolframRuleSignaturetoList[ss, #] & /@ cands[[Ordering[legacyEnumerationKey /@ cands]]]
]


(* An explicit integer s selects the rules with exactly s distinct atoms: the slices that
   EnumerateOrderedHypergraphs accumulates itself. Automatic matches the resource function
   default and returns all rules with up to maxConnectedAtoms[signature, type] atoms, in the
   same order as the reference enumeration. *)

EnumerateWolframModelRules[signature_, type : (Automatic | None | All) : Automatic] :=
    EnumerateWolframModelRules[signature, {Automatic, type}]

EnumerateWolframModelRules[signature_, s : _Integer ? Positive | Automatic : Automatic] := EnumerateWolframModelRules[signature, {s, Automatic}]

EnumerateWolframModelRules[signature_Rule, {Automatic, type : Automatic | None | All}] := With[{
    n = maxConnectedAtoms[signature, type]
},
    If[ ! IntegerQ[n] || n < 1,
        {},
        With[{rules = Catenate @ Table[EnumerateWolframModelRules[signature, {s, type}], {s, n}]},
            rules[[Ordering[legacyEnumerationKey[Flatten[List @@ #]] & /@ rules]]]
        ]
    ]
]

EnumerateWolframModelRules[signature_Rule, {s_Integer ? Positive, type : Automatic | None | All}] :=
    If[s > 62, slowEnumerateWolframModelRules, fastEnumerateWolframModelRules][signature, {s, type}] (* slow path beyond bitmask capacity *)
