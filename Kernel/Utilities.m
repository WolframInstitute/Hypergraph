Package["WolframInstitute`Hypergraph`"]

PackageExport[LinkedHypergraph]
PackageScope["areaGradientDescent"]



getLaplacian := getLaplacian = Quiet @ Block[{
    xx, x, PP, P, UU, U, f, Df, u, Du, g, integrand, quadraturepoints, quadratureweights
},
    xx = Table[Part[x, i], {i, 1, 2}];
    PP = Table[Indexed[P, {i, j}], {i, 1, 3}, {j, 1, 3}];
    UU = Table[Part[U, i], {i, 1, 3}];
    (*local affine parameterization of the surface with respect to the "standard triangle"*)
    f[x_] = PP[[1]] + x[[1]] (PP[[2]] - PP[[1]]) + x[[2]] (PP[[3]] - PP[[1]]);
    Df[x_] = D[f[xx], {xx}];
    (*the Riemannian pullback metric with respect to f*)
    g[x_] = Df[xx]\[Transpose] . Df[xx];
    (*an affine function u and its derivative*)
    u[x_] = UU[[1]] + x[[1]] (UU[[2]] - UU[[1]]) + x[[2]] (UU[[3]] - UU[[1]]);
    Du[x_] = D[u[xx], {xx}];
    integrand[x_] = 1/2 D[Du[xx] . Inverse[g[xx]] . Du[xx] Sqrt[Abs[Det[g[xx]]]], {{UU}, 2}];
    (*since the integrand is constant over each triangle,we use a one-
    point Gauss quadrature rule (for the standard triangle)*)
    quadraturepoints = {{1/3, 1/3}};
    quadratureweights = {1/2};
    With[{code = N[quadratureweights . Map[integrand, quadraturepoints]]},
        Compile[{{P, _Real, 2}},
            code,
            CompilationTarget -> "C",
            RuntimeAttributes -> {Listable}, Parallelization -> True,
            RuntimeOptions -> "Speed"
        ]
    ]
]

getLaplacianCombinatorics := getLaplacianCombinatorics = Quiet @ Module[{ff},
    With[{code = Flatten[Table[
        Table[{Indexed[ff, i], Indexed[ff, j]}, {i, 1, 3}], {j, 1, 3}], 1]},
        Compile[{{ff, _Integer, 1}}, code, CompilationTarget -> "C",
            RuntimeAttributes -> {Listable}, Parallelization -> True,
            RuntimeOptions -> "Speed"
        ]
    ]
]

LaplaceBeltrami[pts_, flist_, pat_] := With[{
  spopt = SystemOptions["SparseArrayOptions"],
  vals = Flatten[getLaplacian[Partition[pts[[flist]], 3]]]
},
    WithCleanup[
        SetSystemOptions["SparseArrayOptions" -> {"TreatRepeatedEntries" -> Total}],
        SparseArray[Rule[pat, vals], {Length[pts], Length[pts]}, 0.],
        SetSystemOptions[spopt]
    ]
]

areaGradientDescent[R_MeshRegion, stepsize_ : 1., steps_ : 10, reassemble_ : False] := Module[{
    method, faces, bndedges, bndvertices, pts, intvertices, pat, flist, A, S, solver
  },
    method = If[reassemble, "Pardiso", "Multifrontal"];
    pts = MeshCoordinates[R];
    faces = MeshCells[R, 2, "Multicells" -> True][[1, 1]];
    bndedges = Developer`ToPackedArray[Region`InternalBoundaryEdges[R][[All, 1]]];
    bndvertices = Union @@ bndedges;
    intvertices = Complement[Range[Length[pts]], bndvertices];
    pat = Flatten[getLaplacianCombinatorics[faces], 1];
    flist = Flatten[faces];
    Do[
      A = LaplaceBeltrami[pts, flist, pat];
      If[reassemble || i == 1, solver = LinearSolve[A[[intvertices, intvertices]], Method -> method]];
      pts[[intvertices]] -= stepsize * (solver[(A . pts)[[intvertices]]]);, {i, 1, steps}];
   S = MeshRegion[pts, MeshCells[R, 2], PlotTheme -> "LargeMesh"];
   S
]


Options[LinkedHypergraph] = Join[{
    "NodeStyle" -> {FontSize -> 16, FontColor -> Black},
    "NodeFrameStyle" -> {
        Background -> LightBlue,
        FrameMargins -> {4 {1, 1}, {1, 1}},
        FrameStyle -> RGBColor[0.34, 0.39, 0.55, .5],
        RoundingRadius -> {5, 10}
    }}, Options[Hypergraph]]

LinkedHypergraph[hg_, opts : OptionsPattern[]] := With[{
    nodeStyle = OptionValue["NodeStyle"],
    nodeFrameStyle = OptionValue["NodeFrameStyle"]
},
    Hypergraph[MapAt[
            Annotation[HoldForm[#], {
                VertexLabels -> None,
                VertexShapeFunction -> Function[Inset[Framed[Style[#2, nodeStyle], nodeFrameStyle], #1, #3]]}
            ] &,
            hg,
            {All, 2}
        ],
        Sequence @@ FilterRules[{opts}, Options[Hypergraph]],
        PlotTheme -> "WolframModel",
        "EdgeSymmetry" -> "Ordered",
        "EdgeLineStyle" -> Directive[Arrowheads[{{0.03, .5}}], Opacity[.7], Hue[0.63, 0.7, 0.5]], VertexLabelStyle -> _ -> FontSize -> 8, VertexLabels -> Automatic
    ]
]

