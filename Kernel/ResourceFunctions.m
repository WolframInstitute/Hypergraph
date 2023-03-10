Package["WolframInstitute`Hypergraph`"]

PackageExport["RandomHypergraph"]
PackageExport["EnumerateHypergraphs"]



RandomHypergraph[args___, opts : OptionsPattern[]] := Hypergraph[ResourceFunction["RandomHypergraph"][args], opts]

EnumerateHypergraphs[args___, opts : OptionsPattern[]] := Hypergraph[#, opts] & /@ ResourceFunction["EnumerateHypergraphs"][args]

