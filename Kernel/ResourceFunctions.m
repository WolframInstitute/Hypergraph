Package["WolframInstitute`Hypergraph`"]

PackageExport["RandomHypergraph"]
PackageExport["EnumerateHypergraphs"]
PackageExport["RandomHypergraphRule"]
PackageExport["EnumerateHypergraphRules"]



RandomHypergraph[args___, opts : OptionsPattern[]] := Hypergraph[ResourceFunction["RandomHypergraph"][args], opts]

EnumerateHypergraphs[args___, opts : OptionsPattern[]] := Hypergraph[#, opts] & /@ ResourceFunction["EnumerateHypergraphs"][args]

RandomHypergraphRule[args___, opts : OptionsPattern[]] := HypergraphRule[##, opts] & @@ ResourceFunction["RandomWolframModel"][args]

EnumerateHypergraphRules[args___, opts : OptionsPattern[]] := HypergraphRule[##, opts] & @@@ ResourceFunction["EnumerateWolframModelRules"][args]

