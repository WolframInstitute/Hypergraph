Package["WolframInstitute`Hypergraph`"]

PackageExport["RandomHypergraph"]
PackageExport["RandomHypergraphRule"]
PackageExport["EnumerateHypergraphRules"]



RandomHypergraph[args___, opts : OptionsPattern[]] := Hypergraph[ResourceFunction["RandomHypergraph"][args], opts]

RandomHypergraphRule[args___, opts : OptionsPattern[]] := HypergraphRule[##, opts] & @@ ResourceFunction["RandomWolframModel"][args]

EnumerateHypergraphRules[args___, opts : OptionsPattern[]] := HypergraphRule[##, opts] & @@@ ResourceFunction["EnumerateWolframModelRules"][args]

