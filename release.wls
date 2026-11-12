#!/usr/bin/env wolframscript
(* Releases Hypergraph with PacletPage's PublishPacletPage, in one kernel and so one cloud account:
   builds the documentation as build.wls does, deploys the paclet resource, publishes the
   documentation site around it with the authoring notebooks as its sources, and checks the site.
   "Submit" -> True would also submit it to the Paclet Repository.

       wl -f release.wls             (or --dry-run: the build and the resource, not deployed)

   The account is PACLETPAGE_ACCOUNT, by default the Wolfram Institute's. *)

$ProgressReporting = False

Get[FileNameJoin[{DirectoryName[$InputFileName], "pacletpage.wl"}]]

account = Replace[Environment["PACLETPAGE_ACCOUNT"], Except[_String ? (StringLength[#] > 0 &)] -> "wolframinstitute@wolframinstitute.org"]

result = PublishPacletPage[$pacletDirectory, "Hypergraph",
    "Account" -> account,
    "DryRun" -> MemberQ[$ScriptCommandLine, "--dry-run"],
    "Monitor" -> True,
    "BuildOptions" -> $documentationOptions,
    "SiteOptions" -> {"FeedbackOptions" -> {"Domains" -> {"wolframinstitute.org", "wolfram.com"}}, "VersionLabel" -> True, "Footer" -> True}
]

If[! AssociationQ[result], Exit[1]]

Print["documentation: ", Length[result["Build"]["Notebooks"]], " pages, failed ", result["Build"]["Failed"]]

Print["resource: ", result["Resource"]["Resource"], If[result["Resource"]["Deployed"], "", " (dry run)"]]

If[AssociationQ[result["Site"]], Print["site: ", result["Site"]["Site"]]]

If[ AssociationQ[result["Check"]],
    Scan[Print["check failed: ", #["Check"], ": ", #["Detail"]] &, Select[Normal[result["Check"]["Checks"]], ! TrueQ[#["OK"]] &]];
    Print["check: ", If[TrueQ[result["Check"]["OK"]], "everything is reachable", "problems found"]];
    Exit[If[TrueQ[result["Check"]["OK"]], 0, 1]]
]
