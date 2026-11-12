#!/usr/bin/env wolframscript
(* Submits Hypergraph to the Wolfram Paclet Repository with PacletPage's SubmitPacletResource: the
   definition notebook's own check, a fresh PacletBuild checked to hold every documentation page
   and to load in a fresh kernel, then the submission under the version in
   Hypergraph/PacletInfo.wl, which has to be newer than the repository's. Build the documentation
   first, with build.wls or release.wls.

       wl -f submit.wls              (or --dry-run: everything but the submission)

   The publisher, WolframInstitute, is nikm@wolfram.com's, the account it submits as unless
   PACLETPAGE_ACCOUNT names another; the wolframinstitute account is not authorized for it. *)

$ProgressReporting = False

Get[FileNameJoin[{DirectoryName[$InputFileName], "pacletpage.wl"}]]

account = Replace[Environment["PACLETPAGE_ACCOUNT"], Except[_String ? (StringLength[#] > 0 &)] -> "nikm@wolfram.com"]

result = SubmitPacletResource[$pacletDirectory,
    "Account" -> account,
    "DryRun" -> MemberQ[$ScriptCommandLine, "--dry-run"]
]

If[! AssociationQ[result], Exit[1]]

Print[result["Name"], " ", result["Version"], ": ", If[TrueQ[result["Submitted"]], "submitted", "not submitted (dry run)"]]

Print["archive: ", result["Archive"], ", ", result["Pages"], " documentation pages"]

If[KeyExistsQ[result, "Result"], Print["result: ", result["Result"]]]
