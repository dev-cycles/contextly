module Contextly.VsCodeExtension.Tests.Completion

open Fable.Mocha
open Fable.Core
open Fable.Import.VSCode
open Fable.Import.VSCode.Vscode
open Contextly.VsCodeExtension.Tests.Helpers

let private getLabels (items:ResizeArray<CompletionItem>) =
    items
    |> Seq.map (fun item ->
        match item.label with
        | U2.Case2 ll -> ll.label
        | U2.Case1 l -> l)

let tests =
    testList "Contextly Completion Tests" [

        testCaseAsync "Completion returns expected list" <| async {
            printfn "Starting Completion returns expected list"
            let testDocPath = "../test/fixtures/simple_workspace/test.txt"
            let! docUri = getDocUri testDocPath |> openDocument |> awaitP

            do! getLanguageClient() |> awaitP |> Async.Ignore 

            let! result = awaitP (VsCodeCommands.complete docUri <| vscode.Position.Create(0.0, 10.0))

            match result with
            | (Some completionResult: CompletionList option) ->
                let labels = getLabels completionResult.items
                let expected = seq {"context"; "definition"; "example"; "term"}
                Expect.seqEqual expected labels "executeCompletionProvider should return expected completion items"
            | None ->
                Expect.isSome result "executeCompletionItemProvider should return a result"

            do! getDocUri testDocPath |> closeDocument |> awaitP
            
            printfn "Ending Completion returns expected list"
        }
    ]