module Contextive.Cloud.LambdaFunctions

open System
open Amazon.CDK
open Amazon.CDK.AWS.Lambda
open Amazon.CDK.AWS.Logs
open Amazon.CDK.AWS.Events
open Amazon.CDK.AWS.S3

let props construct name entryPointModule (definitions: IBucket) (eventBus: IEventBus) =
    let assemblyPath relativePath =
        let basePath =
            System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)

        System.IO.Path.Combine(basePath, relativePath)

    FunctionProps(
        Runtime = Runtime.DOTNET_8,
        Code = Code.FromAsset(assemblyPath "../../../../Contextive.Cloud.Api/bin/Release/net8.0/linux-x64/publish"),
        Handler = $"Contextive.Cloud.Api::{entryPointModule}::FunctionHandlerAsync",
        Description = $"Contextive {name}",
        MemorySize = Nullable<float>(256.0),
        Timeout = Duration.Seconds(15),
        Environment =
            Map
                [ "DEFINITIONS_BUCKET_NAME", definitions.BucketName
                  "SLACK_OAUTH_TOKEN", (System.Environment.GetEnvironmentVariable("SLACK_OAUTH_TOKEN"))
                  "IS_LOCAL", (Context.isLocal construct).ToString()
                  "EVENT_BUS_NAME", eventBus.EventBusName ],
        LogRetention = Option.toNullable (Some RetentionDays.ONE_WEEK)
    )
