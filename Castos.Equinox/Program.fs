module Program


open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Serilog

module Environment =
    let tryGetEnv = Environment.GetEnvironmentVariable >> Option.ofObj

let log = LoggerConfiguration().WriteTo.Console().CreateLogger()

let store = Equinox.MemoryStore.VolatileStore()

let service =
    Equinox.MemoryStore.MemoryStoreCategory(store, Feeds.Events.codec, Feeds.Fold.fold, Feeds.Fold.initial)
    |> Equinox.Decider.resolve log
    |> Feeds.create

let builder = WebApplication.CreateBuilder()
let app = builder.Build()

let addFeed body =
    task {
        let id = Guid.NewGuid() |> Feeds.FeedId.ofGuid
        do! service.AddFeed(id, body)
        return id
    }

app.MapPost("/", Func<_, _>(addFeed)) |> ignore

let addEpisode id body =
    task {
        do! service.AddEpisode(id, body)
        return "OK"
    }

app.MapPost("/{id}/episodes", Func<_, _>(addEpisode)) |> ignore

app.Run()
