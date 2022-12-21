module Feeds

open System

module Events =
    type FeedAdded = { Url: string; Name: string }

    and EpisodeAdded =
        { Guid: string
          Url: string
          MediaUrl: string
          Title: string
          Length: int
          ReleaseDate: System.DateTime
          Episode: int }

    type FeedEvents =
        | FeedAdded of FeedAdded
        | EpisodeAdded of EpisodeAdded
        | FeedPaused
        | FeedContinued

        interface TypeShape.UnionContract.IUnionContract

    let codec = FsCodec.SystemTextJson.Codec.Create<FeedEvents>()

module Fold =
    open Events

    type Episode =
        { Guid: string
          Url: string
          MediaUrl: string
          Title: string
          Length: int
          ReleaseDate: System.DateTime
          Episode: int }

    and State =
        { Url: string
          Name: string
          Episodes: Episode list }

    and Feed =
        | Initial
        | Active of State
        | Paused of State


    let initial = Initial

    let evolve state event =
        match state with
        | Initial ->
            match event with
            | FeedAdded ev ->
                Active
                    { Url = ev.Url
                      Name = ev.Name
                      Episodes = [] }
            | ev -> failwithf "Unexpected %A" ev
        | Active feed ->
            match event with
            | EpisodeAdded ev -> Active { feed with Episodes = feed.Episodes @ [] }
            | FeedPaused -> Paused feed
            | ev -> failwithf "Unexpected %A" ev
        | Paused feed ->
            match event with
            | FeedContinued -> Active feed
            | ev -> failwithf "Unexpected %A" ev

    let fold: Feed -> FeedEvents seq -> Feed = Seq.fold evolve

module Decisions =
    let addFeed data state =
        match state with
        | Fold.Initial -> [ Events.FeedAdded data ]
        | Fold.Active state when state.Url = data.Url -> []
        | _ -> failwith "Feed already exists"

    let addEpisode data state =
        match state with
        | Fold.Active state -> [ Events.EpisodeAdded data ]
        | Fold.Initial -> failwith "Feed not found"
        | Fold.Paused _ -> failwith "Feed is paused"
