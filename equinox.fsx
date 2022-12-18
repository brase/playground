module Feeds
open System

module Events =   
    type FeedAdded =
        { Url: string
          Name: string }
    and EpisodeAdded =
        { Guid: string
          Url: string
          MediaUrl: string
          Title: string
          Length: int
          ReleaseDate: System.DateOnly
          Episode: int }
    type FeedEvents = 
    | FeedAdded of FeedAdded
    | EpisodeAdded of EpisodeAdded
    | FeedPaused
    | FeedContinued


module Fold =
    open Events

    type Episode =
        { Guid: string
          Url: string
          MediaUrl: string
          Title: string
          Length: int
          ReleaseDate: System.DateOnly
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
              Active { Url = ev.Url
                       Name = ev.Name
                       Episodes = [] }
            | ev -> failwithf "Unexpected %A" ev
        | Active feed ->
            match event with
            | EpisodeAdded ev ->
              Active { feed with Episodes = feed.Episodes @ []}
            | FeedPaused ->
              Paused feed
            | ev -> failwithf "Unexpected %A" ev
        | Paused feed ->
            match event with
            | FeedContinued ->
                Active feed
            | ev -> failwithf "Unexpected %A" ev




    type AddFeed =
        { Url: string
          Name: string
          Category: string }
    and AddEpisode =
        { Guid: string
          Url: string
          MediaUrl: string
          Title: string
          Length: int
          ReleaseDate: System.DateOnly
          Episode: int }

