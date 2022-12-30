module Tests

open Feeds
open Swensen.Unquote
open FsCheck.Xunit
open FsCodec.Core

[<Property>]
let ``The event codec round-trips cleanly`` event =
  let encoded = Events.codec.Encode(None, event)
  let saved = TimelineEvent.Create(0L, encoded.EventType, encoded.Data)
  let decoded: Events.FeedEvents option = Events.codec.TryDecode(saved)
  test <@ Some event = decoded @>


let (=>) events interpret =
  Fold.fold Fold.initial events |> interpret

open Feeds.Events

[<Property>]
let ``Adding a feed`` data =
  test <@ [] => Decisions.addFeed data = [ FeedAdded data ] @>
  //test for idempotency
  test <@ [ FeedAdded data ] => Decisions.addFeed data = [] @>
  //a paused feed will throw
  raises <@ [FeedAdded data; FeedPaused] => Decisions.addFeed data = [] @>

[<Property>]
let ``Adding an episode`` added data =
  test <@ [FeedAdded added] => Decisions.addEpisode data = [ EpisodeAdded data]  @>
  test <@ [FeedAdded added; EpisodeAdded data] => Decisions.addEpisode data = [ ]  @>
  raises <@ [FeedAdded added; FeedPaused] => Decisions.addEpisode data = [] @>
  raises <@ [] => Decisions.addEpisode data = [] @>
