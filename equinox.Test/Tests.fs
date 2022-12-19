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
