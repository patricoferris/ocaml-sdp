open Angstrom
module Sdp = Sdp.Basic

let eval f (str : string) =
  match parse_string ~consume:All f str with
  | Ok v -> v
  | Error msg -> failwith msg

let handle_error = function Ok t -> t | Error (`Msg m) -> failwith m

let test_version () =
  let correct = 0 in
  let actual = eval Sdp.Decode.version "v=0\n" in
  Alcotest.(check int) "same version" correct actual

let test_originator () =
  let test = "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5\n" in
  let correct =
    Sdp.Encode.originator ~username:"jdoe" ~sess_id:2890844526L
      ~sess_version:2890842807L ~net_type:"IN" ~addr_type:"IP4"
      ~unicast_addr:"10.47.16.5"
  in
  let actual = eval Sdp.Decode.originator test in
  Alcotest.(check @@ of_pp Sdp.Pp.pp_originator)
    "same originator" correct actual

let test_session_name () =
  let correct = "SDP Seminar" in
  let actual = eval Sdp.Decode.session_name "s=SDP Seminar\n" in
  Alcotest.(check string) "same session name" correct actual;
  let correct = "-" in
  let actual = eval Sdp.Decode.session_name "s=-\n" in
  Alcotest.(check string) "same session name (empty hyphen)" correct actual

let test_session_information () =
  let test = "i=A Seminar on the session description protocol\n" in
  let correct = Some "A Seminar on the session description protocol" in
  let actual = eval Sdp.Decode.session_information test in
  Alcotest.(check (option string)) "same session information" correct actual

let test_uri () =
  let test = "u=http://www.example.com/seminars/Sdp.Decode.pdf\n" in
  let correct =
    Some (Uri.of_string "http://www.example.com/seminars/Sdp.Decode.pdf")
  in
  let actual = eval Sdp.Decode.uri test in
  Alcotest.(check @@ option (of_pp Uri.pp)) "same uri" correct actual

let test_connection () =
  let test = "c=IN IP4 224.2.17.12/127\n" in
  let correct =
    Some
      (Sdp.Encode.connection ~connection_net_type:"IN"
         ~connection_addr_type:"IP4" ~connection_addr:"224.2.17.12/127")
  in
  let actual = eval Sdp.Decode.connection test in
  Alcotest.(check @@ option (of_pp Sdp.Pp.pp_connection))
    "same connection information" correct actual

let test_attribute_prop () =
  let test_prop = "a=prop\n" in
  let correct = [ `Prop "prop" ] in
  let actual = eval Sdp.Decode.attribute test_prop in
  Alcotest.(check @@ of_pp (Fmt.list Sdp.Pp.pp_attribute))
    "Same attribute (property)" correct actual

let test_attribute_value () =
  let test_value = "a=key:value\n" in
  let correct = [ `Value ("key", "value") ] in
  let actual = eval Sdp.Decode.attribute test_value in
  Alcotest.(check @@ of_pp (Fmt.list Sdp.Pp.pp_attribute))
    "Same attribute (value)" correct actual

let test_multiple_attributes () =
  let test =
    "a=fingerprint:sha-256 \
     0F:74:31:25:CB:A2:13:EC:28:6F:6D:2C:61:FF:5D:C2:BC:B9:DB:3D:98:14:8D:1A:BB:EA:33:0C:A4:60:A8:8E\n\
     a=group:BUNDLE 0 1\n"
  in
  let correct =
    [
      `Value
        ( "fingerprint",
          "sha-256 \
           0F:74:31:25:CB:A2:13:EC:28:6F:6D:2C:61:FF:5D:C2:BC:B9:DB:3D:98:14:8D:1A:BB:EA:33:0C:A4:60:A8:8E"
        );
      `Value ("group", "BUNDLE 0 1");
    ]
  in
  let actual = eval Sdp.Decode.attribute test in
  Alcotest.(check @@ of_pp (Fmt.list Sdp.Pp.pp_attribute))
    "Same attribute (value)" correct actual

let test_multiple_attributes_2 () =
  let test =
    "a=setup:active\n\
     a=mid:0\n\
     a=ice-ufrag:CsxzEWmoKpJyscFj\n\
     a=ice-pwd:mktpbhgREmjEwUFSIJyPINPUhgDqJlSd\n\
     a=end-of-candidates\n"
  in
  let correct =
    [
      `Value ("setup", "active");
      `Value ("mid", "0");
      `Value ("ice-ufrag", "CsxzEWmoKpJyscFj");
      `Value ("ice-pwd", "mktpbhgREmjEwUFSIJyPINPUhgDqJlSd");
      `Prop "end-of-candidates";
    ]
  in
  let actual = eval Sdp.Decode.attribute test in
  Alcotest.(check @@ of_pp (Fmt.list Sdp.Pp.pp_attribute))
    "Same attribute (value)" correct actual

(* Time Descriptions *)
let test_time_no_repeats () =
  let test = "t=2873397496 2873404696\n" in
  let correct = (2873397496L, 2873404696L) in
  let actual = eval Sdp.Decode.active_time test in
  Alcotest.(check @@ of_pp (fun ppf (a, b) -> Fmt.pf ppf "%Ld %Ld" a b))
    "Same time (no repeats)" correct actual

let test_time_continues () =
  let test = "t=2873397496 2873404696\nk=prompt\n" in
  let correct = ((2873397496L, 2873404696L), Some `Prompt) in
  let actual =
    eval
      ( Sdp.Decode.active_time >>= fun t ->
        Sdp.Decode.encryption >>= fun k -> return (t, k) )
      test
  in
  Alcotest.(check @@ of_pp (fun ppf ((a, b), _) -> Fmt.pf ppf "%Ld %Ld" a b))
    "Same time (no repeats)" correct actual

let test_repeats () =
  let test = "r=604800 3600 0 90000\n" in
  let correct = (604800L, 3600L, [ 0L; 90000L ]) in
  let pretty ppf (a, b, c) =
    Fmt.pf ppf "%Ld %Ld %a" a b (Fmt.list (fun ppf -> Fmt.pf ppf "%Ld")) c
  in
  let actual = eval Sdp.Decode.repeats test in
  Alcotest.(check @@ of_pp pretty) "Same time (no repeats)" correct actual

let test_time_one_repeat () =
  (* Time descriptions end when we move on to the media section, hence the 'm' *)
  let test = "t=2873397496 2873404696\nr=604800 3600 0 90000\n" in
  let correct =
    [
      Sdp.Encode.time ~active:(2873397496L, 2873404696L)
        ~repeats:[ (604800L, 3600L, [ 0L; 90000L ]) ]
        ();
    ]
  in

  let actual = eval Sdp.Decode.time_descriptions test in
  Alcotest.(check @@ of_pp (Fmt.list Sdp.Pp.pp_time))
    "Same time (one repeat)" correct actual

let test_time_two_repeats () =
  (* Time descriptions end when we move on to the media section, hence the 'm' *)
  let test =
    "t=2873397496 2873404696\nr=604800 3600 0 90000\nr=604800 3600 0 90000\n"
  in
  let correct =
    [
      Sdp.Encode.time ~active:(2873397496L, 2873404696L)
        ~repeats:
          [ (604800L, 3600L, [ 0L; 90000L ]); (604800L, 3600L, [ 0L; 90000L ]) ]
        ();
    ]
  in
  let actual = eval Sdp.Decode.time_descriptions test in
  Alcotest.(check @@ of_pp (Fmt.list Sdp.Pp.pp_time))
    "Same time (two repeats)" correct actual

let test_multiple_times_and_repeats () =
  let test =
    "t=2873397496 2873404696\n\
     r=604800 3600 0 90000\n\
     t=2873397496 2873404696\n\
     r=604800 1h 0 90000\n"
  in
  let correct =
    [
      Sdp.Encode.time ~active:(2873397496L, 2873404696L)
        ~repeats:[ (604800L, 3600L, [ 0L; 90000L ]) ]
        ();
      Sdp.Encode.time ~active:(2873397496L, 2873404696L)
        ~repeats:[ (604800L, 3600L, [ 0L; 90000L ]) ]
        ();
    ]
  in
  let actual = eval Sdp.Decode.time_descriptions test in
  Alcotest.(check @@ of_pp (Fmt.list Sdp.Pp.pp_time))
    "Same times (two times with one repeat each)" correct actual

let test_time_zone () =
  let test = "z=2882844526 -1h 2898848070 0\n" in
  let correct = Some [ (2882844526L, -3600L); (2898848070L, 0L) ] in
  let actual = eval Sdp.Decode.time_zone test in
  Alcotest.(check @@ option (of_pp Sdp.Pp.pp_timezone))
    "Same timezone adjustments" correct actual

let test_optional () =
  let test = "s=Session\nu=http://www.example.com/seminars/Sdp.Decode.pdf\n" in
  let correct =
    ( "Session",
      None,
      Some (Uri.of_string "http://www.example.com/seminars/Sdp.Decode.pdf") )
  in
  let pretty ppf (a, b, c) =
    Fmt.(pf ppf "(%s, %a, %a)" a (option string) b (option Uri.pp) c)
  in
  let actual =
    eval
      ( Sdp.Decode.session_name >>= fun s ->
        Sdp.Decode.session_information >>= fun t ->
        Sdp.Decode.uri >>= fun u -> return (s, t, u) )
      test
  in
  Alcotest.(check @@ of_pp pretty)
    "Should skip session information returning none" correct actual

let test_media () =
  let test = "m=audio 9 UDP/TLS/RTP/SAVPF 111\n" in
  let correct =
    Sdp.Encode.media_field ~typ:`Audio
      ~port:(`Port (9, None))
      ~proto:"UDP/TLS/RTP/SAVPF" ~fmts:[ "111" ]
  in
  let actual = eval Sdp.Decode.media_field test in
  Alcotest.(check @@ of_pp Sdp.Pp.pp_media_field)
    "Same media field" correct actual

let test_media_ports () =
  let test = "m=video 4768/3 UDP/TLS/RTP/SAVPF 111\n" in
  let correct =
    Sdp.Encode.media_field ~typ:`Video
      ~port:(`Port (4768, Some 3))
      ~proto:"UDP/TLS/RTP/SAVPF" ~fmts:[ "111" ]
  in
  let actual = eval Sdp.Decode.media_field test in
  Alcotest.(check @@ of_pp Sdp.Pp.pp_media_field)
    "Same media field" correct actual

let test_medias () =
  let test =
    "m=audio 9 UDP/TLS/RTP/SAVPF 111\n\
     c=IN IP4 0.0.0.0\n\
     a=setup:active\n\
     a=mid:0\n\
     a=ice-ufrag:CsxzEWmoKpJyscFj\n\
     a=ice-pwd:mktpbhgREmjEwUFSIJyPINPUhgDqJlSd\n\
     a=end-of-candidates\n\
     m=video 9 UDP/TLS/RTP/SAVPF 96\n\
     a=setup:active\n"
  in
  let media_field_audio =
    Sdp.Encode.media_field ~typ:`Audio
      ~port:(`Port (9, None))
      ~proto:"UDP/TLS/RTP/SAVPF" ~fmts:[ "111" ]
  in
  let media_field_video =
    Sdp.Encode.media_field ~typ:`Video
      ~port:(`Port (9, None))
      ~proto:"UDP/TLS/RTP/SAVPF" ~fmts:[ "96" ]
  in
  let connection =
    Sdp.Encode.connection ~connection_net_type:"IN" ~connection_addr_type:"IP4"
      ~connection_addr:"0.0.0.0"
  in
  let correct =
    [
      Sdp.Encode.media ~media_field:media_field_audio ~connection
        ~media_attributes:
          [
            `Value ("setup", "active");
            `Value ("mid", "0");
            `Value ("ice-ufrag", "CsxzEWmoKpJyscFj");
            `Value ("ice-pwd", "mktpbhgREmjEwUFSIJyPINPUhgDqJlSd");
            `Prop "end-of-candidates";
          ]
        ();
      Sdp.Encode.media ~media_field:media_field_video
        ~media_attributes:
          [
            `Value ("setup", "active");
            (* `Value ("rtpmap", "96 VP8/90000");
               `Prop "end-of-candidates"; *)
          ]
        ();
    ]
  in
  let actual = eval Sdp.Decode.medias test in
  Alcotest.(check @@ of_pp (Fmt.list Sdp.Pp.pp_media))
    "Same media field" correct actual

let test_sdp_no_media () =
  let test =
    "v=0\n\
     o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5\n\
     s=SDP Seminar\n\
     i=A Seminar on the session description protocol\n\
     u=http://www.example.com/seminars/Sdp.Decode.pdf\n\
     e=j.doe@example.com (Jane Doe)\n\
     c=IN IP4 224.2.17.12/127\n\
     t=2873397496 2873404696\n\
     z=2882844526 -1h 2898848070 0\n"
  in
  let originator =
    Sdp.Encode.originator ~username:"jdoe" ~sess_id:2890844526L
      ~sess_version:2890842807L ~net_type:"IN" ~addr_type:"IP4"
      ~unicast_addr:"10.47.16.5"
  in
  let correct =
    Sdp.Encode.v ~version:0 ~originator ~session_name:"SDP Seminar"
      ~session_information:"A Seminar on the session description protocol"
      ~time:[ Sdp.Encode.time ~active:(2873397496L, 2873404696L) () ]
      ~email:(Emile.of_string "j.doe@example.com (Jane Doe)" |> Result.get_ok)
      ~uri:(Uri.of_string "http://www.example.com/seminars/Sdp.Decode.pdf")
      ~time_zone_adjustments:[ (2882844526L, -3600L); (2898848070L, 0L) ]
      ~session_connection_information:
        (Sdp.Encode.connection ~connection_net_type:"IN"
           ~connection_addr_type:"IP4" ~connection_addr:"224.2.17.12/127")
      ~session_attributes:[] ~media:[] ()
  in
  let actual = eval Sdp.Decode.sdp test in
  Alcotest.(check @@ of_pp Sdp.Pp.pp) "Same SDP (no media)" correct actual

(* WebRTC Example Signal https://webrtcforthecurious.com/docs/02-signaling/ *)
let test_webrtc_small_example () =
  let test =
    "v=0\n\
     o=- 3546004397921447048 1596742744 IN IP4 0.0.0.0\n\
     s=-\n\
     t=0 0\n\
     a=fingerprint:sha-256 \
     0F:74:31:25:CB:A2:13:EC:28:6F:6D:2C:61:FF:5D:C2:BC:B9:DB:3D:98:14:8D:1A:BB:EA:33:0C:A4:60:A8:8E\n"
  in
  let originator =
    Sdp.Encode.originator ~username:"-" ~sess_id:3546004397921447048L
      ~sess_version:1596742744L ~net_type:"IN" ~addr_type:"IP4"
      ~unicast_addr:"0.0.0.0"
  in
  let correct =
    Sdp.Encode.v ~version:0 ~originator ~session_name:"-"
      ~time:[ Sdp.Encode.time ~active:(0L, 0L) () ]
      ~session_attributes:
        [
          `Value
            ( "fingerprint",
              "sha-256 \
               0F:74:31:25:CB:A2:13:EC:28:6F:6D:2C:61:FF:5D:C2:BC:B9:DB:3D:98:14:8D:1A:BB:EA:33:0C:A4:60:A8:8E"
            );
        ]
      ~media:[] ()
  in
  let actual = eval Sdp.Decode.sdp test in
  Alcotest.(check @@ of_pp Sdp.Pp.pp) "Same SDP (WebRTC Example)" correct actual

(* Stripped down *)
let test_webrtc_full_example () =
  let test =
    "v=0\n\
     o=- 3546004397921447048 1596742744 IN IP4 0.0.0.0\n\
     s=-\n\
     t=0 0\n\
     a=fingerprint:sha-256 \
     0F:74:31:25:CB:A2:13:EC:28:6F:6D:2C:61:FF:5D:C2:BC:B9:DB:3D:98:14:8D:1A:BB:EA:33:0C:A4:60:A8:8E\n\
     a=group:BUNDLE 0 1\n\
     m=audio 9 UDP/TLS/RTP/SAVPF 111\n\
     c=IN IP4 0.0.0.0\n\
     a=setup:active\n\
     a=mid:0\n\
     a=ice-ufrag:CsxzEWmoKpJyscFj\n\
     a=ice-pwd:mktpbhgREmjEwUFSIJyPINPUhgDqJlSd\n\
     a=end-of-candidates\n\
     m=video 9 UDP/TLS/RTP/SAVPF 96\n\
     a=setup:active\n\
     a=rtpmap:96 VP8/90000\n\
     a=end-of-candidates\n"
  in
  let originator =
    Sdp.Encode.originator ~username:"-" ~sess_id:3546004397921447048L
      ~sess_version:1596742744L ~net_type:"IN" ~addr_type:"IP4"
      ~unicast_addr:"0.0.0.0"
  in
  let media_field_audio =
    Sdp.Encode.media_field ~typ:`Audio
      ~port:(`Port (9, None))
      ~proto:"UDP/TLS/RTP/SAVPF" ~fmts:[ "111" ]
  in
  let media_field_video =
    Sdp.Encode.media_field ~typ:`Video
      ~port:(`Port (9, None))
      ~proto:"UDP/TLS/RTP/SAVPF" ~fmts:[ "96" ]
  in
  let connection =
    Sdp.Encode.connection ~connection_net_type:"IN" ~connection_addr_type:"IP4"
      ~connection_addr:"0.0.0.0"
  in
  let correct =
    Sdp.Encode.v ~version:0 ~originator ~session_name:"-"
      ~time:[ Sdp.Encode.time ~active:(0L, 0L) () ]
      ~session_attributes:
        [
          `Value
            ( "fingerprint",
              "sha-256 \
               0F:74:31:25:CB:A2:13:EC:28:6F:6D:2C:61:FF:5D:C2:BC:B9:DB:3D:98:14:8D:1A:BB:EA:33:0C:A4:60:A8:8E"
            );
          `Value ("group", "BUNDLE 0 1");
        ]
      ~media:
        [
          Sdp.Encode.media ~media_field:media_field_audio ~connection
            ~media_attributes:
              [
                `Value ("setup", "active");
                `Value ("mid", "0");
                `Value ("ice-ufrag", "CsxzEWmoKpJyscFj");
                `Value ("ice-pwd", "mktpbhgREmjEwUFSIJyPINPUhgDqJlSd");
                `Prop "end-of-candidates";
              ]
            ();
          Sdp.Encode.media ~media_field:media_field_video
            ~media_attributes:
              [
                `Value ("setup", "active");
                `Value ("rtpmap", "96 VP8/90000");
                `Prop "end-of-candidates";
              ]
            ();
        ]
      ()
  in
  let actual = eval Sdp.Decode.sdp test in
  Alcotest.(check @@ of_pp Sdp.Pp.pp) "Same SDP (WebRTC Example)" correct actual

let () =
  let open Alcotest in
  run "session-description-protocol-RFC-4456"
    [
      ( "simple",
        [
          test_case "Version" `Quick test_version;
          test_case "Originator" `Quick test_originator;
          test_case "Session Name" `Quick test_session_name;
          test_case "Session Information" `Quick test_session_information;
          test_case "Connection Information" `Quick test_connection;
          test_case "URI" `Quick test_uri;
          test_case "Option" `Quick test_optional;
          test_case "Media field" `Quick test_media;
          test_case "Medial field (port nums)" `Quick test_media_ports;
          test_case "Multiple media" `Quick test_medias;
        ] );
      ( "attributes",
        [
          test_case "Attributes (prop)" `Quick test_attribute_prop;
          test_case "Attributes (value)" `Quick test_attribute_value;
          test_case "Attributes (multiple)" `Quick test_multiple_attributes;
          test_case "Attributes 2 (multiple)" `Quick test_multiple_attributes_2;
        ] );
      ( "time",
        [
          test_case "Time (no repeats)" `Quick test_time_no_repeats;
          test_case "Time (no repeats -- but continues)" `Quick
            test_time_continues;
          test_case "Repeats in isolation" `Quick test_repeats;
          test_case "Time (one repeat)" `Quick test_time_one_repeat;
          test_case "Time (two repeats)" `Quick test_time_two_repeats;
          test_case "Time (two times)" `Quick test_multiple_times_and_repeats;
          test_case "Timezone Adjustements" `Quick test_time_zone;
        ] );
      ( "sdp",
        [
          test_case "Sdp (no media)" `Quick test_sdp_no_media;
          test_case "Sdp (webrtc small)" `Quick test_webrtc_small_example;
          test_case "Sdp (webrtc full)" `Quick test_webrtc_full_example;
        ] );
    ]
