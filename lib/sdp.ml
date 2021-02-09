open Angstrom

let equals = char '='

module P = struct
  let is_eol = function '\r' | '\n' -> true | _ -> false

  let is_space = function ' ' -> true | _ -> false

  let is_digit = function '0' .. '9' | '-' -> true | _ -> false

  let is_colon = function ':' -> true | _ -> false
end

let spaces = skip_while P.is_space

let digits = take_while1 P.is_digit

let colon = char ':'

let lex p = p <* spaces

let key k = char k *> equals

let value = take_till (fun p -> P.is_space p || P.is_eol p)

let value_list = many value

let key_value c = key c *> value

let key_value_list c = key c *> value_list

(* RFC 4456 *)

module Make (A : Types.Attribute) = struct
  module Types = Types.Make (A)

  module Encode = struct
    let originator ~username ~sess_id ~sess_version ~net_type ~addr_type
        ~unicast_addr =
      {
        Types.username;
        sess_id;
        sess_version;
        addr_type;
        net_type;
        unicast_addr;
      }

    let time ~active ?(repeats = []) () = { Types.active; repeats }

    let time_zone t = t

    let connection ~connection_net_type ~connection_addr_type ~connection_addr =
      { Types.connection_net_type; connection_addr_type; connection_addr }

    let media_field ~typ ~port ~proto ~fmts = { Types.typ; port; proto; fmts }

    let media ~media_field ?title ?connection ?media_bandwidth ?media_key
        ?(media_attributes = []) () =
      {
        Types.media_field;
        title;
        connection;
        media_bandwidth;
        media_key;
        media_attributes;
      }

    let v ~version ~originator ~session_name ?session_information ?uri ?email
        ?phone ?session_connection_information ?session_bandwidth ~time
        ?(time_zone_adjustments = []) ?session_key ~session_attributes ~media ()
        =
      {
        Types.version;
        originator;
        session_name;
        session_information;
        uri;
        email;
        phone;
        session_connection_information;
        session_bandwidth;
        time;
        time_zone_adjustments;
        session_key;
        session_attributes;
        media;
      }
  end

  module Pp = struct
    let pp_version ppf v = Fmt.pf ppf "v=%i\n" v

    let pp_originator ppf (v : Types.originator) =
      Fmt.pf ppf "o=%s %Ld %Ld %s %s %s\n" v.username v.sess_id v.sess_version
        v.net_type v.addr_type v.unicast_addr

    let pp_session_name ppf s = Fmt.pf ppf "s=%s\n" s

    let pp_time ppf (v : Types.time) =
      Fmt.pf ppf "t=%Ld %Ld\n" (fst v.active) (snd v.active)

    let pp_connection ppf (v : Types.connection) =
      Fmt.pf ppf "c=%s %s %s\n" v.connection_net_type v.connection_addr_type
        v.connection_addr

    let pp_timezone ppf =
      Fmt.pf ppf "z=%a\n"
        (Fmt.list (fun ppf (a, b) -> Fmt.pf ppf "%Ld %Ld" a b))

    let pp_attribute = A.pp

    let media_type_to_string = function
      | `Audio -> "audio"
      | `Video -> "video"
      | `Text -> "text"
      | `Application -> "application"
      | `Message -> "message"
      | `Other s -> s

    let pp_media_field ppf (t : Types.media_field) =
      let port_to_string = function
        | `Port (i, None) -> string_of_int i
        | `Port (i, Some j) -> string_of_int i ^ "/" ^ string_of_int j
      in
      Fmt.(
        pf ppf "m=%s %s %s %a\n"
          (media_type_to_string t.typ)
          (port_to_string t.port) t.proto (list string) t.fmts)

    let pp_media ppf (m : Types.media) =
      pp_media_field ppf m.media_field;
      Fmt.(pf ppf "%a" (option pp_connection) m.connection);
      Fmt.(
        pf ppf "%a"
          (list ~sep:(fun ppf () -> char ppf '\n') A.pp)
          m.media_attributes)

    let print_list f = function [] -> () | lst -> f lst

    let pp ppf (t : Types.t) =
      pp_version ppf t.version;
      pp_originator ppf t.originator;
      pp_session_name ppf t.session_name;
      Fmt.(pf ppf "%a" (list pp_time) t.time);
      print_list (pp_timezone ppf) t.time_zone_adjustments;
      Fmt.(pf ppf "%a" (list A.pp) t.session_attributes);
      Fmt.(pf ppf "%a" (list pp_media) t.media)
  end

  module Decode = struct
    let version =
      let* verison_number = key_value 'v' <* end_of_line in
      return (int_of_string verison_number)

    let originator =
      key 'o' *> lex value >>= fun username ->
      lex digits >>= fun sess_id ->
      lex digits >>= fun sess_version ->
      lex value >>= fun net_type ->
      lex value >>= fun addr_type ->
      lex value <* end_of_line >>= fun unicast_addr ->
      return
        {
          Types.username;
          sess_id = Int64.of_string sess_id;
          sess_version = Int64.of_string sess_version;
          net_type;
          addr_type;
          unicast_addr;
        }

    (* Argh...
       RFC 4566: 'Whitespace MUST NOT be used on either side of the "=" sign.'
       Also RFC 4566: If a session has no meaningful name, the value "s= " SHOULD be used (i.e., a single space as the session name)
    *)
    let session_name = key 's' *> take_till P.is_eol <* end_of_line

    (* Free-form text -- meant to be for humans, not for machines
       "It is not suitable for parsing by automata." *)

    let peek_opt c p =
      let some v = v >>= fun t -> return (Some t) in
      let* peek = peek_char_fail in
      if Char.equal peek c then some p else return None

    let opt_end c p = end_of_input *> return None <|> peek_opt c p

    let session_information =
      peek_opt 'i' (key 'i' *> take_till P.is_eol <* end_of_line)

    let uri = peek_opt 'u' (key_value 'u' <* end_of_line >>| Uri.of_string)

    let email =
      peek_opt 'e'
        ( key 'e' *> take_till P.is_eol <* end_of_line >>= fun email ->
          Emile.of_string email |> Result.get_ok |> return )

    let phone = peek_opt 'p' @@ (key_value 'p' <* end_of_line)

    let connection =
      let p =
        let* connection_net_type = key 'c' *> lex value in
        let* connection_addr_type = lex value in
        let* connection_addr = lex value <* end_of_line in
        return
          { Types.connection_net_type; connection_addr_type; connection_addr }
      in
      peek_opt 'c' p

    let bandwidth =
      let b =
        let* bw_type = key 'b' *> take_till P.is_colon in
        let* bw = colon *> digits in
        return { Types.bw_type; bw }
      in
      peek_opt 'b' b

    let active_time =
      let* start = key 't' *> digits in
      let* stop = spaces *> digits <* end_of_line in
      return (Int64.of_string start, Int64.of_string stop)

    let days = digits <* char 'd' >>| fun t -> Int64.(mul (of_string t) 86400L)

    let hours =
      digits <* char 'h' >>= fun t -> return Int64.(mul (of_string t) 3600L)

    let minutes =
      digits <* char 'm' >>= fun t -> return Int64.(mul (of_string t) 60L)

    let seconds = digits <* char 's' >>= fun t -> return Int64.(of_string t)

    let secs = digits >>= fun t -> return Int64.(of_string t)

    let time_to_secs = days <|> hours <|> minutes <|> seconds <|> secs

    let repeats =
      (* https://tools.ietf.org/html/rfc4566#section-5.10 *)
      let* interval = key 'r' *> time_to_secs in
      let* active_duration = spaces *> time_to_secs in
      let* offsets = spaces *> many_till (lex time_to_secs) end_of_line in
      return (interval, active_duration, offsets)

    let time_descriptions =
      let time_description =
        let* active = active_time in
        let* repeats =
          many_till repeats
            ( peek_char
            >>= (function
                  | Some 'm' | Some 't' | Some 'a' | Some 'z' -> return ()
                  | _ -> fail "Expected m, t, a or end_of_input")
            <|> end_of_input )
        in
        return { Types.active; repeats }
      in
      let* required = time_description in
      let* rest = end_of_input *> return [] <|> many time_description in
      return (required :: rest)

    let time_zone =
      let p =
        key 'z'
        *>
        let adjustment = secs in
        let offset = time_to_secs in
        many_till
          ( lex adjustment >>= fun a ->
            lex offset >>= fun o -> return (a, o) )
          end_of_line
      in
      opt_end 'z' p

    let encryption =
      let meth_key =
        let* meth = key 'k' *> take_till P.is_colon in
        let* k = char ':' *> take_till P.is_eol in
        match meth with
        | "clear" -> return (`Clear k)
        | "base64" -> return (`Base64 k)
        | "uri" -> return (`Uri (Uri.of_string k))
        | _ -> fail "Unknown key type"
      in
      let k =
        let* k' = key_value 'k' <* end_of_line in
        match k' with "prompt" -> return `Prompt | k -> return (`Other k)
      in
      peek_opt 'k' (meth_key <|> k)

    let attribute =
      let* repeats =
        many_till A.attribute
          ( peek_char
          >>= (function
                | Some 'm' -> return () | _ -> fail "Expected something else")
          <|> end_of_input )
      in
      return repeats

    let media_field =
      let media_of_string = function
        | "audio" -> `Audio
        | "video" -> `Video
        | "text" -> `Text
        | "application" -> `Application
        | "message" -> `Message
        | s -> `Other s
      in
      let* media = key 'm' *> lex value in
      let* port =
        let* port = lex value in
        match String.split_on_char '/' port with
        | [ port ] -> return (`Port (int_of_string port, None))
        | [ port; num ] ->
            return (`Port (int_of_string port, Some (int_of_string num)))
        | _ -> failwith "Malformed port"
      in
      let* proto = lex value in
      let* fmts =
        let fmt = lex value in
        many_till fmt end_of_line
      in
      return { Types.typ = media_of_string media; port; proto; fmts }

    let medias =
      let media =
        let* media_field = media_field in
        let* connection = end_of_input *> return None <|> connection in
        let* attr = attribute in
        Fmt.(
          pf stdout "ATTR:%a\n"
            (list ~sep:(fun ppf () -> char ppf ',') Pp.pp_attribute)
            attr);
        return
          {
            Types.media_field;
            title = None;
            connection;
            media_bandwidth = None;
            media_key = None;
            media_attributes = attr;
          }
      in
      many_till media end_of_input

    let sdp =
      let* version = version in
      let* originator = originator in
      let* session_name = session_name in
      let* session_information = session_information in
      let* uri = uri in
      let* email = email in
      let* phone = phone in
      let* session_connection_information = connection in
      let session_bandwidth = None in
      let* time = time_descriptions in
      let* time_zone_adjustments = time_zone in
      let* session_attributes = attribute in
      let* media = medias in
      return
        (Encode.v ~version ~originator ~session_name ?session_information
           ?session_connection_information ~session_attributes ~media ?uri
           ?email ?phone ?session_bandwidth ~time ?time_zone_adjustments ())
  end
end

module Simple_attribute = struct
  type t = [ `Prop of string | `Value of string * string ]

  let attribute : t Angstrom.t =
    let value =
      let* property = key 'a' *> take_till P.is_eol <* end_of_line in
      match String.split_on_char ':' property with
      | [] | [ _ ] -> return @@ `Prop property
      | [ key; value ] -> return @@ `Value (key, value)
      | key :: values -> return @@ `Value (key, String.concat ":" values)
    in
    value

  let pp ppf = function
    | `Prop p -> Fmt.pf ppf "a=%s" p
    | `Value (k, v) -> Fmt.pf ppf "a=%s:%s" k v
end

module Basic = Make (Simple_attribute)
