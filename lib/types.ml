module type Attribute = sig
  type t

  val attribute : t Angstrom.t

  val pp : t Fmt.t
end

module Make (A : Attribute) = struct
  type t = {
    version : int;
    originator : originator;
    session_name : string;
    session_information : string option;
    uri : Uri.t option;
    email : Emile.mailbox option;
    phone : string option;
    session_connection_information : connection option;
    session_bandwidth : bandwidth option;
    time : time list;
    time_zone_adjustments : time_zone list;
    session_key : key option;
    session_attributes : attribute list;
    media : media list;
  }

  and key =
    [ `Uri of Uri.t
    | `Base64 of string
    | `Clear of string
    | `Prompt
    | `Other of string ]

  and bandwidth = { bw_type : string; bw : string }

  and originator = {
    username : string;
    sess_id : Int64.t;
    sess_version : Int64.t;
    net_type : string;
    addr_type : string;
    unicast_addr : string;
  }

  and connection = {
    connection_net_type : string;
    connection_addr_type : string;
    connection_addr : string;
  }

  and time = {
    active : Int64.t * Int64.t;
    repeats : (Int64.t * Int64.t * Int64.t list) list;
  }

  and time_zone = Int64.t * Int64.t

  and media = {
    media_field : media_field;
    title : string option;
    connection : connection option;
    media_bandwidth : bandwidth option;
    media_key : key option;
    media_attributes : attribute list;
  }

  and media_field = {
    typ :
      [ `Audio | `Video | `Text | `Application | `Message | `Other of string ];
    port : [ `Port of int * int option ];
    proto : string;
    fmts : string list;
  }

  and attribute = A.t
end
