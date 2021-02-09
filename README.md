Session Description Protocol (RFC 4566)
=======================================

A (*very WIP*) pure OCaml implementation of [RFC 4566](https://tools.ietf.org/html/rfc4566#section-8), Session Description Protocol (SDP).

*Disclaimer: I've never written one of these "OCaml implementations of a protocol" before so I largely have no idea what I'm doing especially trying to make sense of the RFC and in particular, all of the details about encoding (UTF-8, ASCII...). If someone with more experience has the time to check this I'm happy for PRs/Issues for improvement*.

---

Todos: 

 - [ ] Better printing and encoding functions (some are not implemented yet...)
 - [ ] Better testing (could it use fuzzing or database of real-world SDPs?)

---

The library exposes the `Sdp.Make` functor. This is because most of the SDP protocol is fairly straightforward and the same for most use cases except the attributes. These vary widely depending on the usage of the SDP protocol. If you want to extend the abilities of the parser to provide more useful types in your attributes then you need to provide that functionality: 

```ocaml
module type Attribute = sig
  type t
  (* The type of your attributes *)

  val attribute : t Angstrom.t
  (* Angstrom parser -- only needs to parse one line terminated with eol *)

  val pp : t Fmt.t
  (* A pretty printer for your attributes *)
end
```

If you are happy with basic functionality then you can use `Sdp.Basic` which has attribute type of: 

```ocaml
type t = [ `Prop of string | `Value of string * string ]
```

With either `Sdp.Make(A)` or `Sdp.Basic` you then have access to an `Encode` module for building up SDPs, `Decode` for parsing them and `Pp` for printing them.