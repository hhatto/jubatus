(*
 Original version of this file(rust.ml) is go.ml.
 following lines to go.ml original file license:
*)
(*
 Jubatus: Online machine learning framework for distributed environment
 Copyright (C) 2015 Preferred Networks and Nippon Telegraph and
 Telephone Corporation.

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License version 2.1 as published by the Free Software Foundation.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

open Syntax
open Lib

let comment_out_head = "//"

let make_header conf source filename content =
  make_source conf source filename content comment_out_head
;;

let gen_string_literal s =
  "\"" ^ String.escaped s ^ "\""
;;

let gen_args args =
  "(" ^ String.concat ", " args ^ ")"
;;

let gen_call func args =
  func ^ gen_args args
;;

let gen_list vars =
  "[" ^ String.concat ", " vars ^ "]"
;;

let gen_client_struct module_name =
  [
    (0, "pub struct " ^ (snake_to_upper module_name) ^ "Client<'a> {");
    (2,     "client: Client<'a>,");
    (0, "}");
    (0, "");
    (0, "impl<'a> " ^ (snake_to_upper module_name) ^ "Client<'a> {");
  ]
;;

let rec gen_type = function
  | Object -> "new(interface{})"
  | Bool -> "bool"
  | Int(signed, bytes) -> begin
    match signed, bytes with
    | true, 1 -> "i64"
    | true, 2 -> "i64"
    | true, 4 -> "i64"
    | true, 8 -> "i64"
    | false, 1 -> "u64"
    | false, 2 -> "u64"
    | false, 4 -> "u64"
    | false, 8 -> "u64"
    | _ -> raise (Unknown_type (Printf.sprintf "unknown int type: %b, %d" signed bytes))
  end
  | Float(_) -> "f64"
  | Raw -> "[]bytes"
  | String -> "String"
  | Datum -> "Datum"
  | Struct s  -> snake_to_upper s
  | List t -> "Vec<" ^ gen_type t ^ ">"
  | Map(key, value) -> "HashMap<" ^ gen_type key ^ ", " ^ (gen_type value) ^ ">"
  | Nullable(t) -> gen_call "TNullable.new" [gen_type t]
;;

let rec gen_to_msgpack_value_arg_start = function
  | Bool -> "Value::Bool("
  | Int(signed, bytes) -> begin
    match signed, bytes with
    | true, _ -> "Value::Integer(Integer::I64("
    | false, _ -> "Value::Integer(Integer::U64("
  end
  | Float(_) -> "Value::Float(Float::F64("
  | Raw -> "[]bytes"
  | String -> "Value::String("
  | Struct s  -> ""
  | List t -> "Value::Array("
  | Map(key, value) -> "Value::Map("
  | Nullable(t) -> gen_call "TNullable.new" [gen_type t]
  | _ -> ""
;;

let rec gen_to_msgpack_value_arg_x = function
  | Int(_, _) -> "*x"
  | Float(_) -> "*x"
  | _ -> "x"
;;

let rec gen_to_msgpack_value_arg_end = function
  | String -> ".to_owned())"
  | Int(_) -> "))"
  | Float(_) -> "))"
  | Datum -> ".to_msgpack_value()"
  | Struct(_) -> ".to_msgpack_value()"
  | Map(key, value) -> ".iter().map(|(k, v)| (" ^
      gen_to_msgpack_value_arg_start key ^ "k" ^ gen_to_msgpack_value_arg_end key ^ ", " ^
      gen_to_msgpack_value_arg_start value ^ "v" ^ gen_to_msgpack_value_arg_end value ^
      ")).collect())"
  | List t -> ".iter().map(|x| " ^
      gen_to_msgpack_value_arg_start t ^
      gen_to_msgpack_value_arg_x t ^
      gen_to_msgpack_value_arg_end t ^
      ").collect())"
  | _ -> "gen_to_msgpack_value_arg_end.X"
;;

let gen_client_call_args m =
  let msgpack_args = List.map (fun f ->
    gen_to_msgpack_value_arg_start f.field_type ^
    f.field_name ^
    gen_to_msgpack_value_arg_end f.field_type) m.method_arguments in
  "let args: Vec<Value> = vec![" ^ String.concat ", " msgpack_args ^ "];"
;;

let gen_client_call m =
  let name = m.method_name in
  let call_arg = [gen_string_literal name; "args";] in
  gen_call "let result = self.client.call" call_arg ^ ";"
;;

let gen_def service func args return_type =
  let arg_list = List.map (fun a -> a.field_name ^ ": " ^ gen_type a.field_type) args in
  let arg_list' = "&mut self" :: arg_list in
  "  pub fn " ^ func ^ "(" ^
      String.concat ", " arg_list' ^
      ") -> " ^ return_type ^ " {"
;;

let gen_constructor service =
  let camel_name = snake_to_upper service in
  [ (2, "pub fn new(host: &str, name: &'a str) -> Self {");
    (4,   "let c = Client::new(host, name);");
    (4,   camel_name ^ "Client {");
    (6,     "client: c,");
    (4,   "}");
    (2, "}");
  ]
;;

let gen_return_type m =
  Option.default "" (Option.map gen_type m.method_return_type)
;;

let rec gen_type_decode = function
  | Bool -> ".as_bool().unwrap()"
  | Int(signed, bytes) -> begin
    match signed, bytes with
    | true, _ -> ".as_i64().unwrap()"
    | false, _ -> ".as_u64().unwrap()"
  end
  | Float(_) -> ".as_f64().unwrap()"
  | String -> ".as_str().unwrap().to_string()"
  | Struct s  -> snake_to_upper s ^ "::from_msgpack_value(x)"
  | List t -> ".as_array().unwrap().iter().map(|x| " ^ gen_type_decode t ^ ").collect()"
  | Map(key, value) -> ".as_map().unwrap().iter().map(|k, v| " ^
    "k" ^ gen_type_decode key ^
    "v" ^ gen_type_decode value ^ ")"
  | _ -> "x_x"
;;

let rec gen_type_decode_start = function
  | Datum -> "Datum::from_msgpack_value("
  | Struct s -> snake_to_upper s ^ "::from_msgpack_value("
  | _ -> ""
;;

let rec gen_type_decode_end = function
  | Bool -> ".as_bool().unwrap()"
  | Int(signed, bytes) -> begin
    match signed, bytes with
    | true, _ -> ".as_i64().unwrap()"
    | false, _ -> ".as_u64().unwrap()"
  end
  | Float(_) -> ".as_f64().unwrap()"
  | String -> ".as_str().unwrap().to_string()"
  | Datum -> ".clone())"
  | Struct s  -> ".clone())"
  | List t -> ".as_array().unwrap().iter().map(|x| " ^
      gen_type_decode_start t ^ "x" ^ gen_type_decode_end t ^ ").collect()"
  | Map(key, value) -> ".as_map().unwrap().iter().map(|m| {\n" ^
    "let (ref k, ref v): (Value, Value) = *m;\n" ^
    "(" ^
    "k" ^ gen_type_decode key ^
    "," ^ gen_type_decode_start value ^ "v" ^ gen_type_decode_end value ^
    ")}).collect::<HashMap<" ^
    gen_type key ^ ", " ^ gen_type value ^ ">>()"
  | _ -> "x_x"
;;

let rec gen_type_from_msgpack_value_start = function
  | Datum  -> "Datum::from_msgpack_value("
  | Struct s -> snake_to_upper s ^ "::from_msgpack_value("
  | _ -> ""
;;

let rec gen_type_from_msgpack_value_end = function
  | Bool -> ".as_bool().unwrap()"
  | Int(signed, bytes) -> begin
    match signed, bytes with
    | true, _ -> ".as_i64().unwrap()"
    | false, _ -> ".as_u64().unwrap()"
  end
  | Float(_) -> ".as_f64().unwrap()"
  | String -> ".as_str().unwrap().to_string()"
  | Datum -> ".clone())"
  | Struct s  -> ".clone())"
  | List t -> ".as_array().unwrap().iter().map(|x| " ^
      gen_type_from_msgpack_value_start t ^
      "x.clone()" ^
      gen_type_from_msgpack_value_end t ^
      ").collect()"
  | Map(key, value) -> ".as_map().unwrap().iter().map(|m| {\n" ^
    "let (ref k, ref v): (Value, Value) = *m;\n" ^
    "(" ^
    "k" ^ gen_type_decode key ^
    ", v" ^ gen_type_decode value ^ ")}).collect::<HashMap<" ^
    gen_type key ^ ", " ^ gen_type value ^ ">>()"
  | _ -> "x_x"
;;

let gen_response_decode m =
  let ret_type_s = match m.method_return_type with
    | None -> "nil"
    | Some t -> gen_type_decode_start t in
  let ret_type_e = match m.method_return_type with
    | None -> "nil"
    | Some t -> gen_type_decode_end t in
  ret_type_s ^ "result" ^ ret_type_e
;;

let gen_client_method service_name m =
  let name = m.method_name in
  [ (0, gen_def (snake_to_upper service_name) name m.method_arguments (gen_return_type m));
    (2,   gen_client_call_args m);
    (2,   gen_client_call m);
    (2,   gen_response_decode m);
    (0, "}");
  ]
;;

let gen_client s =
  let gen_client_with_service = gen_client_method s.service_name in
  let methods = List.map gen_client_with_service s.service_methods in
  let content = methods in
  concat_blocks content;
;;


let gen_self_with_equal field_names =
  List.map (fun s -> (0, "@" ^ s ^ " = " ^ s)) field_names
;;

let range s e =
  let rec aux s e =
    if s >= e then [] else s :: aux (s+1) e in
  if s > e then List.rev (aux e s) else aux s e
;;

let gen_message m =
  let fields = List.map (fun f -> (f.field_name, f.field_type)) m.message_fields in
  let args = List.map (fun (name, type_name) ->
                gen_to_msgpack_value_arg_start type_name ^
                "self." ^ name ^ (gen_to_msgpack_value_arg_end type_name)) fields in
  let ifields = List.combine (range 0 (List.length fields)) fields in
  List.concat [
    [
      (0, "#[derive(Default, Debug, Clone)]");
      (0, "pub struct " ^ snake_to_upper m.message_name ^ " {");
    ];
    List.map (fun (name, type_name) ->
      (2, "pub " ^ name ^ ": " ^ gen_type type_name ^ ",") ) fields;
    [(0, "}"); (0, "");];
    [
      (0, "impl " ^ snake_to_upper m.message_name ^ " {");
      (2,   "pub fn to_msgpack_value(&self) -> Value {");
      (4,     "Value::Array(vec![" ^ String.concat ", " args ^ "])");
      (2,   "}");
      (0, "");
      (2,   "pub fn from_msgpack_value(data: Value) -> " ^ snake_to_upper m.message_name ^ "{");
      (4,     "let s = data.as_array().unwrap();");
      (4,     snake_to_upper m.message_name ^ "{");
    ];
    List.map (fun (idx, (name, type_name)) ->
      (2, name ^ ": " ^ gen_type_from_msgpack_value_start type_name ^
          "s[" ^ Printf.sprintf "%d" idx ^ "]" ^
          gen_type_from_msgpack_value_end type_name ^ ",")) ifields;
    [
      (4,     "}");
      (2,   "}");
      (0, "}");
      (0, "");
    ];
  ]
;;

let gen_typedef = function
  | Message m ->
     gen_message m
  | _ ->
     [(0, "")]
;;

let common_functions name =
  [
    (* save func *)
    (0, "pub fn save(&mut self, id: String) -> HashMap<String, String> {");
    (2,   "let args: Vec<Value> = vec![Value::String(id)];");
    (2,   "let result = self.client.call(\"save\", args);");
    (2,   "let mut ret: HashMap<String, String> = HashMap::new();");
		(2,   "for r in result.as_map().unwrap().iter() {");
		(4,     "let (ref k, ref v): (Value, Value) = *r;");
    (4,     "ret.insert(k.as_str().unwrap().to_string(),");
    (4,     "v.as_str().unwrap().to_string());");
		(2,   "}");
    (2,   "ret");
    (0, "}");
    (0, "");

    (* load func *)
    (0, "pub fn load(&mut self, id: String) -> bool {");
    (2,   "let args: Vec<Value> = vec![Value::String(id)];");
    (2,   "let result = self.client.call(\"load\", args);");
    (2,   "result.as_bool().unwrap()");
    (0, "}");
    (0, "");

    (* get_config func *)
    (0, "pub fn get_config(&mut self) -> String {");
    (2,   "let args: Vec<Value> = vec![];");
    (2,   "let result = self.client.call(\"get_config\", args);");
    (2,   "result.as_str().unwrap().to_string()");
    (0, "}");
    (0, "");

    (* get_status func *)
    (0, "pub fn get_status(&mut self) -> HashMap<String, HashMap<String, String>> {");
    (2,   "let args: Vec<Value> = vec![];");
    (2,   "let result = self.client.call(\"get_status\", args);");
		(2,   "let mut ret: HashMap<String, HashMap<String, String>> = HashMap::new();");
		(2,   "for r in result.as_map().unwrap().iter() {");
		(4,     "let (ref kk, ref vv): (Value, Value) = *r;");
		(4,     "let mut hh: HashMap<String, String> = HashMap::new();");
		(4,     "for rr in vv.as_map().unwrap().iter() {");
		(6,       "let (ref kkk, ref vvv): (Value, Value) = *rr;");
    (6,       "hh.insert(kkk.as_str().unwrap().to_string(),");
    (6,       "vvv.as_str().unwrap().to_string());");
		(4,     "}");
		(4,     "ret.insert(kk.as_str().unwrap().to_string(), hh);");
		(2,   "}");
		(2,   "ret");
    (0, "}");
    (0, "");

    (* do_mix func *)
    (0, "pub fn do_mix(&mut self) -> bool {");
    (2,   "let args: Vec<Value> = vec![];");
    (2,   "let result = self.client.call(\"do_mix\", args);");
    (2,   "result.as_bool().unwrap()");
    (0, "}");
    (0, "");

    (* get_proxy_status func *)
    (0, "pub fn get_proxy_status(&mut self) -> HashMap<String, HashMap<String, String>> {");
    (2,   "let args: Vec<Value> = vec![];");
    (2,   "let result = self.client.call(\"get_proxy_status\", args);");
		(2,   "let mut ret: HashMap<String, HashMap<String, String>> = HashMap::new();");
		(2,   "for r in result.as_map().unwrap().iter() {");
		(4,     "let (ref kk, ref vv): (Value, Value) = *r;");
		(4,     "let mut hh: HashMap<String, String> = HashMap::new();");
		(4,     "for rr in vv.as_map().unwrap().iter() {");
		(6,       "let (ref kkk, ref vvv): (Value, Value) = *rr;");
    (6,       "hh.insert(kkk.as_str().unwrap().to_string(),");
    (6,       "vvv.as_str().unwrap().to_string());");
		(4,     "}");
		(4,     "ret.insert(kk.as_str().unwrap().to_string(), hh);");
		(2,   "}");
		(2,   "ret");
    (0, "}");
    (0, "");

    (* get_name func *)
    (0, "pub fn get_name(&self) -> &str {");
    (2,   "return self.client.name");
    (0, "}");
    (0, "");

    (* set_name func *)
    (0, "pub fn set_name(&mut self, new_name: &'a str) {");
    (2,   "self.client.name = new_name;");
    (0, "}");
    (0, "");
  ]
;;

let end_br = 
  [
    (0, "}");
  ]
;;

let gen_client_header module_name = 
  [
    (0, "use std::collections::HashMap;");
    (0, "use msgpack::Value;");
    (0, "use msgpack::value::{Integer, Float};");
    (0, "use common::datum::Datum;");
    (0, "use common::client::Client;");
    (0, "use " ^ module_name ^ "::types::*;");
    (0, "use rmp_serialize::Decoder;");
    (0, "");
  ]
;;

let gen_client_file conf source services =
  let base = File_util.take_base source in
  let filename = Filename.concat base "client.rs" in
  let clients = List.map (fun s ->
    concat_blocks [
      gen_client_header s.service_name;
      gen_client_struct s.service_name;
      gen_constructor s.service_name;
      gen_client s;
      common_functions s.service_name;
      end_br;
    ]
  ) services in

  let content = concat_blocks [
    [
      (0, "");
    ];
    concat_blocks clients;
  ] in
  make_header conf source filename content
;;

let gen_mod_file conf source =
  let base = File_util.take_base source in
  let name = Filename.concat base "mod.rs" in

  let content = [
    (0, "pub mod client;");
    (0, "pub mod types;");
  ] in
  make_header conf source name content
;;

let gen_type_file conf source idl =
  let base = File_util.take_base source in
  let name = Filename.concat base "types.rs" in
  let types = List.map gen_typedef idl in

  let content = concat_blocks [
    [
      (0, "use std::collections::HashMap;");
      (0, "use common::datum::Datum;");
      (0, "use msgpack::Value;");
      (0, "use msgpack::value::Float;");
      (0, "use msgpack::value::Integer;");
      (0, "");
    ];
    concat_blocks types;
  ] in
  make_header conf source name content
;;

let generate conf source idl =
  let services = get_services idl in
  gen_client_file conf source services;
  gen_mod_file conf source;
  gen_type_file conf source idl
;;
