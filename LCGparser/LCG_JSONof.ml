(*
 *  LCGparser, a parser for Logical Categorial Grammar formalism
 *  Copyright (C) 2019 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2019 LekSeek Sp. z o.o. sp. k.
 *
 *  This library is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open LCGtypes
open Xjson

let rec linear_term = function
(*    Var v -> Xml.Element("Var",[],[Xml.PCData v])*)
  | Tuple l -> JArray(Xlist.map l linear_term)
  | Variant(e,l) ->
      JObject(["variant",JString e] @ Xlist.map l (fun (i,t) ->
        "option",JObject["number",JString i;"value",linear_term t]))
(*  | VariantVar(v,t) -> Xml.Element("VariantVar",["var",v],[linear_term t])
  | Proj(n,t) -> Xml.Element("Proj",["option",string_of_int n],[linear_term t])
  | ProjVar(v,t) -> Xml.Element("ProjVar",["var",v],[linear_term t])
  | SubstVar v -> Xml.Element("SubstVar",[],[Xml.PCData v])
  | Subst(s,v,t) -> Xml.Element("Subst",["var",v],[linear_term s;linear_term t])
  | Inj(n,t) -> Xml.Element("Inj",["option",string_of_int n],[linear_term t])
  | Case(t,l) ->
      Xml.Element("Case",[],
        Xml.Element("matched",[],[linear_term t]) ::
        Xlist.map l (fun (v,t) ->  Xml.Element("option",["var",v],[linear_term t])))
  | Lambda(v,t) -> Xml.Element("Lambda",["var",v],[linear_term t])
  | LambdaSet(l,t) -> Xml.Element("LambdaSet",[],Xlist.map l (fun v -> Xml.Element("var",[],[Xml.PCData v])) @ [linear_term t])
  | LambdaRot(n,t) -> Xml.Element("LambdaRot",["number",string_of_int n],[linear_term t])
  | App(s,t) -> Xml.Element("App",[],[linear_term s;linear_term t])*)
  | Dot -> JString "Dot"
  | Val s -> JString s
(*  | SetAttr(e,s,t) -> Xml.Element("SetAttr",["label",e],[linear_term s;linear_term t])
  | Fix(s,t) -> Xml.Element("Fix",[],[linear_term s;linear_term t])
  | Empty t -> Xml.Element("Empty",[],[linear_term t])
  | Apply t -> Xml.Element("Apply",[],[linear_term t])
  | Insert(s,t) -> Xml.Element("Insert",[],[linear_term s;linear_term t])*)
  | Node t -> JObject[
       "orth",JString t.orth;"lemma",JString t.lemma;"pos",JString t.pos;
       "weight",JNumber(string_of_float t.weight);"id",JNumber(string_of_int t.id);"arg_dir",JString t.arg_dir;
       "symbol",linear_term t.symbol;
       "arg_symbol",linear_term t.arg_symbol;
       "attrs",JObject(Xlist.map t.attrs (fun (k,v) -> k,linear_term v));
       "args",linear_term t.args]
(*  | Coord(l,t,a) -> Xml.Element("Coord",[],Xlist.map (t :: a :: l) linear_term)
  | AddCoord(s,t) -> Xml.Element("AddCoord",[],[linear_term s;linear_term t])
  | MapCoord(s,t) -> Xml.Element("MapCoord",[],[linear_term s;linear_term t])
  | ConcatCoord(s,t) -> Xml.Element("ConcatCoord",[],[linear_term s;linear_term t])*)
  | Ref i -> JObject["ref",JNumber (string_of_int i)]
  | Cut t -> JObject["cut",linear_term t]
  | t -> failwith ("linear_term: " ^ LCGstringOf.linear_term 0 t)
  
let linear_term_array a =
  let l = Int.fold 0 (Array.length a - 1) [] (fun l i ->
    (JObject["index",JNumber (string_of_int i);"element",linear_term a.(i)]) :: l) in
  JArray(List.rev l)
