(*
 *  ENIAMsemantics implements semantic processing for ENIAM
 *  Copyright (C) 2016-2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2017 Institute of Computer Science Polish Academy of Sciences
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

open SemTypes

let rec linear_term = function
  | Tuple l -> Xml.Element("Tuple",[],Xlist.map l linear_term)
  | Variant(e,l) ->
      Xml.Element("Variant",["label",e],Xlist.map l (fun (i,t) ->
        Xml.Element("option",["number",i],[linear_term t])))
  | Dot -> Xml.Element("Dot",[],[])
  | Val s -> Xml.Element("Val",[],[Xml.PCData s])
  | Node t ->
      Xml.Element("Node",["orth",t.orth;"lemma",t.lemma;"pos",t.pos;
        "weight",string_of_float t.weight;"id",string_of_int t.id;"arg_dir",t.arg_dir;"gf",t.gf;
        "role",t.role;"role_attr",t.role_attr;"arole",t.arole;"arole_attr",t.arole_attr;
        "arev",string_of_bool t.arev;"label",t.n_label;"def_label",t.n_def_label],
        [Xml.Element("symbol",[],[linear_term t.symbol]);
         Xml.Element("arg_symbol",[],[linear_term t.arg_symbol]);
         Xml.Element("attrs",[],Xlist.map t.attrs (fun (k,v) -> Xml.Element("attr",["name",k],[linear_term v])));
         Xml.Element("args",[],[linear_term t.args]);
         Xml.Element("selprefs",[],[linear_term t.selprefs]);
(*          Xml.Element("sense",[],[linear_term t.sense]); *)
         Xml.Element("sem_args",[],[linear_term t.sem_args])])
  | Ref i -> Xml.Element("Ref",["index",string_of_int i],[])
  | Concept c ->
      Xml.Element("Concept",["sense",c.sense;"cat",c.cat;(*"local_quant",string_of_bool c.c_local_quant;*)
        "label",c.label;"def_label",c.def_label;
        (*"variable",fst c.variable ^ "_" ^ snd c.variable;"pos",string_of_int c.pos*)],
        [(*Xml.Element("sense",[],[XM c.sense]);
         Xml.Element("cat",[],[linear_term c.cat]);*)
(*         Xml.Element("name",[],[linear_term c.name]);
         Xml.Element("quant",[],[linear_term c.quant]);*)
         Xml.Element("relations",[],[linear_term c.relations]);
         Xml.Element("contents",[],[linear_term c.contents])])
(*  | Context c ->
      Xml.Element("Context",["label",c.cx_label;"def_label",c.cx_def_label;
        "variable",fst c.cx_variable ^ "_" ^ snd c.cx_variable;"pos",string_of_int c.cx_pos],
        [Xml.Element("sense",[],[linear_term c.cx_sense]);
         Xml.Element("contents",[],[linear_term c.cx_contents]);
         Xml.Element("relations",[],[linear_term c.cx_relations]);
         Xml.Element("cat",[],[linear_term c.cx_cat])])*)
  | Relation(r,a,c) -> Xml.Element("Relation",["role",r;"role_attribute",a],[linear_term c])
  | RevRelation(r,a,c) -> Xml.Element("RevRelation",["role",r;"role_attribute",a],[linear_term c])
  | SingleRelation r -> Xml.Element("SingleRelation",[],[linear_term r])
  | AddRelation(t,r,a,s) ->
      Xml.Element("AddRelation",["role",r;"role_attribute",a],
        [Xml.Element("",[],[linear_term t]);Xml.Element("",[],[linear_term s])])
  | AddParentRelation(t,s) ->
      Xml.Element("AddParentRelation",[],
        [Xml.Element("",[],[linear_term t]);Xml.Element("",[],[linear_term s])])
  | AddSingleRelation(r,s) ->
      Xml.Element("AddSingleRelation",[],
        [Xml.Element("",[],[linear_term r]);Xml.Element("",[],[linear_term s])])
  | RemoveRelation(r,a,t) -> Xml.Element("RemoveRelation",["role",r;"role_attribute",a],[linear_term t])
  | SetContextName(s,t) ->
      Xml.Element("SetContextName",["sense",s],[linear_term t])
  | CreateContext(s,t) ->
      Xml.Element("CreateContext",[],[linear_term (Concept s);linear_term t])
  | ManageCoordination(t,r) ->
      Xml.Element("ManageCoordination",[],[linear_term (Node t);linear_term r])
