/*   graphkb.pl
     Author: Giménez, Christian.

     Copyright (C) 2019 Giménez, Christian

     This program is free software: you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation, either version 3 of the License, or
     at your option) any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program.  If not, see <http://www.gnu.org/licenses/>.

     11 oct 2019
*/


:- module(graphkb, [
              load_ttl/1,
              draw_graph/1,
              list_nodes/1
          ]).
/** <module> Graph KB: Make graphs with the input ontology.

@author Christian Gimenez
@license GPLv3
*/


:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf11)).
%% :- ensure_loaded(library(semweb/rdf_db)).


:- rdf_load(library(semweb/rdfs)).
:- rdf_register_prefix(mm, 'http://mm.fi.uncoma.edu.ar/kb/journals#').
:- rdf_register_prefix(owl, 'http://www.w3.org/2002/07/owl#').
:- rdf_register_prefix(swrc, 'http://swrc.ontoware.org/ontology#').


/**
 load_ttl(+File: term) is det

Load a turtle file into a default graph.

This is an easy predicate for using the rdf_load/1 predicate.

@param File The file path.
*/
load_ttl(File) :-
    rdf_default_graph(_Old, graph),
    rdf_load(File).

/**
 get_objects(+Node: term, -Objects: list) is det

Return the objects node of the graph associated to the given Node. 

In other words, return the Objects from the (Node, Pred, Object) tripples
that appear in the graph. Prefer the abbreviated form of each Subject if it
exists.

@param Node The node IRI or prefix:suffix form.
@param Subjects A list of prefix:suffix if founded, or IRIs if there's no
  abbreviated form.
*/
get_objects(Node, Objects) :-
    rdf_subject(Node),
    findall((Node,B,C), rdf(Node, B, C), Objects).


/**
 get_subjects(+Node: term, -Subjects: list) is det

Return the subjects node of the graph associated to the given Node. 

In other words, return the Subjects from the (Subject, Pred, Node) tripples
that appear in the graph. Prefer the abbreviated form of each Subject if it
exists.

@param Node The node IRI or prefix:suffix form.
@param Subjects A list of prefix:suffix if founded, or IRIs if there's no
  abbreviated form.
*/
get_subjects(Node, Subjects) :-
    rdf_object(Node),
    findall((A, B, Node), rdf(A, B, Node), Subjects).


/**
 get_associated(+Node: term, -Associations: list) is det

Return the the nodes associated the given one.

In other words, return the Subjects from the (Subject, Pred, Node) tripples
and the Objects from the (Node, Pred, Object) that appear in the graph.
Prefer the abbreviated form of each Subject and Object if it exists.

@param Node The node IRI or prefix:suffix form.
@param Associations A list of prefix:suffix if founded, or IRIs if there's no
  abbreviated form.
*/
get_associated(Node, Associations) :-
    rdf_global_id(Node, NodeAbbrv),
    get_subjects(NodeAbbrv, Subjects),
    get_objects(NodeAbbrv, Objects),
    append(Subjects, Objects, Associations).


/**
 abbrev_name(+Name: term, ?Abbrev: term)

Return the abbreviated name if it exists. 

Return the prefix:suffix form of Name if it exists, if not, return the IRI.

@param Name The IRI to abbreviate.
@param Abbrev The abbreviated prefix:suffix form.
*/
abbrev_name(Name, Abbrev) :-
    rdf_global_id(Abbrev, Name), !.
abbrev_name(Name, Name).

/**
 draw_node(+Node: term) is det

Print to stdout a dot representation of the Node.

Print a dot graph node of the given RDF graph Node. Change the representation 
according to the type of the node.

If it is a datatype (is not an IRI), use a box node.
If it is an IRI, use an ellipse node.

@param Node A prefix:suffix form or IRI.
*/

draw_node(Node) :-
    \+ rdf_is_iri(Node), !,
    format('"~w" [shape=box, color=blue];', [Node]).    
draw_node(Node) :-
    abbrev_name(Node, Name),
    (Name = owl:_Suffix ; Name = rdf:_Suffix ; Name = rdfs:_Suffix), !,
    format('"~w" [color=red, shape=ellipse];', [Name]).
draw_node(Node) :-
    rdf(Node, rdf:type, owl:'NamedIndividual'),!,
    abbrev_name(Node, Name),
    format('"~w" [color=blue];', [Name]).
draw_node(Node) :-
    rdf(Node, rdf:type, owl:'Class'),!,
    abbrev_name(Node, Name),
    format('"~w" [shape=ellipse];', [Name]).
draw_node(Node) :-
    %% rdf_is_object(Node),!,
    abbrev_name(Node, Name),
    format('"~w" [shape=box, color=blue];', [Name]).

/**
 draw_edge(+Triple: term) is det

Print to stdout a dot representation of the given edge.

Use the Triple to print a dot representation of the edge of the graph. The 
nodes are printed with draw_node/1. 

@param Tripe A `(Subject, Pred, Object)` triple representation.
*/
draw_edge((A, B, C)) :-
    abbrev_name(B, rdfs:subClassOf),
    abbrev_name(A, A2),
    abbrev_name(C, C2),
    format('edge [label="~w", style=solid, arrowhead=none] "~w" -> "~w";\n',
           [rdfs:subclassOf, A2, C2]).
draw_edge((A, B, C)) :-
    abbrev_name(A, A2),
    abbrev_name(B, B2),
    abbrev_name(C, C2),
    format('edge [label="~w", style=dashed, arrowhead=normal] "~w" -> "~w";\n',
           [B2, A2, C2]).


/**
 draw_edges(+Triples: list) is det

Draw all the edges on the list using draw_edge/1.

@param Triples A list of `(S, P, O)` terms.
*/
draw_edges([]) :- !.
draw_edges([Assoc|Rest]) :-
    draw_edge(Assoc),
    draw_edges(Rest).

/**
 draw_nodes(+Triples: list) is det

Draw all the nodes on the list using draw_nodes/1.

Use the subject and object nodes only.

@param Triples A list of `(S, P, O)` terms.
*/
draw_nodes([]) :- !.
draw_nodes([(S, _P, O)|Rest]) :-
    draw_node(S), nl,
    draw_node(O), nl,
    draw_nodes(Rest).


/**
 draw_graph(Node) is det

Print a dot syntax with a graph with the given Node as a center concept.

Print all associations related to the give Node (all subjects and objects 
related to it).

@param Node A prefix:suffix term or an IRI.
*/
draw_graph(Node) :-
    get_associated(Node, Assocs),
    write('digraph {'), nl,
    draw_nodes(Assocs),nl,nl,
    draw_edges(Assocs),
    write('}'), nl.
    

/**
 abbrev_nodes(?Node: pred) is semidet 

Node is the most abbreviated form.

Use findall/3 for searching all the nodes in the graph in its abbreviated form.
The prefix is obtained according to the registered ones. Use 
rdf_register_prefix/2 to register new prefixes.

@param Node An IRI or a prefix:term that represent a node in the most 
  abbreviated form.
*/
abbrev_nodes(Node) :-
    rdf_iri(Node1),
    rdf_node(Node1),
    abbrev_name(Node1, Node).


/**
 list_nodes(-Nodes) is det

Nodes is a list of nodes in its most abbreviated form.

Return all the nodes in the RDF graph in its abbreviated form.
The current registered prefixes are used for making abbreviations. 

See 
*/
list_nodes(Nodes) :-
    findall(Node, abbrev_nodes(Node), Nodes).
