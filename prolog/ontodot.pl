/*   ontodot.pl
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


:- module(ontodot, [
              load_ttl/1,
              draw_all/1,
              draw_graph/2,
              draw_prefix/2,
              draw_hierarchy/1,
              list_nodes/1,
              dot_all/1,
              dot_prefix/2,
              dot_graph/2,
              dot_hierarchy/1
          ]).
/** <module> Graph KB: Make graphs with the input ontology.

@author Christian Gimenez
@license GPLv3
*/


:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf11)).
% :- ensure_loaded(library(semweb/rdf_db)).


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
@param Subjects A list of triples `(S,P,O)` that are associated with the
  given Node. Node is S in the triple.
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
@param Subjects A list of triples `(S,P,O)` that are associated with the
  given Node. Node is O in the triple.
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
@param Associations A list of triples `(S,P,O)` that are associated with the
  given Node. Node is O or S in the triple.
*/
get_associated(Node, Associations) :-
    rdf_global_id(Node, NodeAbbrv),
    (get_subjects(NodeAbbrv, Subjects) ; Subjects = []),
    (get_objects(NodeAbbrv, Objects) ; Objects = []),
    append(Subjects, Objects, Associations).


/**
 get_isa_subjects(+Node: term, -Assocs: list)

Get all the children of the given node.

@param Node A prefix:suffix term or an URI.
@param Assocs A list of triples `(S, rdfs:subClassOf, Node)`.
*/
get_isa_subjects(Node, Assocs) :-
    rdf_object(Node),
    findall((A,rdfs:subClassOf,Node),
            rdf(A,rdfs:subClassOf,Node),
            Assocs).

/**
 get_isa_objects(+Node: term, -Assocs: list)

Get all the parents of the given node.

@param Node A prefix:suffix term or an URI.
@param Assocs A list of triples `(Node, rdfs:subClassOf, O)`.
*/
get_isa_objects(Node, Assocs) :-
    rdf_subject(Node),
    findall((Node,rdfs:subClassOf,A),
            rdf(Node,rdfs:subClassOf,A),
            Assocs).

/**
 get_isa(+Node: term, -Assocs: list)

Assocs is the list of triples where Node parent or child of a subclass.

@param Node a Prefix:Suffix or an URL term.
@param Assocs A list of triples like (S, rdfs:subClassOf, Node) or
  (Node, rdfs:subClassOf, O).
*/
get_isa(Node, Assocs) :-
    rdf_global_id(Node, NodeAbbrv),
    get_isa_subjects(NodeAbbrv, Assocs1),
    get_isa_objects(NodeAbbrv, Assocs2),
    append(Assocs1, Assocs2, Assocs).
    

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
 draw_node(+Node: term, -Str: string) is det

Generate a dot representation of the Node.

Generate a dot graph node of the given RDF graph Node. Change the 
representation according to the type of the node.

If it is a datatype (is not an IRI), use a box node.
If it is an IRI, use an ellipse node.

@param Node A prefix:suffix form or IRI.
@param Str The resulting dot string.
*/

draw_node(Node, Str) :-
    \+ rdf_is_iri(Node), !,
    format(string(Str), '"~w" [shape=box, color=blue];', [Node]).    
draw_node(Node, Str) :-
    abbrev_name(Node, Name),
    (Name = owl:_Suffix ; Name = rdf:_Suffix ; Name = rdfs:_Suffix), !,
    format(string(Str), '"~w" [color=red, shape=ellipse];', [Name]).
draw_node(Node, Str) :-
    rdf(Node, rdf:type, owl:'NamedIndividual'),!,
    abbrev_name(Node, Name),
    format(string(Str), '"~w" [color=blue];', [Name]).
draw_node(Node, Str) :-
    rdf(Node, rdf:type, owl:'Class'),!,
    abbrev_name(Node, Name),
    format(string(Str), '"~w" [shape=ellipse];', [Name]).
draw_node(Node, Str) :-
    %% rdf_is_object(Node),!,
    abbrev_name(Node, Name),
    format(string(Str), '"~w" [shape=box, color=blue];', [Name]).

/**
 draw_edge(+Triple: term, -Str: string) is det

Generate a dot representation of the given edge.

Use the Triple to create a dot representation of the edge of the graph. The 
node dot string is generated with draw_node/1. 

@param Tripe A `(Subject, Pred, Object)` triple representation.
@param Str The resulting dot string.
*/
draw_edge((A, B, C), Str) :-
    abbrev_name(B, rdfs:subClassOf),
    abbrev_name(A, A2),
    abbrev_name(C, C2),
    format(
        string(Str),
        'edge [label="~w", style=solid, arrowhead=none] "~w" -> "~w";\n',
        [rdfs:subclassOf, A2, C2]).
draw_edge((A, B, C), Str) :-
    abbrev_name(A, A2),
    abbrev_name(B, B2),
    abbrev_name(C, C2),
    format(
        string(Str),
        'edge [label="~w", style=dashed, arrowhead=normal] "~w" -> "~w";\n',
        [B2, A2, C2]).

/**
 draw_edges_noprops(+Assocs: list, -Str: string)

Draw all edges that has no properties as object.

@param Assocs A list of (S,P,O) triples. 
@see draw_edge/1
*/
draw_edges_noprops([], "") :- !.
draw_edges_noprops([(_A, _B, C)|Rest], Str) :-
    % Ignore if it is an IRI, is probably a property.
    \+ rdf_is_iri(C), !, %% red cut
    draw_edges_noprops(Rest, Str).
draw_edges_noprops([Edge|Rest], Str) :-
    draw_edge(Edge, Str),
    draw_edges_noprops(Rest, RestStr),
    string_concat(Str, RestStr, Str).

/**
 draw_edges(+Triples: list, -Str: string) is det

Draw all the edges on the list using draw_edge/1.

@param Triples A list of `(S, P, O)` terms.
*/
draw_edges([], "") :- !.
draw_edges([Assoc|Rest], Str) :-
    draw_edge(Assoc, EdgeStr),
    draw_edges(Rest, RestStr),
    string_concat(EdgeStr, RestStr, Str).



/**
 draw_nodes(+Triples: list) is det

Draw all the nodes on the list using draw_nodes/1.

Use the subject and object nodes only.

@param Triples A list of `(S, P, O)` terms.
*/
draw_nodes([], "") :- !.
draw_nodes([(S, _P, O)|Rest], Str) :-
    draw_node(S, SubjectStr), 
    draw_node(O, ObjectStr),
    
    draw_nodes(Rest, NodesStr),

    format(string(Str), '~s~n~s~n~s',
           [SubjectStr, ObjectStr, NodesStr]).


/**
 draw_graph(+Node: term, -Str: string) is det

Generate a dot syntax with a graph with the given Node as a center concept.

Generate all associations related to the give Node (all subjects and objects 
related to it).

@param Node A prefix:suffix term or an IRI.
@param Str The resulting dot string.
*/
draw_graph(Node, Str) :-
    get_associated(Node, Assocs),
    
    draw_nodes(Assocs, NodesStr),
    draw_edges(Assocs, EdgesStr),

    format(string(Str),
           'digraph {rankdir=RL;~n~s~n~n~s}~n',
           [NodesStr,EdgesStr]).
    

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
*/
list_nodes(Nodes) :-
    findall(Node, abbrev_nodes(Node), Nodes).

/**
 get_all_assocs(+Nodes: list, -Assocs: list).

Get all associations.
*/
get_all_assocs([], []) :- !.
get_all_assocs([Node|NRest], Lst) :-
    get_all_assocs(NRest, Lst1),
    (get_associated(Node, Assocs); Assocs = []),
    append(Assocs, Lst1, Lst).

/**
 get_all_isa(+Nodes: list, -Assocs: list)

Given a list of nodes, make Assocs a list of triples with their is-a 
associations.

Assocs will have an is-a relationship whose each node is a parent or child 
of other node. 

@param Nodes A list of prefix:suffix or IRIs.
@param Assocs A list of triples with the rdfs:subClassOf relationship.
@see get_isa/2
*/
get_all_isa([], []) :- !.
get_all_isa([Node|NRest], Lst) :-
    get_all_isa(NRest, Lst1),
    (get_isa(Node, Assocs); Assocs = []),
    append(Assocs, Lst1, Lst).
    

/**
 draw_all(-Str: string).

Generate the dot string that represent the default RDF graph.

Generate a dot representation of the default RDF graph. To improve readability, property objects are not represented associated with the other subjects. See draw_edges_noprops/1 for more information.

This predicate can be used with load_ttl/1. For instance:
```
?- load_ttl('my_kb.ttl'), draw_all(Str).
```

@param Str The resulting dot string.
*/
draw_all(Str) :-
    list_nodes(Nodes),
    get_all_assocs(Nodes, Assocs),

    draw_nodes(Assocs, NodesStr), 
    draw_edges_noprops(Assocs, EdgesStr),

    format(string(Str),
           'digraph {rankdir=LR;~n~s~n~n~s}~n',
           [NodesStr, EdgesStr]).


/**
 nodes_with_prefix(+Prefix:term, -Nodes: list)

Nodes is a list of all prefix:suffixs declared to be in the namespace/prefix
given.

@param Prefix The prefix term.
@param Nodes A list of Prefix:Name terms.
*/
nodes_with_prefix(Prefix, Nodes) :-
    list_nodes(AllNodes),
    findall(Prefix:Name, member(Prefix:Name, AllNodes), Nodes).

/**
 draw_prefix(+Prefix: term, -Str: string)

Generate the dot text with the relations of all the nodes with a certain
prefix.

@param Prefix A term.
@param Str The resulting dot string.
*/
draw_prefix(Prefix, Str) :-
    nodes_with_prefix(Prefix, Nodes),
    get_all_assocs(Nodes, Assocs),

    draw_nodes(Assocs, NodesStr),
    draw_edges(Assocs, EdgesStr),

    format(
        string(Str),
        'digraph {rankdir=RL;~n~s~n~n~s}~n',
        [NodesStr, EdgesStr]).

/**
 draw_hierarchy(-Str: string).

Generate a dot text with the hierarchy is-a relation of all nodes.

@param Str The resulting dot string.
*/
draw_hierarchy(Str) :-
    list_nodes(AllNodes),
    get_all_isa(AllNodes, Assocs),
    
    draw_nodes(Assocs, NodesStr),
    draw_edges(Assocs, EdgesStr),

    format(
        string(Str),
        'digraph {~n~s~n~n~s}~n',
        [NodesStr, EdgesStr]).

/**
 prepare_cmd(+File: term, -Stream: term)

Start the dot command for receiving the dot file from stdin. 

The type of the image to generate is determined by the File name extension.

@param File The file where the image file is created.
@param Stream The stream opened for providing the dot. This is setted as the
  default output.
*/
prepare_cmd(File, Stream) :-
    file_name_extension(_Base, Type, File),
    dot_command(Type, File, CMD),
    open(pipe(CMD), write, Stream).    

/**
 dot_command(+Type: term, +File: term, -CMD: term)

Create the dot command for exporting an image file at the given File path.

@param Type The type of the output file. See dot manpage.
@param File The image file path.
@param CMD The resulting command.

@see dot manpage for available types.
*/
dot_command(Type, File, CMD) :-
    format(atom(CMD), 'dot -T~s -o \'~w\'', [Type, File]).

/**
 dot_graph(+Node: term, +File: term)

Create an image file with all the relationships of a given node.

The image type is deduce by the File extension. 'example.png' will create a PNG
image.

@param Node The prefix:suffix or IRI.
@param File The path to the image file to be generated.
*/
dot_graph(Node, File) :-
    prepare_cmd(File, Stream),
    draw_graph(Node, Str),
    write(Stream, Str),
    close(Stream).

/**
 dot_all(+File: term)

Create an image file with all the relationships of all the nodes.

The image type is deduce by the File extension. 'example.png' will create a PNG
image.

@param File The path to the image file to be generated.
*/
dot_all(File) :-
    prepare_cmd(File, Stream),
    draw_all(Str),
    write(Stream, Str),
    close(Stream).

/**
 dot_prefix(+Prefix: term, +File: term)

Create an image file with all the nodes from a namespace/prefix and their 
relations.

The image type is deduce by the File extension. 'example.png' will create a PNG
image.

@param Prefix The prefix term.
@param File The path to the image file to be generated.
*/
dot_prefix(Prefix, File) :-
    prepare_cmd(File, Stream),
    draw_prefix(Prefix, Str),
    write(Stream, Str),
    close(Stream).

/**
 dot_hierarchy(+File: term)

Create an image file with all the nodes and their is-a relationships.

The image type is deduce by the File extension. 'example.png' will create a PNG
image.

@param File The path to the image file to be generated.
*/
dot_hierarchy(File) :-
    prepare_cmd(File, Stream),
    draw_hierarchy(Str),
    write(Stream, Str),
    close(Stream).
