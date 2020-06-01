# Zen Database--Functional Requirements, version 0.5

This is a functional requirements specification for one or more databases
planned for storing the structure and content of a web site's pages. 
Zen is the working name of a web application being designed and built.
The 'Zen Database' is the database used to store the HTML documents
and document fragments that Zen serves.

## The alpha prototype

*Nota bene:* Beta features should be kept in mind when architecting
the alpha prototype. See the next section of this document for the
additional features to be added to the beta prototype.

1. The Zen Database will store complete representations of HTML documents
and document fragments (see [the DocumentFragment
API](https://developer.mozilla.org/en-US/docs/Web/API/DocumentFragment))
so that Zen can serve them in response to HTTP requests.
The storage will not be in the form of HTML text but, rather,
in the form of serialised trees.
Serialisation is the process of converting a live, in-memory object
to a form that can be stored on disk or transmitted over a network link.
The Zen Database will convert (deserialise) a serialised representation
into an HTML document.

2. Each node in a stored tree will represent an HTML element or node.
The distinction between element and node is made so that the Text
interface of the DOM can be accomodated. (See [the Text interface
API](https://developer.mozilla.org/en-US/docs/Web/API/Text).)

3. Similar to Git, the Zen Database will provide access to individual
objects and groups of objects. In Git, a blob stores a file's contents,
but in the Zen Database, a Lisp object stores an HTML element's contents
and nodeName.
(See [the nodeName
property](https://developer.mozilla.org/en-US/docs/Web/API/Node/nodeName).
Note that for nodes that are HTML elements, the nodeName is also the
tagName.)
In Git, a tree names and stores a group of files, but in the Zen Database,
a tree stores an HTML document or document fragment.

4. The Lisp object representing an HTML element or node will be a
CLOS object with slots representing the HTML element's attributes.
In the design of the Zen Database API, thought should be given to the
possibility of using regular non-CLOS objects for elements and nodes
that have no attributes, to economise memory and CPU time.

5. For now, the above requirements will allow new pages to be 
edited by a single-page web application and uploaded to Zen,
but we should design the Zen Database keeping in mind the following
future features so that the first design does not have to be completely
or mostly discarded:

    a. storing a tree;

    b. pruning a tree;

    c. grafting a document fragment (i.e. tree; or node, in the edge case)
to a tree;

    d. undoing an operation;

    e. redoing an operation;

    f. multiple-level undoing of operations;

    g. multiple-level redoing of operations; and

    h. retrieving any state in a tree-structured history (i.e. history with
multiple paths and subpaths).

6. The Zen Database must support transactions. A suitably authorized and
authenticated web session must be able to request a transaction such as
a - h described in point 5. Using a reliable HTTP connection, Zen should
only reply with a [success code](https://mzl.la/3b6lPHG) if the transaction
succeeds, so the client will know the state of the Zen Database.
The Zen Database must ensure that if there is a failure that causes
the transaction to succeed without the client being informed,
the Zen Database must reject a reinitiation of the transaction
and send a notification to the client that the transaction succeeded
during a previous attempt.

## The beta prototype

Features of the beta prototype will depend upon the following features:

1. Each node should contain only its [tagName](https://mzl.la/3b95W2l)
or [nodeName](https://mzl.la/2zcB5om) and
[attributes](https://mzl.la/2zcB5om).
Tree structures should be maintained as a metastructure.

2. Histories should be maintained as a meta-metastructure.

3. Any node in a tree that represents a full or partial web page should
be stored in such a way that the database can be content-addressed by
certain selectable parts of the node:

    a. the node type (tagName, e.g. DIV, or nodeName, e.g. TEXT_NODE);

    b. a subset of the node's attributes; and

    c. a subset of (a) and (b) and a subset of the descendents of the node.
