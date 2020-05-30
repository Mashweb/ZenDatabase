# Zen Database--Design Requirements, version 0.6

This is a design specification for one or more databases
planned for storing the structure and content of a web site's pages. 
Refer to the functional requirements document as an antecedent.
All the requirements described in that document should be adhered to.
Zen is the working name of a web application being designed and built.
The 'Zen Database' is the database used to store the HTML documents
and document fragments that Zen serves.

The interface is a web interface inspired by
the CRUD and HTTP verbs and paths enumerated in table form in the document
[Rails Routing from the Outside In](https://edgeguides.rubyonrails.org/routing.html)
and by Aaron Swartz's incomplete book 
[*A Programmable
Web*](https://www.morganclaypool.com/doi/pdf/10.2200/S00481ED1V01Y201302WBE005).
The intent is that the interface will implement a properly designed web service,
i.e. it will be a RESTful interface, designed the way web services
are supposed to be designed.

A note is appropriate here to avoid confusion.
In this document the term *DOM* refers to a tree of document nodes with
a single root.
(See
[MDN's description of the Node interface](https://mzl.la/2TrWCR4).)
This is not the only way the term is used; others often refer to all the
nodes in a document as being the DOM.

## The alpha prototype

*Nota bene:* Beta features should be kept in mind when architecting
and programming the alpha prototype. See the next section of this
document for the additional features to be added to the beta prototype.

*Nota bene:* The Database should be designed to accomodate the potential
feature of allowing *on-the-fly* creation of new groups of HTTP requests
that apply to specialized types or classes of DOMs.
For example, in addition to the "GET /doms" request type described below,
it should be possible
to define new request type "GET /mashweb-articles" to retrieve DOMs
for application-defined mashweb-articles
or new request type "GET /mashweb-forum-messages" to retrieve DOMs
for application-defined mashweb-forum-messages.
The particularly important request type "GET /headnodes" will retrieve DOMs
that only make sense when appended to the `<head>` of an HTML document.
It should be possible to define these requests and responses
without shutting down or reconfiguring Zen or the Zen Database.
Access to this feature would be controlled
via credentials and authorizations specific to the type or class of DOMs.
Where particular credentials and authorizations are not required, the
Authorization headers are not required.

*Nota bene:* This design specification does not define the
server responses to GET requests where the Accept header is not `text/plain`.
The namespace for Zen Database queries must be segregated
from a namespace or namespaces for other types of data, such as HTML pages.
The same segregation could be enforced using HTTP port numbers,
but using the Accept header is expected to be simpler.

This design specification will describe the interface to the Zen Database.
In the alpha prototype, no credentials or roles will be necessary,
but in a later version, read and write accesses to the Database
will be controlled by user credentials and user roles.
Zen will force all access to the Zen Database to use HTTPS.
The HTTP requests mentioned below might include *or require* additional
request headers besides those mentioned,
such as `Accept-Charset`, `Accept-Encoding` (gzip only), `Cache-Control`,
`Connection`, `Content-Encoding`, etc.
Some of the required request headers might be missing from this specification.
This is due to the author's inexperience working directly with
request headers.
The data shown in the requests will be in the form plain text.
This is indicated by the request header `Content-Type: text/plain`.
Here are the HTTP requests for Zen:

1. GET /doms - get all the DOMs (typically, 'divisions', 'cards', 'blocks',
'sections', 'articles', 'messages', and other application-defined DOMs)
stored in the Zen Database.
The Database's HTTP response should be a serialised Scheme-language list.
Each item in the list should be a sublist representing one of the DOMs.
This request corresponds to the first row of the table in the Rails guide
mentioned above.
An example of this HTTP GET request is:

	```
        GET /doms HTTP/1.1
        Accept: text/plain
        Authorization: Basic Zm9vOmJhcg==
	```

	where 'Zm9vOmJhcg==' is 'username:password' encoded in base64,
for some username and password.
The above Accept header specifies that the response should be a list of
DOMs represented in Scheme language.
The details of the Scheme-encoded text are specified later in this document.

2. In the alpha and beta implementations of the Zen Database, no request will be defined
corresponding to the verb and path in the second row of the Rails guide table,
i.e. the request that returns an HTML form for creating a new photo.

3. POST /doms - create a new Zen Database entry storing a serialized DOM.
Typically a DOM will be a 'division', 'card', 'block',
'section', 'article', 'message', or other application-defined DOM.
The response must return the Zen Database's index of the new entry.
An example of this POST request is:

	```
        POST /dom HTTP/1.1
        Content-Length: ...
        Content-Type: text/plain
        Authorization: Basic Zm9vOmJhcg==
	
        ((DIV (id "main") (style "width:100px;")) "Main" ((OL) ((LI) "1") ((LI) "2")) ((P)))
	
	```

	following the same authorization pattern as the 'GET /doms' request above.
The DOM in this example would correspond to the HTML representation

	```
        <div id="main" style="width:100px;">Main<ol><li>1</li><li>2</li></ol><p></p></div>
	```

	Note that it is uncertain that there is a single BiwaScheme function or macro
that will send a POST message with this type of Content-Type header.
If there is not, then lower-level functions and/or macros will have to be used
in the web browser.

4. GET /doms/:id - get the DOM indexed by :id,
where :id is the index defined by the Zen Database at the time the entry
for a DOM was made. The Database's HTTP response should be a serialised
Scheme-language list representing the indexed DOM.
An example of this HTTP GET request is:

	```
        GET /dom/1372 HTTP/1.1
        Accept: text/plain
        Authorization: Basic Zm9vOmJhcg==
	```

	following the same pattern as the 'GET /doms' request above
except for the URL.
When a future version of the Zen Database is able to translate any
of its entries to HTML, it should respond to

	```
        GET /dom/1372 HTTP/1.1
        Accept: text/html
        Authorization: Basic Zm9vOmJhcg==
	```

	with a HTML document or HTML document fragment.

5. In the alpha implementation of the Zen Database, no request will be defined
corresponding to the verb and path in the fifth row of the Rails guide table.

6. In the alpha implementation of the Zen Database, no request will be defined
corresponding to the verb and path in the sixth row of the Rails guide table.

7. DELETE /doms/:id - delete the DOM indexed by :id,
where :id is the index defined by the Zen Database at the time the entry
for a DOM was made. An example of this DELETE request is:

	```
        DELETE /dom/1372 HTTP/1.1
        Authorization: Basic Zm9vOmJhcg==
	```

	following the same pattern as the 'GET /doms' request above
except for the URL.

## The format of a Scheme-language list representing a DOM

1. No colons or apostrophes exist anywhere.

2. Strings are only for literal text or attribute values.

3. Any attribute is usually a 2-item list; sometimes a 1-item list.

4. A tag is a ((tag-name attributes...) contents...).

5. A "hole" to be replaced by a run-time calculation is a symbol in place of a tag
or an attribute value.

An example is:

```
((DIV (id "main") (style "width:100px;")) "Main" ((OL) ((LI) "1") ((LI) "2")) ((P)))
```

corresponding to the HTML:

```
<div id="main" style="width:100px;">Main<ol><li>1</li><li>2</li></ol><p></p></div>
```

An example of "holes" is:

```
(('sect (id 'label) (style "width:100px;")) 'label ((OL) ((LI) "1") ((LI) "2")) ((P)))
```
