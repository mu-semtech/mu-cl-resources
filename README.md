# JSONAPI to SPARQL, and back

mu-cl-resources provides a [JSONAPI](http://jsonapi.org) compatible interface to the content specified in the configuration.  Most configuration occurs in the configuration/domain.lisp file, an example of which can be found in this repository.

Most configuration happens in the domain.lisp file.  See `configuration/domain.lisp` for an example.  This file defines the glue between the JSON world and the RDF world.  When defining a model, be sure to have a good idea on what both worlds will look like.

The documentation offered here is not exhaustive.  This component handles a wide variety of use-cases and has support for esotheric features for experimentation, which may land in the core at a later time.  As such, some features which the component offers are not documented in this readme.

## Brief hands-on overview

This service is driven from the domain.lisp file which you should adapt to describe your domain.  In this section we briefly describe how everything is wired together, and how you can quickly get an API up and running.

mu-cl-resources is driven from the domain.lisp file.  This file describes the connection between the JSONAPI and the semantic model.  Secondly, there is the repository.lisp file, in which you can define new prefixes to shorten your domain description.  This repository contains an example of both files in the configuration folder.

### Setup in mu.semte.ch project

It is common for a mu.semte.ch project to contain mu-cl-resources.  The default blueprint for a mu.semte.ch project, [mu-semtech/mu-project](https://github.com/mu-semtech/mu-project), contains mu-cl-resources.  Furthermore, there is an example written to config/domain.lisp.  This is where we will apply our changes.  Secondly, there is the repositories.lisp, in which you can change add prefixes to use in your domain specification.  If you're setting up mu-cl-resources in a mu.semte.ch project, be sure to check out the documentation of the dispatcher to ensure mu-cl-resources receives your queries.

### /configuration/domain.lisp

The domain.lisp contains resource definitions for each file in the application.  These resource definitions provide a three-way connection:

  - It names things to make connections within the domain.lisp file
  - It describes the properties as seen through the json api
  - It describes the semantic model used in order to implement the json api

Each resource definition is a combination of these three views.  Let us assume an exmaple using [foaf](http://xmlns.com/foaf/0.1/).  In our example, we will model a Person, having one or more online accounts.  This model can be vizualised using [WebVOWL](http://visualdataweb.de/webvowl/#).

Intermezzo: mu-cl-resources is mainly configured in lisp.  Lisp uses parens () for grouping content.  If a paren is followed by a word, that word tends to indicate the content of the group.  If there is no word, it tends to be a list.  Other characters, like the backtick (`) or the comma (,) are best copied from examples.

    (define-resource person ()
      :class (s-url "http://xmlns.com/foaf/0.1/Person")
      :properties `((:name :string ,(s-url "http://xmlns.com/foaf/0.1/name")))
      :resource-base (s-url "http://my-application.com/people/")
      :on-path "people")

A simple definition of a person uses the foaf vocabulary to write the person and the person name.

  - *Line 1* contains `define-resource person`, which indicates that we'll create a new endpoint which we will name `person` in this file.  It is most customary to use a singular name for this name.
  - *Line 2* specifies that the RDF class to which the person belongs in the triplestore is [foaf:Person](http://xmlns.com/foaf/0.1/Person).
  - *Line 3* specifies a singular property of the person.  The JSONAPI will assume content of type `string` is stored in the json key `data.attributes.name` (because of `:name`).  This value is connected to our resource in the triplestore by the perdicate [foaf:name](http://xmlns.com/foaf/0.1/name).  Note that this word may contain dashes, but not capitals (capitals are ignored).
  - *Line 4* indicates the URI to use in the triplestore when we create new resources of this type.  The supplied url is postfixed with a UUID.  Line 5 specifies the endpoint on which we can list/create/update our resource.  In our case, requests to `/people` are mapped to this resource.

Assuming the foaf prefix is defined, we can make this example slightly easier to read.  Note the use of `s-prefix`.

    (define-resource person ()
      :class (s-prefix "foaf:Person")
      :properties `((:name :string ,(s-prefix "foaf:name")))
      :resource-base (s-url "http://my-application.com/people/")
      :on-path "people")

This code sample implements the same functionality as the example above, yet it is easier on the eyes.

You may have noticed the double opening parens on line 3, after the `:properties` keyword.  We can insert multiple properties if desired.  Ensuring we have the right amount of opening and closing parens, we can update our example to also contain the age of the person, expressed as a number.

    (define-resource person ()
      :class (s-prefix "foaf:Person")
      :properties `((:name :string ,(s-prefix "foaf:name"))
                    (:age :number ,(s-prefix "foaf:age")))
      :resource-base (s-url "http://my-application.com/people/")
      :on-path "people")

With this minor change, our person supports the name and age attributes.

Most resources link to other resources.  Let's first define a second resouce, an [OnlineAccount](http://xmlns.com/foaf/0.1/OnlineAccount).

    (define-resource account ()
      :class (s-prefix "foaf:OnlineAccount")
      :properties `((:name :string ,(s-prefix "foaf:accountName")))
      :resource-base (s-url "http://my-application.com/accounts/")
      :on-path "accounts")

The definition of this `account` resource is very similar to that of the `person` resource.  How do we link a person to an account?  Assuming the person has many accounts, we link by using the `:has-many` keyword.

    (define-resource person ()
      :class (s-prefix "foaf:Person")
      :properties `((:name :string ,(s-prefix "foaf:name"))
                    (:age :number ,(s-prefix "foaf:age")))
      :has-many `((account :via ,(s-prefix "foaf:account")
                           :as "accounts"))
      :resource-base (s-url "http://my-application.com/people/")
      :on-path "people")

The statement on lines 5 and 6 specifies that a `person` may link to many resources of type `account`.  In the triplestore, the link can be found by following the [foaf:account](http://xmlns.com/foaf/0.1/account) property, originating from the person's URI.  This relationship is exposed to the JSON API by using the relationship name "accounts".  Hence a GET to `/people/42/accounts` would yield the accounts of the person with UUID 42.

How about getting the person which links to this account.  There is only a single person connected to an account.  Hence we can use the `has-one` keyword to symbolize this.  In the semantic model of the triplestore, the relationship uses the [foaf:account](http://xmlns.com/foaf/0.1/account) property going from the person to the account.  Finding the person for an account therefore means we have to follow the same relationship in the other direction.  We can add the option `:inverse t` to any relationship to make the semantic model follow the inverse arrow.  Here, the key in the json body will be `owner` rather than person.

    (define-resource account ()
      :class (s-prefix "foaf:OnlineAccount")
      :properties `((:name :string ,(s-prefix "foaf:accountName")))
      :has-one `((person :via ,(s-prefix "foaf:account")
                         :inverse t
                         :as "owner"))
      :resource-base (s-url "http://my-application.com/accounts/")
      :on-path "accounts")

The complete setup of our user and account looks as follows:

    (define-resource person ()
      :class (s-prefix "foaf:Person")
      :properties `((:name :string ,(s-prefix "foaf:name"))
                    (:age :number ,(s-prefix "foaf:age")))
      :has-many `((account :via ,(s-prefix "foaf:account")
                           :as "accounts"))
      :resource-base (s-url "http://my-application.com/people/")
      :on-path "people")

    (define-resource account ()
      :class (s-prefix "foaf:OnlineAccount")
      :properties `((:name :string ,(s-prefix "foaf:accountName")))
      :has-one `((person :via ,(s-prefix "foaf:account")
                         :inverse t
                         :as "person"))
      :resource-base (s-url "http://my-application.com/accounts/")
      :on-path "accounts")

### /configuration/repositories.lisp

The previous example used the foaf prefix in order to denote classes and properties.  The `/configuration/repositories.lisp` allows you to specify your own prefixes to use in your definitions.  A good source for commonly used abbreviations is [prefix.cc](https://prefix.cc).

    (add-prefix "foaf" "http://xmlns.com/foaf/0.1/")

### Resulting API
We intend to support the full spec of [JSONAPI](http://jsonapi.org).  Support for this API comes out of the box with frameworks such as [ember-data](https://github.com/emberjs/data).  Most of what you read there will work, errors being a notable exception.  Here, we list some common calls which you could execute using the resources specified above.

  - `# GET /people`
  - `# GET /people/0b29a57a-d324-4302-9c92-61958e4cf250/accounts`
  - `# GET /people?filter=John`
  - `# GET /people?filter[age]=42`
  - `# GET /people?include=accounts`
  - `# GET /people?filter[:exact:name]=John%20Doe`
  - `# GET /people?sort=age`
  - `# GET /accounts?sort=-person.age`

  - `# POST /people/0b29a57a-d324-4302-9c92-61958e4cf250`
  - `# PATCH /people/0b29a57a-d324-4302-9c92-61958e4cf250`
  - `# PATCH /people/0b29a57a-d324-4302-9c92-61958e4cf250/relationships/accounts`
  - `# DELETE /people/0b29a57a-d324-4302-9c92-61958e4cf250/relationships/accounts`
  - `# DELETE /people/0b29a57a-d324-4302-9c92-61958e4cf250`

More information on each of these calls can be found throughout this document.

### More configuration options

The complete mu-cl-resources instance, a specific resource, as well as a property can have options set to override the default behaviour.

  - *mu-cl-resources options:* These are specified by the `defparameter` expression as a top-level form in the domain.lisp file.
  - *resource specific options:* The keyword `:features` at the same level as the `:class` may specify options which alter the behaviour of the specific resource.
  - *property options:* Symbols following the definition of a single property may give mu-cl-resources extra information on how the property will be used.

## Defining resources

As the integration with the frontend data-store is handled automatically, most of your time with mu-cl-resources will be spent configuring resources.  This overview provides a non-exhaustive list of the most common features of mu-cl-resources.

Each defined resource is specified by the `define-resource` construction.  An example could look like this:

    (define-resource person ()
      :class (s-prefix "foaf:Person")
      :properties `((:name :string ,(s-prefix "foaf:name")
                           :required)
                    (:age :number ,(s-url "http://xmlns.com/foaf/0.1/age")))
      :has-one `((location :via ,(s-prefix "foaf:based_near")
                           :as "location"))
      :has-many `((account :via ,(s-prefix "foaf:account")
                           :as "accounts")
                  (document :via ,(s-prefix "foaf:publications")
                            :as "publications"))
      :features '(include-uri)
      :resource-base (s-url "https://my-application.com/people/")
      :on-path "people")

We will use this example to explain how various features in mu-cl-resources work.

### Overview of keys

Each call to `define-resource` starts out with the name of the resource (used when referring to the resource internally), a set of empty parens (for future use), and a set of key-value pairs.  This section gives a brief overview of the valid keys, and what their use is.

  - *`:class`* Sets the RDF Class to which instances should belong.  Use `s-url` when setting the full URL.
  - *`:properties`* Describes the properties (currently named attributes in the json response) of the resource.
  - *`:has-one`* Describes relationships of which at most one is expected to exist.
  - *`:has-many`* Describes relationships of which zero or more are expected to exist.
  - *`:features`* Optional features to be used in this resource.  Our example indicates the URI should be returned as an attribute.
  - *`:resource-base`* An `s-url` containing the prefix for the URI used when creating new resources.
  - *`:on-path`* The path on which the resource is supplied, this corresponds to the `type` property in the JSON body.  JSONAPI advises to use the plural form here.

### Simple properties

The properties section in the mu-cl-resources configuration corresponds to the attributes in the JSON payload.  This section describes how to set properties.

The properties section in our example looks like:

      :properties `((:name :string ,(s-prefix "foaf:name")
                           :required)
                    (:age :number ,(s-url "http://xmlns.com/foaf/0.1/age")))

All properties are contained in a backtick (`) quoted list (note that this is *not* a regular quote (').  Each property description is itself contained in a list.  The list contains three ordered elements by default:

  1. *key name* First option is the key name (ie: `:name`).  It is downcased and used as the JSON key of the attribute.
  2. *type* Second option is the type of the attribute.  This ensures we correctly translate the attribute from JSON to SPARQL and vice-versa.  Use `,(s-url "...")` for full URLs or `,(s-prefix "...")` for shorthand names.
  3. *rdf property* Third option is the RDF property of the attribute.  This is the URL used on the arrow of the RDF model in the triplestore.
  4. *options* Any other keys following the three elements above are options to describe something extra about the resources.  The format of these may change over time.

A wide set of types is supported.  Extensions are necessary in order to implement new types:

  - *string* A regular string
  - *number* A number (can be integers or floats)
  - *boolean* A boolean, true or false
  - *date* A date as understood by your triplestore
  - *datetime* A date and time combination, as understood by your triplestore
  - *url* A URL to another resource
  - *uri-set* An array of URIs
  - *string-set* An array of strings
  - *language-string* A string which has a language connected to it (may contain multiple languages)
  - *language-string-set* An array of strings which have a language connected to it (may contain multiple languages per answer)
  - *g-year* Experimental: A specific representation of a year
  - *geometry* Experimental: A geometry-string in a format your triplestore understands

### Relationships

Relationships are split into single value `:has-one` and multiple value `:has-many` relationships.  In both cases, having a value is optional.

      :has-one `((location :via ,(s-prefix "foaf:based_near")
                           :as "location"))
      :has-many `((account :via ,(s-prefix "foaf:account")
                           :as "accounts")
                  (document :via ,(s-prefix "foaf:publications")
                            :as "publications"))

Both `:has-one` and `:has-many` allow to specify more than one relationship.  The outermost parens group all relationships.  We use the backtick (`) rather than quote (') in order to denote the list of properties.

The format of a single value consists of the internal name of the resource to be linked to, followed by keyword properties describing the relationship.

  - *`:via`* Contains the URI of the RDF property by which the related objects can be found.
  - *`:as`* Contains the attribute in the JSON API.
  - *`:inverse`* Optional, when set to `t` it inverses the direction of the relationship supplied in `:via`.

## Querying the API

mu-cl-resources provides extensive support for searching and filtering through results.  A notable exception is fuzzy text search as that is not a built-in for standard SPARQL.

The [JSONAPI spec](http://jsonapi.org/format/#fetching-filtering) leaves all details on searching, except for the used url parameter, open to the implementors.  Our specification leverages the breath of the Linked Data model to enable powerful searches.

We will mostly base ourselves on the example which was previous supplied.

    (define-resource person ()
      :class (s-prefix "foaf:Person")
      :properties `((:name :string ,(s-prefix "foaf:name")
                           :required)
                    (:age :number ,(s-url "http://xmlns.com/foaf/0.1/age")))
      :has-one `((location :via ,(s-prefix "foaf:based_near")
                           :as "location"))
      :has-many `((account :via ,(s-prefix "foaf:account")
                           :as "accounts")
                  (document :via ,(s-prefix "foaf:publications")
                            :as "publications"))
      :features '(include-uri)
      :resource-base (s-url "https://my-application.com/people/")
      :on-path "people")

    (define-resource account ()
      :class (s-prefix "foaf:OnlineAccount")
      :properties `((:name :string ,(s-prefix "foaf:accountName")))
      :has-one `((person :via ,(s-prefix "foaf:accounts")
                         :inverse t
                         :as "owner"))
      :resource-base (s-url "http://my-application.com/accounts/")
      :on-path "accounts")

Various resources have not been defined in our example.  `location` and `document` are left as an exercise to the reader.

### Basic filtering

Basic searching is done by using the `?filter` query parameter.  We can search for "John Doe" in any key of our `person` by sending

    GET /people?filter=John Doe

If we want to search only for names matching "John Doe", we can limit the search to that keywoard.

    GET /people?filter[name]=John Doe

All of these searches are case-insensitive, and they search for any field which contain the contents (John Doelala) would therefore be returned too.  We can make an exact match with a special search.

    GET /people?filter[:exact:name]=John Doe

All filter modifiers start with a colon (:) followed by the name of the filter, followed by a configuration parameter.  This specific filter will search for a name with exactly "John Doe" in its contents.  No more, no less.

### Filtering relationships

Filters can also be scoped to relationships.  JSONAPI guarantees that attributes and relationships will never share a name.  Hence we can use the same syntax as we used to identify an attribute in order to identify a relationship.

    GET /people?filter[account]=GitHub

Searches for people which have an account named Mozilla.  It is also possible to search for specific properties, or to apply special filters to this.  Assuming we want to find all accounts for people whose *name* contains John, we'd search for the following:

    GET /accounts?filter[owner][name]=John

We can add more filters as we please.  We can search for all documents belonging to a person named John who has an account named exactly `Dropbox`.

    GET /documents?filter[document-owner][name]=John&filter[document-owner][account][:exact:name]=Dropbox

This will return all documents belonging to anyone named John in an account named exactly `Dropbox`.

### Sorting

Sorting is specified in [JSONAPI](http://jsonapi.org/format/#fetching-sorting) somewhat more extensively.  What is specified there works, but is augmented to sorting by relationships.

Let's sort our users by their name

    GET /people?sort=name

Let's sort by age, descending and then by name

    GET /people?sort=-age,name

Sorting by relationships allows us to sort accounts by the name of their owner

    GET /accounts?sort=owner.name

### Filtering on exact relationships

Objects in JSONAPI are identified by their type and their identifier.  To find all objects that link to another object through some relationship, we can use this feature.

Imagine we want to find all users who have published a specific paper with ID 42 (the generated identifiers are UUIDs, but for the sake of this example, we'll assume the document has ID 42.

    GET /people?filter[publications][:id:]=42

This pattern becomes more intersting when we start searching for more than one document.  To search for all people that have published one of three papers (ids being 42, 45, 66), we can build a comma-seperated list of these ids.

    GET /people?filter[publications][:id:]=42,45,66

This filter can be combined with the other filters to provide very extensive search interfaces.

### Special filters

Aside from regular text searches, a set of custom filters have been added.  These filters are the last component of a search, and are easy to identify as they start with a colon (:).  Following is a brief list of filters which exist.  This list may be extended over time.

- *:uri:* Search for the URL of a relationship.

    GET /people?filter[accounts][:uri:]=http://my-application.com/people/42

- *:exact:* Searches for the exact string as a value

    GET /people?filter[accounts][:exact:name]=Dropbox

- *:gt:* Ensures the property has a larger value than the supplied value

    GET /people?filter[:gt:age]=42

- *:gte:* Ensures the property has a value larger than or equal to the supplied value
- *:lt:* Ensures the property has a smaller value than then supplied value
- *:lte:* Ensures the property has a smaller value than then supplied value or is equal to the supplied value

- *:has-no:* Ensures the supplied relationship does not exist.  An example could list all people without an account.  The supplied value is not used.  Syntax may be subject to change.

    GET /people?filter[:has-no:account]=yes

- *:has:* The inverse of `:has-no:` forces the relationship to exist.  Syntax may be subject to change.

### Including results

There exists an optional part of [the JSONAPI spec](http://jsonapi.org/format/#fetching-includes) which handles the inclusion of related resources.  It specifies how you can request resources related to your response so you don't have to make too many calls to the server.

In order to add resources you want to see returned, specify the resources by using the `include` query parameter.  We could request all people with their related publications like so:

    GET /people?include=publications

This can be combined with other specifications.  The previous call returned all people, even if there was no publication.  We can only yield people which have a publication, and yield the publications too.

    GET /people?include=publications&filter[:has:publications]=true

You can also get nested results.  We can list accounts with the related people and the related publications:

    GET /accounts?include=owner.publications

We can also include the location if we'd want to render that too:

    GET /accounts?include=owner.publications,owner.location

Combining included results with filters makes for a very powerful API.  Note that including results does require more queries to be sent to the database.

### Pagination

Pagination is also included in [the JSONAPI spec](http://jsonapi.org/format/#fetching-pagination).  All resources have it enabled by default.  We support the `page[number]` and `page[size]` variant.

The default page size can be configured by setting the `*default-page-size*` to the desired amount of pages.  It defaults to 20.  Single resources can opt out of pagination.  Single requests can overwrite the size of the pagination.  In practice, we discover that there's nearly always an upper bound you want to set for the pagination to ensure things don't break in the frontend.  Included resources (using the `include` query parameter) are never paginated.

If you want to override the default page size in your `domain.lisp`, add the following code:

    (defparameter *default-page-size* 50)

You can also choose to set the `MU_DEFAULT_PAGE_SIZE` environment variable before mu-cl-resources boots.

If you want to opt out of pagination for a specific resource, add the `no-pagination-defaults` feature.

    (define-resource account ()
      :class (s-prefix "foaf:OnlineAccount")
      :properties `((:name :string ,(s-prefix "foaf:accountName")))
      :has-one `((person :via ,(s-prefix "foaf:accounts")
                         :inverse t
                         :as "owner"))
      :features '(no-pagination-defaults)
      :resource-base (s-url "http://my-application.com/accounts/")
      :on-path "accounts")

If you want to override the page size for a specific request, you can do so by suppling the `page[size]` query parameter:

    GET /people?page[size]=100

If you want to request a different page and a different page size, supply both `page[size]` and `page[number]`:

    GET /people?page[size]=42&page[number]=3

If you want mu-cl-resources to yield the total amount of results in the `meta` portion of the response, set `*include-count-in-paginated-response*` to `t` in your `domain.lisp`.

    (defparameter *include-count-in-paginated-response* t)


### Sparse fieldsets

Sparse fieldsets is also [a feature of JSONAPI](http://jsonapi.org/format/#fetching-sparse-fieldsets).  If your model has many attributes, but you do not intend to render them on the frontend, you can opt out of fetching them.  Use the `fields` query parameter to fetch only the necessary results.

The `fields` parameter needs to be scoped to the type of the objects for which you want to limit the returned properties.  If we'd want to return only the name for a people listing, we'd use the following:

    GET /people?fields[people]=name

This becomes more intersting as we include more resources.  Say that I include the publications, but only the title and publication-date of these should be taken into account.

    GET /people?include=publications&fields[people]=name&fields[documents]=publication-date,title

You can add filters to ensure we only get authors of papers aged over 42 which have a Dropbox account:

    GET /people?filter[accounts][:exact:name]=Dropbox&filter[:gt:age]=42&filter[:has:publication]=true&include=publications


## Caching

Efficient caching is a complex story.  mu-cl-resources ships with support for two levels of caching: an internal cache which can keep track of object properties and counts, and an external cache which can cache complete queries.

Both of these caches are subject to change in their implementation, but the end-user API should stay the same.

### External cache

Caching requests is more complex for a JSONAPI than for a web page.  A single update may invalidate a wide range of pages, but it should not invalidate too many pages.  As such, we've written a separate cache for JSONAPI-like bodies.  Find it at [mu-semtech/mu-cache](https://github.com/mu-semtech/mu-cache).

In order to enable the external cache, you have to set the `*supply-cache-headers-p*` parameter to `t` in your `domain.lisp`.

    (defparameter *supply-cache-headers* t)

Note: mu-cl-resources speaks the protocol of this cache, but does not update the cache yet when external resources update the semantic model.

### Internal cache

In order to opt in to the internal model caching, set `*cache-model-properties*` to `t`.  Note that this currently assumes mu-cl-resources is the only service altering the resources.

    (defparameter *cache-model-properties* t)

Separate from this, you can choose to also cache the count queries.  On very large datasets, counting the amount of results may become expensive.  Set the `*cache-count-queries*` parameter to `t` for this.

    (defparameter *cache-count-queries* t)

## Features to be documented

### Authorization

The current implementation of mu-cl-resources ships with an extensive authorization model.  This model is being revized as we aim to cut authorization out of the microservices and convert it into a layer on top of the SPARQL endpoint.  Thereby abstracting authorization and ensuring it is configured correctly throughout the whole stack.

Current configuration follows a model which has a grant token on a per-object basis.  The following query builds the connection to the token.

    (format nil (s+ "~A session:account/^foaf:account/((a/auth:belongsToActorGroup*/auth:hasRight)|(auth:belongsToActorGroup*/auth:hasRight)) ~A. ~&"
                    "~A auth:hasToken ~A. ~&"
                    "~A auth:operatesOn/(~:[~;(^auth:belongsToArtifactGroup*/^a)|~](^auth:belongsToArtifactGroup*)) ~A. ")
            (session-uri) token-var
            token-var token
            token-var allow-target-inheritance source)

We do not document this in depth as support for this is only useful at this time.

### Using different sparql stores

The default connector assumes you'll connect to a Virtuoso endpoint.  However, this is not a requirement.  By adding the necessary content into the dependencies folder, you can choose to enable a different store.  This may mock specific details of the store, such as wrapping incorrect results or choosing a different connector point.  An example of this can be found at [madnificent/cl-fuseki-blazegraph-plugin](https://github.com/madnificent/cl-fuseki-blazegraph-plugin)

### Generators

There are a few applications which build on top of mu-cl-resources to generate meaningful content.

One example is the mu-application-generator which scaffolds an resource editing application based on your API, another is [mu-semtech/cl-resources-openapi-generator](https://github.com/mu-semtech/cl-resources-openapi-generator).

### Configuration parameters

There are a wider set of configuration parameters which allow you to configure mu-cl-resources to your liking.  Including enabling experimental features.  These are not documented for now.  Many can be found in the release notes.

### Separate domain.lisp files

The domain.lisp file exists in multiple formats.  As such, experimental drafts in json also exist.  It is also possible to split your domain.lisp file in multiple files and ensure they're all loaded.  In order to load a second file, you could use `read-domain-file` in your domain.lisp.  It understands lisp files and json files.

    (read-domain-file "my-main-domain.lisp")
    (read-domain-file "my-other-domain.lisp")

## High-level questions

This component is in use in a great deal of mu.semte.ch stacks.  A high-level picture as to why we built this component, and how it fits in the architecture, is suiting.

### Why this component?

The idea of mu-cl-resources is to provide an API which can easily be consumed by frontend applications.  The most common calls which a backend offers are repetitive and boring to implement.  Good developers should not be bothered with these boring details.  Instead, a declarative configuration can get rid of all of this.

Furthermore, the wide reuse of this component provides us with a rich shared code-base.  The wide use of the code-base allows us to unlock an enormous amount of shared value by implementing small features in this codebase.  On the flipside, we should take great care that this repository does not get out of hand, as that may impact many applications.

## Is this a microservice?

Much of the code needed to implement this component was written specifically for this component.  It can be argued that this component is therefore not a typical microservice component.  We would argue differently.

From a consumer's perspective, the configuration supplied of mu-cl-resources is the relevant portion of this service.  The API which is offered may be broad, but the configuration to maintain is comparatively small.  As the consumer has a good overview of the code necessary to configure this service, this could be considered a microservice from the consumer's perspective.

From the developer's perspective things may look different.  It is clear that mu-cl-resources quite a broad code base.  Replacing it with other components has proven to be more expensive than expected.  Although this holds, many of the used functions could (and should) be abstracted into separate libraries over time.  Furthermore, we expect a similar code-base to work in different languages, although more code may need to be written.
