---
title: Hypertrout
author: Oskar Wickström
---

## Purpose

The purpose of this
package,
[Hypertrout](https://github.com/owickstrom/purescript-hypertrout), is
writing web servers using the *type-level routing API*
in [Trout](https://github.com/owickstrom/purescript-trout).  It
provides a router middleware which, together with records of handler
functions for resources, and rendering instances, gives us a
full-fledged server.

## A Single-Resource Example

Let's say we want to render a home page as HTML. We start out by
declaring the data type `Home`, and the structure of our site:

``` {.haskell language=purescript include=docs/src/Site1.purs snippet=routing-type}
```

`Resource (Get Home HTML)` is a routing type with only one resource,
responding to HTTP GET requests, rendering a `Home` value as HTML. So
where does the `Home` value come from? We provide it using a *handler*
inside a resource record. A resource record for `Site1` would be some
value of the following type:

``` {.haskell}
forall m. Monad m => {"GET" :: ExceptT RoutingError m Home}
```

The resource record has fields for each supported HTTP method, with values
being the corresponding handlers. A resource record type, supporting both GET
and POST, could have the following type:

``` {.haskell}
forall m. Monad m => { "GET" :: ExceptT RoutingError m SomeType
                     , "POST" :: ExceptT RoutingError m SomeType
                     }
```

We can construct a resource record for the `Site1` routing type using `pure`
and a `Home` value:

``` {.haskell language=purescript include=docs/src/Site1.purs snippet=handler}
```

Nice! But what comes out on the other end? We need something that
renders the `Home` value as HTML. By providing an instance of
`EncodeHTML` for `Home`, we instruct the resource how to render.

``` {.haskell include=docs/src/Site1.purs snippet=encoding}
```

The `HTML` type is a phantom type, only used as a marker type, and the
actual markup is written in the `MarkupM` DSL from
[purescript-smolder](https://github.com/bodil/purescript-smolder).

We are getting ready to create the server. First, we need a value-level
representation of the `Site1` type, to be able to pass it to the
`router` function. For that we use
[Proxy](https://pursuit.purescript.org/packages/purescript-proxy/1.0.0/docs/Type.Proxy).
Its documentation describes it as follows:

> The Proxy type and values are for situations where type information is
> required for an input to determine the type of an output, but where it
> is not possible or convenient to provide a value for the input.

We create a top-level definition of the type `Proxy Site1` with the
value constructor `Proxy`.

``` {.haskell include=docs/src/Site1.purs snippet=proxy}
```

We pass the proxy, our handler, and the `onRoutingError` function for
cases where no route matched the request, to the `router` function.

``` {.haskell include=docs/src/Site1.purs snippet=router}
```

The value returned by `router` is regular middleware, ready to be passed
to a server.

``` {.haskell include=docs/src/Site1.purs snippet=main}
```

## Routing Multiple Resources

Real-world servers often need more than one resource. To combine
multiple resources, resource routing types are separated using the
`:<|>` operator, the type-level operator for separating
*alternatives*.

``` {.haskell}
RoutingType1 :<|> RoutingType2 :<|> ... :<|> RoutingTypeN
```

When combining multiple resources in a routing type, each resource has
to be named. The `:=` type-level operator names a resource, or another
nested structure of resources, using a Symbol on the left-hand side,
and a routing type on the right-hand side.

``` {.haskell}
"<resource-name>" := RoutingType
```

The following is a routing type for two resources, named `"foo"` and
`"bar"`:

``` {.haskell}
     "foo" := Resource (Get Foo HTML)
:<|> "bar" := Resource (Get Bar HTML)
```

Named routes can be nested to create a structure of arbitrary depth, a
practice useful for grouping related resources:

``` {.haskell}
type UserResources =
       "profile"  := Resource (Get UserProfile HTML)
  :<|> "settings" := Resource (Get UserSettings HTML)

type AdminResources =
       "users" := Resource (Get Users HTML)
  :<|> "logs"  := Resource (Get Logs HTML)

type MyNestedResources =
       "user"  := UserResources
  :<|> "admin" := AdminResources
```

### Example

Let's define a router for an application that shows a home page with
links, a page listing users, and a page rendering a specific user.

``` {.haskell include=docs/src/Site2.purs snippet=resources-and-type}
```

There are some new things in this code that we haven't talked about,
and some we touched upon a bit. Here's a walk-through of what's going
on:

-   `:<|>` is the type-level operator that, in general, separates
    alternatives. In case of resources, a router will try each route
    in order until one matches.
-   `:=` names a route, where the left-hand argument is a Symbol, the
    name, and the right-hand argument is a routing type. Named routes
    are combined with `:<|>`, as explained previously.
-   `:/` separates a literal path segment and the rest of the routing
    type. Note that a named routing type, created with `:=`, has no relation
    to literal path segments. In other words, if want a resource named
    `"foo"` to be served under the path `/foo`, we write:
    ``` {.haskell}
    "foo" := "foo" :/ ...
    ```
-   `Capture` takes a descriptive string and a type. It takes the next
    available path segment and tries to convert it to the given type.
    Each capture in a routing type corresponds to an argument in the
    handler function.
-   `:>` separates a routing type modifier, like `Capture`, and the rest
    of the routing type.

We define a resource record using regular functions on the specified data
types, returning `ExceptT RoutingError m a` values, where `m` is the monad of
our middleware, and `a` is the type to render for the resource and method.

``` {.haskell include=docs/src/Site2.purs snippet=handlers}
```

As in the single-resource example, we want to render as HTML. Let's
create instances for our data types. Notice how we can create links
between routes in a type-safe manner.

``` {.haskell include=docs/src/Site2.purs snippet=encoding}
```

The record destructuring on the value returned by `linksTo` extracts the
correct link, based on the names from the routing type. Each link will have a
type based on the corresponding resource. `user` in the previous code has
type `Int -> URI`, while `users` has no captures and thus has type `URI`.

We are still missing `getUsers`, our source of `User` values. In a real
application it would probably be a database query, but for this example
we simply hard-code some famous users of proper instruments.

``` {.haskell include=docs/src/Site2.purs snippet=get-users}
```

Almost done! We just need to create the router, and start a server.

``` {.haskell include=docs/src/Site2.purs snippet=main}
```

Notice how the `resources` record matches the names and structure of our
routing type. If we fail to match the type we get a compile error.

## Multi-Method Resources

So far we have just used a single method per resource, the `Get` method.
By replacing the single method type with a sequence of alternatives,
constructed with the type-level operator `:<|>`, we get a resource with
multiple methods.

``` {.haskell include=docs/src/MultiMethodExample.purs snippet=routing-type}
```

`MultiMethodExample` is a routing type with a *single resource*, named
`"user"`, which has *multiple resource methods*. Handlers for the
resource methods are provided as a record value, with field names
matching the HTTP methods:

``` {.haskell include=docs/src/MultiMethodExample.purs snippet=resources}
```

## Content Negotiation

By specifying alternative content types for a method, Hyper can choose
a response and content type based on the request `Accept` header. This
is called *content negotiation*. Instead of specifying a single type,
like `HTML` or `JSON`, we provide alternatives using `:<|>`. All content
types must have `MimeRender` instances for the response body type.

``` {.haskell include=docs/src/Site3.purs snippet=routing-type}
```

By making requests to this site, using `Accept` headers, we can see how
the router chooses the matching content type (output formatted and
shortened for readability).

``` {.bash}
$ <strong>curl -H 'Accept: application/json' http://localhost:3000/users</strong>
[
  {
    "name": "John Paul Jones",
    "id": "1"
  },
  {
    "name": "Tal Wilkenfeld",
    "id": "2"
  },
  ...
]
```

There is support for *wildcards* and *qualities* as well.

``` {.bash}
$ curl -H 'Accept: text/*;q=1.0' http://localhost:3000/users
<div>
  <h1>Users</h1>
  <ul>
    <li><a href="/users/1">John Paul Jones</a></li>
    <li><a href="/users/2">Tal Wilkenfeld</a></li>
    ...
  </ul>
</div>
```
