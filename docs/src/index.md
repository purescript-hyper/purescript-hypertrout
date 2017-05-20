---
title: Hypertrout
author: Oskar Wickström
---

## Purpose

The purpose of this package,
[Hypertrout](https://github.com/owickstrom/purescript-hypertrout),
is writing web servers using the *type-level routing API* in
[Trout](https://github.com/owickstrom/purescript-trout).
It provides a router middleware which, together with handler functions
and rendering functions, gives us a full-fledged server.

## A Single-Resource Example

Let's say we want to render a home page as HTML. We start out by
declaring the data type `Home`, and the structure of our site:

``` {.haskell language=purescript include=docs/src/Site1.purs snippet=routing-type}
```

`Resource (Get Home HTML)` is a routing type with only one resource,
responding to HTTP GET requests, rendering a `Home` value as HTML. So
where does the `Home` value come from? We provide it using a *handler*.
A handler for `Site1` would be some value of the following type:

``` {.haskell}
forall m. Monad m => ExceptT RoutingError m Home
```

We can construct such a value using `pure` and a `Home` value:

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

Real-world servers often need more than one resource. Let's define a
router for an application that shows a home page with links, a page
listing users, and a page rendering a specific user.

``` {.haskell include=docs/src/Site2.purs snippet=resources-and-type}
```

Let's go through the new constructs used:

-   `:<|>` is a type operator that separates *alternatives*. A router
    for this type will try each route in order until one matches.
-   `:/` separates a literal path segment and the rest of the routing
    type.
-   `Capture` takes a descriptive string and a type. It takes the next
    available path segment and tries to convert it to the given type.
    Each capture in a routing type corresponds to an argument in the
    handler function.
-   `:>` separates a routing type modifier, like `Capture`, and the rest
    of the routing type.

We define handlers for our resource methods as regular functions on the
specified data types, returning `ExceptT RoutingError m a` values, where
`m` is the monad of our middleware, and `a` is the type to render for
the resource.

``` {.haskell include=docs/src/Site2.purs snippet=handlers}
```

As in the single-resource example, we want to render as HTML. Let's
create instances for our data types. Notice how we can create links
between routes in a type-safe manner.

``` {.haskell include=docs/src/Site2.purs snippet=encoding}
```

The pattern match on the value returned by `linksTo` must match the
structure of the routing type. We use `:<|>` to pattern match on links.
Each matched link will have a type based on the corresponding resource.
`getUser` in the previous code has type `Int -> URI`, while `allUsers`
has no captures and thus has type `URI`.

We are still missing `getUsers`, our source of User values. In a real
application it would probably be a database query, but for this example
we simply hard-code some famous users of proper instruments.

``` {.haskell include=docs/src/Site2.purs snippet=get-users}
```

Almost done! We just need to create the router, and start a server.

``` {.haskell include=docs/src/Site2.purs snippet=main}
```

Notice how the composition of handler functions, using the value-level
operator `:<|>`, matches the structure of our routing type. If we fail
to match the type we get a compile error.

## Multi-Method Resources

So far we have just used a single method per resource, the `Get` method.
By replacing the single method type with a sequence of alternatives,
constructed with the type-level operator `:<|>`, we get a resource with
multiple methods.

``` {.haskell include=docs/src/MultiMethodExample.purs snippet=routing-type}
```

`MultiMethodExample` is a routing type with a *single resource*, which
has *multiple resource methods*. Handlers for the resource methods needs
to be separated by the value-level operator `:<|>`, just as with
handlers for different resources.

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
