# Part 2

## Adding some more labelled resources 
Let's add some secrets to our module from Part 1; the complete file looks like this:

```
#lang marv

import types/gcp/storage as storage
import types/gcp/secretmanager as secret

type labelledBucket = {
    identity(cfg) = cfg <- { 
        labels = (cfg.labels | {} ) <- 
            { costcentre = "abc123", environment="uat" } 
    }
    * = storage:bucket.*
}

defaults = {
    project = env("MARV_GCP_PROJECT") 
    region = env("MARV_GCP_REGION") 
}

module main {

    bucket1 = labelledBucket defaults <- {
        name = imm: strf("~a-hello-world1" defaults.project)
    }

    bucket2 = labelledBucket defaults <- {
        name = imm: strf("~a-hello-world2" defaults.project)
    }
    
    secret1 = secret:secret defaults <- {
        name = "my-little-secret1"
        replication = { automatic = {} }
    }

    secret2 = secret:secret defaults <- {
        name = "my-little-secret2"
        replication = { automatic = {} }
    }

}
```

Let's now assume that we want to add the same labels to our new secrets. We can accomplish this easy enough by defining a type, same as we did for `bucket`:

```
...
type labelledSecret = {
    identity(cfg) = cfg <- { 
        labels = (cfg.labels | {} ) <- 
            { costcentre = "abc123", environment="uat" } 
        } 
    }
    * = secret:secret.*
}

    ...

    secret1 = labelledSecret defaults <- {
        name = "my-little-secret1"
        replication = { automatic = {} }
    }

    secret2 = labelledSecret defaults <- {
        name = "my-little-secret2"
        replication = { automatic = {} }
    }

```

This works, but once again look at the repetition - `labelledSecret` is virtually identical to `labelledBucket`. We can do a bit better than this!

## Type Templates

Type templates are an abstraction that allow us to factor-out the common parts of types, and fill-in-the-blanks later on to create a **concrete type** - this is a type that you can actually use to define a resource;  **you cannot directly use a type-template to define a resource** (yet).

A type-template declaration looks like this:

    type T<a,b..> = { .... }

and it's usage looks like:

    type N = T<type1, type2...>

It's pretty straightforward to see how this works by taking our labelled resources scenario as an example:

```
type LabelResource<R> = {
    identity(cfg) = R.identity(cfg) <- { 
        labels = (cfg.labels | {} ) <- { costcentre = "abc123", environment="uat" } 
    }
    * = R.*
}
type bucket = LabelResource<storage:bucket>
type secret = LabelResource<secret:secret>
```

`LabelResource` is our template, which accepts a parameter `R` (for Resource). The template defines `identity` which calls `R`'s `identity` function before adding in the labels.  It uses the wildcard `* = R.*` to declare the rest of the type's functions which are pulled from `R`.

Notice we've changed the name of the types to `bucket` and `secret` to make the code more succinct, so our complete module looks like this:

```
#lang marv

import types/gcp/storage as storage
import types/gcp/secretmanager as secret

type LabelResource<R> = {
    identity(cfg) = R.identity(cfg) <- { 
        labels = (cfg.labels | {} ) <- 
            { costcentre = "abc123", environment="uat" } 
    }
    * = R.*
}
type bucket = LabelResource<storage:bucket>
type secret = LabelResource<secret:secret>

defaults = {
    project = env("MARV_GCP_PROJECT") 
    region = env("MARV_GCP_REGION") 
}

module main {

    bucket1 = bucket defaults <- {
        name = imm: strf("~a-hello-world1" defaults.project)
    }

    bucket2 = bucket defaults <- {
        name = imm: strf("~a-hello-world2" defaults.project)
    }
    
    secret1 = secret defaults <- {
        name = "my-little-secret1"
        replication = { automatic = {} }
    }

    secret2 = secret defaults <- {
        name = "my-little-secret2"
        replication = { automatic = {} }
    }

}
```

NB: there is a subtle issue with the template - if `R` is a type which *already* defines `labels`, then these will be erased/replaced by the `identity` function in `LabelResource`.

## Taking Templates further...

We are still repeating ourselves with the use of `defaults <-`  - we can improve on this, albeit we're rapidly approaching the point of diminishing returns! However, it's worth pursuing a little further, if only to demonstrate more ways type-templates can be used.

So we're going to try and eliminate the need to use that `defaults` construct everywhere in our code. At first pass, it's tempting to just add it into `LabelResource`, and this might be fine for some cases. However, we have to do it in a way that's *a bit messy*, because the attributes need to be passed to `R`'s identity function:

```
type LabelResource<R> = {
    identity(cfg) = R.identity(cfg <- {project = defaults.project, region=defaults.region })  <- { 
        labels = (cfg.labels | {} ) <- { costcentre = "abc123", environment="uat" } 
    }
    * = R.*
}
```

NB there are various combinations of that idea.

A better approach is to separate everything into basic types, then combine them using a `Compose` type-template:

```
type Label = {
    identity(cfg) = cfg <- ((cfg.labels | {} ) <- 
            { costcentre = "abc123", environment="uat" } )
}

type Defaults = {
    identity(cfg) = cfg <- {
        project = defaults.project
        region = defaults.region
    }
}

type Compose<C1, C2, R> = {
    identity(cfg) = R.identity(C2.identity(C1.identity(cfg)))
    * = R.*
}

type bucket = Combine<Label, Defaults, storage:bucket>
type secret = Combine<Label, Defaults, secret:secret>

```

This is a better approach because the `Compose` type template is generic and re-usable. In fact, Marv's type library already has these defined for up to 5 combinations:


```
import types/marv/compositions
...
type bucket = C2<Label, Defaults, storage:bucket>
type secret = C2<Label, Defaults, secret:secret>
```