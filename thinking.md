
# Experiment

NB `_auto:secrets` defined below as it will need to change for new scheme

Notes:

## inheritance
Not using inheritance or composition operator, but composes functions from
other types. Is this expicitness a better idea than the concept of hiding some
of the plumbing?

'type' is possibly wrong name for a collection of methods. but it does reflect
the ultimate end use case. package?

## messaging 

There's tension between the 'messaging' idea, which I'm clinging onto, and
having straightforward functions in the types. 

- Functions would give clearer errors, and also would be easier to check at
compile time. They also don't hide the plumbing, which appears to me as a major
source of confusion and circular thinking!

- Messaging relies on a receiver having that handler; if it's not there the behaviour
could be tricky to track down.

## further work

'types' become a collection of functions rather than message handlers, current
code handles these in a case statement so there needs to be an organisational
change ; simple idea - (define (type$funcname ....)) - it might be just as easy
to have an internal module definition that provides them.

No inheritance means that code has to be explicit - clear :)   This will greatly
simplify the semantics of handling types.

** Do we lose the initial goal of composing on top of types though? Some
supported override/import mechanism is inevitable? **

`apply` vs `data manipulation` is a separate concern.

### Base secret
```
type secret = {

    create(cfg, apply) = post-create(cfg, apply(pre-create(cfg)))

    pre-create(cfg)= _auto:secrets.create-api(cfg <- { parent = strf("projects/~a" cfg.project) }) <- { 
        api.url="https://secretmanager.googleapis.com/v1/{+parent}/secrets?secretId={secretId}"
    }
    post-create(original, resp) = resp <- { secretId=original.secretId, project=original.project }

    read(state, apply) = post-read(apply(pre-read(state)))
    pre-read(state) = state
    post-read(original, resp) = resp <- { name=strf("projects/~a/secrets/~a" original.project original.secretId) }

    update(cfg, apply) = post-update(cfg, apply(pre-update(cfg)))
    pre-update(cfg) = _auto:secrets.pre-create(cfg <- { parent = strf("projects/~a" cfg.project) })
    post-update(original, resp) = post-create(original, resp)

    delete(state, apply)= apply(_auto:secrets.delete(state))
}
```



### From _auto

```
type secrets = {
 origin(cfg)= cfg <- {
  driver="gcp"
 }
 create-api(cfg)={
  config=cfg
  api={
   api-id=API-ID
   request-body=schemas.Secret(cfg)
   response-type="secretmanager.schemas.Secret"
   method="POST"
   url="https://secretmanager.googleapis.com/v1/{+parent}/secrets"
   required=["(parent)"]
  }
 }
 read(cfg)={
  config=cfg
  api={
   api-id=API-ID
   response-type="secretmanager.schemas.Secret"
   method="GET"
   url="https://secretmanager.googleapis.com/v1/{+name}"
   required=["(name)"]
  }
 }
 update(cfg)={
  config=cfg
  api={
   api-id=API-ID
   request-body=schemas.Secret(cfg)
   response-type="secretmanager.schemas.Secret"
   method="PATCH"
   url="https://secretmanager.googleapis.com/v1/{+name}"
   required=["(name)"]
  }
 }
 delete(cfg)={
  config=cfg
  api={
   api-id=API-ID
   response-type="secretmanager.schemas.Empty"
   method="DELETE"
   url="https://secretmanager.googleapis.com/v1/{+name}"
   required=["(name)"]
  }
 }
}
export secrets
```

### Network example

- NB compute api is more directly usable

```
type _base = {
  post-resp(original, resp)=resp <- { name=original.name, region=original.region, project=original.project }
}

# This is boilerplate code for every type, can be auto-generated
# (could also be a use case for template/generic)
type network = {
    create(cfg, apply)=post-create(apply(pre-create cfg))
    pre-create(cfg) = _auto:networks.create-api(cfg)
    post-create=_base.post-resp

    read(cfg, apply)=post-read(apply(pre-read cfg))
    pre-read(cfg) = cfg
    post-read = _base.post-resp

    ...

    delete(state, apply)=apply(_auto:networks.delete(state))
}
```

### Extending the network type 

```
type my-network = {
    # my enhancements here...
    pre-create(cfg)=network.pre-create(cfg) <- { labels= ... }

    # this is boilerplate, for syntax/sugar to handle
    create=network.create
    post-create=network.post-create
    
    read=network.read
    pre-read=network.pre-read

    ...
}

# In syntax:
type my-network = network | {
    # my enhancements here...
    pre-create(cfg)=network.pre-create(cfg) <- { labels= ... }
}

# or:
type my-network(network) = {
    # my enhancements here...
    pre-create(cfg)=network.pre-create(cfg) <- { labels= ... }
}
```

### Generic/Template Labels extension

```
# type that can apply to any labelled type...
type labels = {
    pre-create(cfg) = cfg <- { labels ...}
    post-create(cfg) = ...
}

type policy = {
    pre-create(cfg) = cfg <- { policy ...}
    post-create(cfg) = cfg <- { do something to returned entity }
}

type my-network = {
    pre-create(cfg) = network.pre-create(labels.pre-create(policy.pre-create(cfg)))

    # Note the difference in order in which functions are composed...
    post-create(cfg) = policy.post-create(labels.post-create(network.post-create(cfg)))

    # alternative:
    pre-create = compose(network.pre-create, labels.pre-create, policy.pre-create)
    post-create = compose(policy.pre-create, labels.pre-create, network.pre-create)

    <other boilerplate as above...>
}

# In sugary syntax:

# Extend the type, use explicit calls...
type my-network(network) = {
    pre-create(cfg) = network.pre-create(labels.pre-create(policy.pre-create(cfg)))
    post-create(cfg) = policy.post-create(labels.post-create(network.post-create cfg))
}

# parameterised types...
type my-any<t, ls, ps> = {
    pre-create = compose(t.pre-create, ls.pre-create, ps.pre-create)
    post-create = compose-inv(t.post-create, ls.post-create, ps.post-create)

    # surface t's functions, but in lower precedence in the type
    t.*
}
type my-network = my-any<network, labels, policy>
```

NB we can have a library of basic re-usable types based on the above model:
```
type base1<T, O1> = { .. }
type base2<T, O1, O2> = { .. }
type base3<T, O1, O2, O3> = { .. }
type base3<T, O1, O2, O3....> = { .. }
```

```
# Type templates/generic types
type T[labels] = {
    pre-create(cfg) = T.pre-create(cfg <- { labels ...} )
    post-create(cfg) = T.post-create(cfg) <- { xyz }
}

type T[policy] = {
    pre-create(cfg) = T.pre-create(cfg <- { other policy } )
    post-create(cfg) = T.post-create(cfg) <- { policy stuff }
}

type my-network = network[labels]

type my-policy-network = network[labels, policy]
type my-policy-network = network[labels[policy]]

    T[policy] -> pre-create(cfg) = T[labels].pre-create(cfg <- { other policy } )
    T[policy] -> post-create(cfg) = T[labels].post-create(cfg) <- {policy stuff }>

    T[labels] -> pre-create(cfg) = network.pre-create(cfg <- { labels ...} )
    T[labels] -> post-create(cfg) = network.post-create <- {xyz}

# Hmm, feels more like inheritance than functional composition..

type my-policy-network = network | labels | policy

```

## Syntax-sugar ideas:

```
type my-network = {
    pre-create(cfg)=network.pre-create(cfg) <- { labels= ... }

    # This feels like it could be a language macro feature...
    |for m in create,post-create,read,pre-read etc ; m = network.m|
}

type my-network = network | {
    pre-create(cfg)=network.pre-create(cfg) <- { labels= ... }
}
```

## Networks API

```
type networks = {
 # TODO41 - destructors
 origin(cfg)= cfg <- {
  driver="gcp"
 }
 create-api(cfg)={
  config=cfg
  api={
   api-id=API-ID
   request-body=schemas.Network(cfg)
   response-type="compute.schemas.Operation"
   method="POST"
   url="https://compute.googleapis.com/compute/beta/projects/{project}/global/networks"
   required=["(project)"]
  }
 }
 read(cfg)={
  config=cfg
  api={
   api-id=API-ID
   response-type="compute.schemas.Network"
   method="GET"
   url="https://compute.googleapis.com/compute/beta/projects/{project}/global/networks/{name}"
   required=["(project network)"]
  }
 }
 update(cfg)={
  config=cfg
  api={
   api-id=API-ID
   request-body=schemas.Network(cfg)
   response-type="compute.schemas.Operation"
   method="PATCH"
   url="https://compute.googleapis.com/compute/beta/projects/{project}/global/networks/{name}"
   required=["(project network)"]
  }
 }
 delete(cfg)={
  config=cfg
  api={
   api-id=API-ID
   response-type="compute.schemas.Operation"
   method="DELETE"
   url="https://compute.googleapis.com/compute/beta/projects/{project}/global/networks/{name}"
   required=["(project network)"]
  }
 }
}
export networks
```